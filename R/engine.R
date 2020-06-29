
## CSS standard says 1px = 1/96in !?
dpi <- 96

createTmpDir <- function() {
    ## Work in temp directory
    wd <- file.path(tempdir(), "layoutEngineRSelenium")
    if (!dir.exists(wd)) {
        result <- dir.create(wd, showWarnings=TRUE)
        if (!result) stop("Creation of working directory failed")
    }
    ## Create directory for assets
    asset_dir <- file.path(wd, "assets")
    if (!dir.exists(asset_dir)) {
        result <- dir.create(asset_dir, showWarnings=TRUE)    
        if (!result) stop("Creation of working directory failed")
    }
    wd
}

imageTag <- function(browser_type, headless, image_tag_request) {
    if (is.null(image_tag_request)) {
        paste0("selenium/standalone-", browser_type,
               ifelse(headless, ":latest", "-debug:latest"))
    } else {
        image_tag_request
    }
}

dockerRunCmd <- function(dir, image_tag, headless, port) {
    ## Check OS to match system commands
    os <- Sys.info()["sysname"]
    if (os == "Linux") {
        ## Container name
        name <- "rselenium-container-linux"
        ## Link docker to host display
        display_setup <- paste("--env DISPLAY=unix$DISPLAY",
                               "--volume /dev/shm:/dev/shm",
                               "--volume /tmp/.X11-unix:/tmp/.X11-unix")
    } else if (os == "Darwin") {
        ## Container name
        name <- "rselenium-container-mac"
        ## Get IP info
        HOME <- system("echo $HOME", intern=TRUE)
        IP <- system("ipconfig getifaddr en0", intern=TRUE)
        ## Link docker to host display
        ## NOTE: Xquartz X11 Preferences/Security set "Allow connections from network clients"
        display_setup <- paste(paste0("--env DISPLAY=", IP, ":0"),
                               "--env XAUTHORITY=/.Xauthority",
                               "--volume /tmp/.X11-unix:/tmp/.X11-unix",
                               paste0("--volume ", HOME, "/.Xauthority:/.Xauthority"))
    } else {
        stop("Only Linux OS & MacOS is supported by RSeleniumEngine at this time.")
    }
    list(name=name,
         cmd=paste("docker run -d --rm --name", name,
                   "--shm-size=2g",
                   ifelse(headless, "", display_setup),
                   "--volume", paste0(dir, ":/tmp/src"),
                   "-p ", paste0(port, ":", port),
                   image_tag)
         )
}
## Primary function to create & run Selenium within Docker container
## Includes methods to start RSelenium browser and
## destroy the running container and hosted RSelenium browser
dockerRun <- function(url="localhost", port=4444L,
                      browser_type="firefox", headless=FALSE,
                      image_tag_request=NULL, fresh_pull=FALSE) {
    ## Create tmp directory for docker instance
    dir <- createTmpDir()
    ## Generate variables to run checks and create container
    image_tag <- imageTag(browser_type, headless, image_tag_request)
    docker_run_cmd <- dockerRunCmd(dir, image_tag, headless, port)
    name <- docker_run_cmd$name
    ## Check if container is running and if the docker image has changed
    container_status <- getContainerStatus(name)
    same_tag <- identical(image_tag, container_status$image_tag)
    ## Method to close container down
    close <- function () {
        container_status <- getContainerStatus(name)
        if (container_status$running) {
            closed <- system(paste("docker stop", name), intern=TRUE)
            options(layoutEngine.RSelenium.server=list())
            print(paste0("Docker container '", closed, "' stopped."))
        } else {
            print(paste0("Docker container '", name, "' is not running."))
        }
    }    
    ## Method to start RSelenium session
    startServer <- function() {
        container_status <- getContainerStatus(name)
        if (container_status$running) {
            remDr <- remoteDriver(remoteServerAddr=url,
                                  port=port,
                                  browserName=browser_type)
            print(paste0("Selenium server running in docker container '",
                         name, "' and accessible at http://",
                         url, ":", port))
            server <- list(remDr=remDr, container=list(name=name, dir=dir))
            options(layoutEngine.RSelenium.server=server)
        } else {
            print(paste0("Docker container '", container$name,
                         "' is not running. Please setup before",
                         " requesting server start."))
        }
    }
    ## Either return details of running container or start new container
    if (container_status$running & same_tag & !fresh_pull) {
        print(paste0("Docker container '", name,
                     "' is already running."))
        id <- container_status$id
    } else {
        if (container_status$running) {
            close()
            print(paste0("Docker container '", name,
                         "' is being rebuilt with updated image ",
                         image_tag))
        }
        if (fresh_pull) system(paste("docker pull", image_tag))
        output <- system(docker_run_cmd$cmd, intern=TRUE)
        id <- substr(tail(output, 1), 1, 12)
        print(paste0("Docker container created with name=", name, " and id=", id))
    }
    ## Output relevant container information and methods
    list(id=id, name=name, dir=dir, url=url, port=port,
         browser_type=browser_type, headless=headless,
         startServer=startServer, close=close)
}

## Function to check if container is running
getContainerStatus <- function(name) {
    id <- system(paste0("docker ps --filter 'name=", name,
                        "' --format '{{.ID}}'"), intern=TRUE)
    image_tag <- system(paste0("docker ps --filter 'name=", name,
                               "' --format '{{.Image}}'"), intern=TRUE)
    ifelse(length(id) > 0, running <- TRUE, running <- FALSE)
    list(id=id, image_tag=image_tag, running=running)
}

## Function to open RSelenium session 
openSession <- function(remDr) {
    remDr_status <- remDr$getStatus()
    if(remDr_status$ready){
        sessions <- remDr$getSessions()
        number_of_sessions <- length(sessions)
        if (number_of_sessions == 0) remDr$open(silent=TRUE)
        remDr$getSessions()
    } else {
        print("Remote Driver not ready.")
    }
}

## Primary function to generate layout within the RSelenium hosted
## browser and return to R
RSeleniumLayout <- function(html, width, height, fonts, device) {
    server <- getOption("layoutEngine.RSelenium.server")
    server_not_active <- ifelse(length(server) == 0, TRUE, FALSE)
    if (server_not_active) {
        print("RSelenium server is not active.")
    } else {
        remDr <- server$remDr
        wd <- server$container$dir
        asset_dir <- file.path(wd, "assets")
        ## Copy font files
        fontFiles <- fontFiles(fonts, device)
        file.copy(fontFiles, asset_dir)
        ## Convert any .pfb/.pfa to .ttf
        pffiles <- grepl("[.]pf[ab]$", fontFiles)
        if (any(pffiles)) {
            fontforge <- Sys.which("fontforge")
            if (nchar(fontforge) == 0) stop("FontForge not available")
            for (i in fontFiles[pffiles]) {
                system(paste0(fontforge,
                              " -quiet -lang=ff -script ",
                              system.file("FF", "pf2ttf",
                                          package="layoutEngineRSelenium"),
                              " ", file.path(asset_dir, basename(i))
                              ))
            }}
        ## Copy all assets to asset directory
        copyAssets(html, asset_dir)
        file.copy(system.file("JS", "font-baseline", "index.js",
                              package="layoutEngineRSelenium"),
                  asset_dir)
        file.copy(system.file("JS", "layout.js",
                              package="layoutEngineRSelenium"),
                  asset_dir)
        ## Add script tags to font-baseline/index.js &  layout.js file
        body <- xml_find_first(html$doc, "body")
        xml_add_child(body, "script", src="assets/index.js")
        xml_add_child(body, "script", src="assets/layout.js")
        ## Set width & height of body to match arguments
        xml_set_attr(body,
                     "style",
                     paste0("width: ", as.character(width*dpi), "px; ",
                            "height: ", as.character(height*dpi), "px;"))   
        ## Create index.html flie to navigate to with RSelenium 
        fileConn <- file(paste0(wd, "/index.html"), "w")
        write(paste0(html$doc, collapse=""), file=fileConn, append=FALSE)
        close(fileConn)
        ## Open RSelenium Session and navigate to index file
        session <- openSession(remDr)
        remDr$navigate("file:///tmp/src/index.html")
        ## Set the page <body> size to match R graphics device and
        ## add and execute function call from layout.js to calculate the page layout
        remDr$executeScript(
                  script="
    const script = document.createElement('script');
    script.innerHTML = 'calculateLayout()';
    document.body.appendChild(script);
    ")   
        ## Get the layout info back to pass back to R
        layoutCSV <- remDr$findElement(
                               "id", "layoutEngineRSeleniumresult"
                           )$getElementAttribute("innerHTML")[[1]]
        ## Build data.frame with layout data
        layoutDF <- read.csv(textConnection(layoutCSV),
                             header=FALSE, stringsAsFactors=FALSE,
                             quote="'\"")
        names(layoutDF) <- names(layoutFields)
        ## Convert font size from CSS pixels to points
        layoutDF$size <- layoutDF$size * 72 / dpi
        do.call(makeLayout, layoutDF)
    }
}

RSeleniumfontFile <- function(file) {
    ## Strictly, @font-face spec does not allow .pfb/.pfa
    ## Replace .pfb/.pfa with .ttf
    ## (the conversion of the actual font happens in the layout function)
    gsub("[.]pf[ab]$", ".ttf", file)
}

RSeleniumEngine <- makeEngine(RSeleniumLayout,
                              cssTransform=list(
                                  fontFile=RSeleniumfontFile))
