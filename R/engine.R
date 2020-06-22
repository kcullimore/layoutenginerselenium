
## CSS standard says 1px = 1/96in !?
dpi <- 96

dockerServer <- function(dir) {
    RSeleniumOptions <- getOption("layoutEngine.RSelenium")
    ## Debugging options
    debug <- RSeleniumOptions$debug
    ## Default browser options
    browser <- RSeleniumOptions$browser
    ## Default docker options
    docker  <- RSeleniumOptions$docker
    ## If docker container is running assign Container ID
    id <- system(paste0(
        "docker ps --filter 'name=", docker$name, "' --format '{{.ID}}'"),
        intern=TRUE)
    ## If docker container is NOT running then spin up
    if (length(id) > 0) {
        print("Docker container already running")
    } else {
        print("Spinning up Docker container...")
        ## Docker image to run
        image <- ifelse(is.null(docker$imageRequest),
                        paste0("selenium/standalone-", browser$type,
                               ifelse(browser$headless, ":latest", "-debug:latest")),
                        docker$imageRequest)
        print(paste0("Setting up RSelenium with a ", browser$type,
                     " browser running on Docker container built from ",
                     image, "."))
        ## Update docker image on host machine
        if (docker$freshPull) {
            update <- system(paste("docker pull", image), intern=TRUE)
        }
        ## Check OS to match system commands
        os <- Sys.info()["sysname"]
        if (os == "Linux") {
            ## Link docker to host display
            display_setup <- paste("--env DISPLAY=unix$DISPLAY",
                                   "--volume /dev/shm:/dev/shm",
                                   "--volume /tmp/.X11-unix:/tmp/.X11-unix")
            ## Setup to open docker contained browser within host machine
            output <- system(paste(
                paste("docker run -d --rm --name", docker$name),
                ifelse(browser$headless, "", display_setup),
                "--volume", paste0(dir, ":/tmp/Rsrc"),
                "-p ", paste0(browser$port, ":", browser$port), 
                image),
                intern=TRUE)
            id <- substr(tail(output, 1), 1, 12)
            print(paste("Docker container created with id:", id))
        } else {
            stop("Only the Linux OS is supported by RSeleniumEngine at this time.")
        }
    }
    list(id=id, name=docker$name, dir=dir, browser=browser$type,
         port=browser$port, url=browser$url,
         persist=browser$persist, headless=browser$headless,
         timeout=debug$timeout)
}

dockerClose <- function() {
    RSeleniumOptions <- getOption("layoutEngine.RSelenium")
    name <- RSeleniumOptions$docker$name
    dockerClosed <- system(paste("docker stop", name), intern=TRUE)
    print(paste0("Docker container '", dockerClosed, "' closed."))
}


RSeleniumLayout <- function(html, width, height, fonts, device) {
    ## Work in temp directory
    wd <- file.path(tempdir(), "layoutEngineRSelenium")
    if (!dir.exists(wd)) {
        result <- dir.create(wd, showWarnings=TRUE)
        if (!result) stop("Creation of working directory failed")
    }
    ## Create directory for assets
    assetDir <- file.path(wd, "assets")
    if (!dir.exists(assetDir)) {
        result <- dir.create(assetDir, showWarnings=TRUE)    
        if (!result) stop("Creation of working directory failed")
    }   
    ## Copy font files
    fontFiles <- fontFiles(fonts, device)
    file.copy(fontFiles, assetDir)
    ## Convert any .pfb/.pfa to .ttf
    pffiles <- grepl("[.]pf[ab]$", fontFiles)
    if (any(pffiles)) {
        fontforge <- Sys.which("fontforge")
        if (nchar(fontforge) == 0) stop("FontForge not available")
        for (i in fontFiles[pffiles]) {
            system(paste0(fontforge,
                          " -lang=ff -script ",
                          system.file("FF", "pf2ttf",
                                      package="layoutEngineRSelenium"),
                          " ", file.path(assetDir, basename(i))
                          ))
        }}
    ## Copy all assets to asset directory
    copyAssets(html, asssetDir)
    file.copy(system.file("JS", "font-baseline", "index.js",
                          package="layoutEngineRSelenium"),
              assetDir)
    file.copy(system.file("JS", "layout.js",
                          package="layoutEngineRSelenium"),
              assetDir)
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
    ## Activate server and open connection to remote driver
    server <- dockerServer(wd)
    if (server$headless){
        print("sleeping...")
        Sys.sleep(server$timeout / 2)
        print("waking...")
    } else {
        print("sleeping...")
        Sys.sleep(server$timeout)
        print("waking...")
    }
    ## Define RSelenium remote driver & open connection 
    remDr <- remoteDriver(remoteServerAddr=server$url,
                          port=server$port,
                          browserName=server$browser)
    remDr$open(silent=TRUE)
    ## session <- remDr$sessionInfo
    ## Navigate to a index.html file when browser is ready
    remDr$navigate("file:///tmp/Rsrc/index.html")
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
    ## Close browser and destroy docker container 
    if (server$headless | !server$persist) {
        remDr$close()
        dockerClosed <- system(paste("docker stop", server$name), intern=TRUE)
    print(paste0("Docker container '", dockerClosed, "' closed."))
    }
    ## Build data.frame with layout data
    layoutDF <- read.csv(textConnection(layoutCSV),
                         header=FALSE, stringsAsFactors=FALSE,
                         quote="'\"")
    names(layoutDF) <- names(layoutFields)
    ## Convert font size from CSS pixels to points
    layoutDF$size <- layoutDF$size * 72 / dpi
    do.call(makeLayout, layoutDF)
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
