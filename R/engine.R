
## CSS standard says 1px = 1/96in !?
dpi <- 96

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
                          " ", file.path(assetDir, basename(i))))
        }}


    ## Copy any assets to asset directory
    copyAssets(html, asssetDir)
    file.copy(system.file("JS", "font-baseline", "index.js",
                          package="layoutEngineRSelenium"),
              assetDir)
    file.copy(system.file("JS", "layout.js",
                          package="layoutEngineRSelenium"),
              assetDir)
    ## Add script tags to layout.js file
    body <- xml_find_first(html$doc, "body")
    xml_add_child(body, "script", src="assets/index.js")
    xml_add_child(body, "script", src="assets/layout.js")

    ## Create index.html flie to navigate to with RSelenium 
    fileConn <- file(paste0(wd, "/index.html"), "w")
    write(paste0(html$doc, collapse=""), file=fileConn, append=FALSE)
    close(fileConn)

    
    ## Establish R http server root
    oldwd <- getwd()
    if (!is.null(oldwd)) 
        on.exit(setwd(oldwd))
    setwd(wd)

    browser()
    
    ## Directory location to serve local files
    ##srcDir <- paste0(oldwd, "/src")
    ##jsDir <- system.file("JS", package="layoutEngineRSelenium")

    ## Close docker container if open
    ##system("docker stop RSeleniumContainer")

    ## Setup to open docker browser within host machine (firefox)
    system(paste("docker run -d --rm --name RSeleniumContainer",
                 "--env DISPLAY=unix$DISPLAY",
                 "--volume /dev/shm:/dev/shm",
                 "--volume /tmp/.X11-unix:/tmp/.X11-unix",
            ##     "--volume", paste0(srcDir, ":/src"),
            ##     "--volume", paste0(jsDir, ":/JS"),
                 "--volume", paste0(wd, ":/tmp/Rsrc"),
                 "-p 4444:4444",
                 "selenium/standalone-firefox-debug:latest"))

    ## Connect to server
    remDr <- remoteDriver(remoteServerAddr = "localhost",
                          port = 4444L,
                          browserName = "firefox")

    ## Send request to server to initialize session (give it a time lime to execute)
    ## session <- NULL
    ## now <- Sys.time()
    ## while (is.null(session$id) &&
    ##        Sys.time() - now < 5) {
    ##            session <- remDr$open(silent = TRUE)               
    ## }
    ## if (session$id == NULL) {
    ##     stop("RSelenium failed to open session")
    ## }
    
    ## Open RSelenium hosted browser and navigate to a index.html file
    remDr$open(silent = TRUE)
    remDr$navigate("file:///tmp/Rsrc/index.html")
    
    ## Set the page <body> size to match R graphics device and
    ## add and execute function call from layout.js to calculate the page layout
    remDr$executeScript(
              script="
    document.body.style.width = arguments[0];
    document.body.style.height = arguments[1];
    const script = document.createElement('script');
    script.innerHTML = 'calculateLayout()';
    document.body.appendChild(script);
    ",
    args=list(as.character(width*dpi), as.character(height*dpi))
    )
    
    ## Get the layout info back to pass back to R
    layoutCSV <- remDr$findElement(
                           "id", "layoutEngineRSeleniumresult"
                       )$getElementAttribute("innerHTML")[[1]]
    
    ##closePage(page)
    ##remDr$close() 
    
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
