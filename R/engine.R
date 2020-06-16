
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

    ## Setup to open docker browser within host machine (firefox)
    dockerID <- "rselenium-container"
    system(paste(paste("docker run -d --rm --name", dockerID),
                 "--env DISPLAY=unix$DISPLAY",
                 "--volume /dev/shm:/dev/shm",
                 "--volume /tmp/.X11-unix:/tmp/.X11-unix",
                 "--volume", paste0(wd, ":/tmp/Rsrc"),
                 "-p 4444:4444",
                 "selenium/standalone-firefox-debug:latest"))

    ## Connect to server
    remDr <- remoteDriver(remoteServerAddr = "localhost",
                          port = 4444L,
                          browserName = "firefox")
  
    ## browser()
    Sys.sleep(2)
    ## Open RSelenium hosted browser 
    remDr$open(silent = TRUE)

    ## Navigate to a index.html file
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
