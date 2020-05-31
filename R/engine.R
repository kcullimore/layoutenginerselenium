
## CSS standard says 1px = 1/96in !?
dpi <- 96

RSeleniumLayout <- function(html, width, height, fonts, device) {
    ## Work in temp directory
    wd <- file.path(tempdir(), "layoutEngineRSelenium")
    if (!dir.exists(wd)) {
        result <- dir.create(wd, showWarnings=TRUE)
        if (!result) stop("Creation of working directory failed")
    }
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
        }
    }    


    ## Copy any assets
    copyAssets(html, assetDir)
    ## Append layout calculation code
    file.copy(system.file("JS", "font-baseline", "index.js",
                          package="layoutEngineRSelenium"),
              assetDir)
    file.copy(system.file("JS", "layout.js",
                          package="layoutEngineRSelenium"),
              assetDir)
    body <- xml_find_first(html$doc, "body")
    ## xml_add_child(body, "script", src="/assets/index.js")
    ## xml_add_child(body, "script", src="/assets/layout.js")

    HTML <- as.character(xml_children(body))
    head <- xml_find_all(html$doc, "head/*")
    ## Establish R http server root
    oldwd <- getwd()
    if (!is.null(oldwd)) 
        on.exit(setwd(oldwd))
    setwd(wd)

    browser()
    
    ## Directory location to serve local files
    srcDir <- paste0(oldwd, "/src")
    jsDir <- system.file("JS", package="layoutEngineRSelenium")

    ## Setup to open docker browser within host machine (firefox)
    system(paste("docker run -d --rm",
                 "--env DISPLAY=unix$DISPLAY",
                 "--volume /dev/shm:/dev/shm",
                 "--volume /tmp/.X11-unix:/tmp/.X11-unix",
                 "--volume", paste0(srcDir, ":/src"),
                 "--volume", paste0(jsDir, ":/JS"),
                 "--volume", paste0(assetDir, ":/assets"),
                 "-p 4444:4444",
                 "selenium/standalone-firefox-debug:latest"))

    

    ## Connect to server
    remDr <- remoteDriver(remoteServerAddr = "localhost",
                          port = 4444L,
                          browserName = "firefox")

    ## Send request to server to initialize session
    remDr$open(silent = TRUE)
    
    ## Naigate to a template file
    remDr$navigate("file:///src/basic.html")
    
    ## Append head elements to head element
    remDr$executeScript(
              script="
    const headIn = arguments[0];
    document.head.innerHTML += headIn;
    ",
    args=list(paste(head, collapse=""))
    )
    
    ## Replace body element with HTML
    remDr$executeScript(
              script="
    const newHTML = arguments[0];
    document.body.innerHTML = newHTML;
    ",
    args=list(paste(HTML, collapse=""))
    )
    
    ## Set the page <body> size
    remDr$executeScript(
              script="
    const newWidth = arguments[0];
    const newHeight = arguments[1];
    const body = document.body;
    body.style.width = newWidth;
    body.style.height = newHeight;
    ",
    args=list(as.character(width*dpi), as.character(height*dpi))
    )

    ## Add script to calculate the page layout
    remDr$executeScript(
              script="
     const script_1 = document.createElement('script');
     script_1.src = '/assets/index.js';
     document.body.appendChild(script_1);
     const script_2 = document.createElement('script');
     script_2.src = '/assets/layout.js';
     document.body.appendChild(script_2);
     "
     )

    ## Add script to calculate the page layout
    remDr$executeScript(
              script="
    const script = document.createElement('script');
    script.innerHTML = 'calculateLayout()';
    document.body.appendChild(script);
    "
    )

    ## Temp: Make text visible to use RSelenium - getElementText() method
    remDr$executeScript(
              script="
    document.getElementById('layoutEngineRSeleniumresult').style.display = 'inline';
    "
    )

    ## Get the layout info back
    layoutDIV <- remDr$findElement("id", "layoutEngineRSeleniumresult")
    layoutCSV <- layoutDIV$getElementText()[[1]]

    
    ##closePage(page)
    remDr$close() 
    
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
