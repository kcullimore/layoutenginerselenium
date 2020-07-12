
## CSS standard says 1px = 1/96in !?
dpi <- 96

## Primary function to generate layout within the RSelenium hosted
## browser and return to R
RSeleniumLayout <- function(html, width, height, fonts, device) {
    ## Initiate a live browser session
    global_rSSS <- getOption("layoutEngine.rSSSession")
    if (is.null(global_rSSS)) {
        rSSS <- rSSSession()
    } else {
        rSSS <- global_rSSS
    }
    ## Define remote driver pointer
    remDr <- rSSS$remDr
    ## Define working directory
    wd <- rSSS$dir
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
            system2(fontforge,
                    args=c("-quiet", "-lang=ff", "-script",
                           system.file("FF", "pf2ttf",
                                       package="layoutEngineRSelenium"),
                           " ", file.path(asset_dir, basename(i))
                           ), stderr=FALSE)
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
    ## Navigate to index file
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

RSeleniumfontFile <- function(file) {
    ## Strictly, @font-face spec does not allow .pfb/.pfa
    ## Replace .pfb/.pfa with .ttf
    ## (the conversion of the actual font happens in the layout function)
    gsub("[.]pf[ab]$", ".ttf", file)
}

RSeleniumEngine <- makeEngine(RSeleniumLayout,
                              cssTransform=list(
                                  fontFile=RSeleniumfontFile))
