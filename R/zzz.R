.onLoad <- function(libname, pkgname) {
    options(layoutEngine.backend=RSeleniumEngine,
            layoutEngine.RSelenium=list(
                debug=list(timeout=2),
                browser=list(type="firefox", port=4444L,
                             url="localhost", persist=TRUE, headless=FALSE),
                docker=list(name="rselenium-container",
                            freshPull=FALSE, imageRequest=NULL))
            )
}
