.onLoad <- function(libname, pkgname) {
    options(layoutEngine.backend=RSeleniumEngine,
            layoutEngine.RSelenium.debug=list(timeout=2),
            layoutEngine.RSelenium.browser=list(open=FALSE,
                                                type="firefox",
                                                port=4444L,
                                                url="localhost",
                                                headless=FALSE),
            layoutEngine.RSelenium.docker=list(running=FALSE,
                                               name="rselenium-container",
                                               fresh_pull=FALSE,
                                               image_tag=NULL)
            )
}
