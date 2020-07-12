
.onLoad <- function (libname, pkgname) {
    system2("docker", args=c("stop", "rselenium-container"),
            stdout=FALSE, stderr=FALSE, wait=FALSE)
    options(layoutEngine.backend=RSeleniumEngine,
            layoutEngine.rSSSession=NULL)
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Welcome to the RSelenium backend for the layout engine.")
}
