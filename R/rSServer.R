
rSServerStatus <- function(rSS) {
    container_info <- rSS$container$getInfo()
    if (!container_info$running) {
        ready <- FALSE
    } else {
        ready <- rSS$remDr$getStatus()$ready
    }
    list(ready=ready)
}

rSServerOpen <- function(settings) {
    ## Docker container setup and status variables
    container <- dockerContainer(settings)
    container$run()
    container_info <- container$getInfo()

    ## RSelenium remote driver
    if (settings$browser_type == "firefox") {
        remDr <- remoteDriver(remoteServerAddr=settings$url,
                              port=settings$port,
                              browserName=settings$browser_type,
                              extraCapabilities = list(marionette=TRUE))
    } else {
        remDr <- remoteDriver(remoteServerAddr=settings$url,
                              port=settings$port,
                              browserName=settings$browser_type)
    }     

    list(remDr=remDr, container=container, dir=container_info$dir)
}

rSServerClose <- function(rSS) {
    container_info <- rSS$container$getInfo()
    name <- rSS$container$name
    if (!container_info$running) {
        message("RSelenium server is not running.")
    } else {
        rSS$container$close()
        message(paste0("RSelenium server running in docker container '",
                       name, "' has been closed."))
    }
}

rSServer <- function(settings) {

    rSS <- rSServerOpen(settings)
    getStatus <- function() {
        rSServerStatus(rSS)
    }

    open <- function() {
        rSServerOpen(settings)
    }

    close <- function() {
        rSServerClose(rSS)
    }

    ## rSServer object
    c(rSS, getStatus=getStatus, open=open, close=close)
}
