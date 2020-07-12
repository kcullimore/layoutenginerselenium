

## Method to get status of the RSelenium session    
sessionStatus <- function(container, remDr, browser) {
    ## Get docker container status
    container_status <- container$getStatus()
    if (!container_status$running) {
        remDr_status <- list(ready=FALSE)
        browser_status <- list(ready=FALSE)
    } else {
        remDr_status <- remDr$getStatus()
        if (remDr_status$ready) {
            session <- remDr$getSessions()
            if (length(session) > 0 ) {
                browser_status <- list(ready=TRUE)
            } else {
                browser_status <- list(ready=FALSE)
            }
        }
    }
    list(container=container_status,
         remDr=remDr_status,
         browser=browser_status)
}

sessionOpen <- function(container, remDr, browser) {
    session_status <- sessionStatus(container, remDr, browser)
    browser_session <- list()
    if (!session_status$container$running) {
        stop("Docker container is not running")
    } else {
        
        if (!session_status$remDr$ready) {
            stop("RSelenium remote driver not ready.")
        } else {
            if (session_status$browser$ready) {
                browser_session <- remDr$getSessions()
                message("Browser is already open")
            } else {
                remDr$open(silent=TRUE)
                Sys.sleep(2)
                browser_session <- remDr$getSessions()
                message(paste0("RSelenium session running in docker container '",
                               session_status$container$name,
                               "' and accessible at: ",
                               session_status$container$page))
            }
        }
    }
    session <- list(remDr=remDr,
                    browser=browser_session,
                    container=container)
    options(layoutEngine.RSelenium.dockerSession=list(
                ready=TRUE,
                session=session))
    session
}


sessionClose <- function(container, remDr, browser) {
        session_status <- sessionStatus(container, remDr, browser)
        if (session_status$remDr$ready) {
            remDr$close()
            container$close()
            message(paste0("RSelenium session running in docker container '",
                           container$settings$name,
                           "' has been closed and the container removed"))
            options(layoutEngine.RSelenium.dockerSession=list(
                        ready=FALSE,
                        session=list()))
        } else {
            warning("RSelenium session is not running.")
        }
}


## Primary function to create & run Selenium within Docker container
## Includes methods to start RSelenium browser and
## destroy the running container and hosted RSelenium browser
dockerSession <- function(url="localhost", port=4444L, network=NULL,
                          browser_type="firefox", headless=FALSE,
                          image_request=NULL, fresh_pull=FALSE) {

    ## Create container object
    container <- dockerContainer(dir, url, port, network, browser_type,
                                 headless, image_request, fresh_pull)
    id <- container$run()
    ## Get status of container
    ## container_status <- container$getStatus()
    ## if (!container_status$running) {
    ##     id <- container$run()
    ## } else {
    ##     id <- container_status$id
    ## }
    ## Create RSelenium remote driver object from inputs
    remDr <- remoteDriver(remoteServerAddr=url,
                          port=port,
                          browserName=browser_type)    
    getStatus <- function() {
        sessionStatus(container, remDr, browser)
    }
    ## Method to start RSelenium session
    open <- function() {
        sessionOpen(container, remDr, browser)
    }   
    ## Method to close RSelenium session
    close <- function() {
        sessionClose(container, remDr, browser)
    }
    list(remDr=remDr, container=container, getStatus=getStatus, open=open, close=close)
}

