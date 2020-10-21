

rSSSessionOpen <- function(rSS) {
    browser()
    rSSS_ready <- FALSE    
    rSS_ready <- rSS$getStatus()$ready
    if (!rSS_ready) {
        rSS$open()
    }
    browser_session <- rSS$remDr$getSessions()
    browser_count <- length(browser_session)
    if (browser_count == 0) {
        rSS$remDr$open(silent=TRUE)
        browser_session <- rSS$remDr$getSessions()
        rSSS_ready <- TRUE        
        message("RSelenium browser session has been created.")
    } else {
        rSSS_ready <- TRUE
    }
    rSSS <- list(ready=rSSS_ready,
                 remDr=rSS$remDr,
                 dir=rSS$dir,
                 close=rSS$container$close,
                 session=browser_session)
    options(layoutEngine.rSSSession=rSSS)
    rSSS
}

rSSSessionClose <- function(rSS) {
    if (!rSS$getStatus()$ready) {
        message("RSelenium browser session is not active so cannot be closed.")
    } else {
        rSS$container$close() 
    }
}

rSSSession <- function(url="localhost", port=4444L,
                       network="bridge", shm_size="1g",
                       browser_type="firefox", headless=FALSE,
                       image_request=NULL, fresh_pull=FALSE) {

    settings <- list(url=url, port=port, network=network, shm_size=shm_size,
                     browser_type=browser_type, headless=headless,
                     image_request=image_request, fresh_pull=fresh_pull)

    ## RSelenium server setup
    rSS <- rSServer(settings)

    ## RSelenium browser session setup 
    rSSS <- rSSSessionOpen(rSS)
    
    open <- function() {
        rSSSessionOpen(rSS)
    }

    close <- function() {
        rSSSessionClose(rSSS)
    }
    c(rSSS, open=open, close=close)
}
