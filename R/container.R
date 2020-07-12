createTmpDir <- function() {
    ## Work in temp directory
    wd <- file.path(tempdir(), "layoutEngineRSelenium")
    if (!dir.exists(wd)) {
        result <- dir.create(wd, showWarnings=TRUE)
        if (!result) stop("Creation of working directory failed")
    }
    ## Create directory for assets
    asset_dir <- file.path(wd, "assets")
    if (!dir.exists(asset_dir)) {
        result <- dir.create(asset_dir, showWarnings=TRUE)    
        if (!result) stop("Creation of working directory failed")
    }
    wd
}

dockerImage <- function(browser_type, headless, image_request) {
    if (is.null(image_request)) {
        if (headless) {
            paste0("selenium/standalone-", browser_type, ":latest")
        } else {
            paste0("selenium/standalone-", browser_type, "-debug:latest")
        }
    } else {
        image_request
    }
}

displaySetup <- function(headless) {
    if (headless) {
        display_setup <- ""
    } else {
        ## Check OS to match system commands
        os <- Sys.info()["sysname"]
        if (os == "Linux") {
            ## Link docker to host display
            display_setup <- c("--env",  "DISPLAY=unix$DISPLAY",
                                   "--volume",  "/dev/shm:/dev/shm",
                                   "--volume", "/tmp/.X11-unix:/tmp/.X11-unix")
        } else if (os == "Darwin") {
            ## Get IP info
            HOME <- system2("echo", args=c("$HOME"),
                            stdout=TRUE, stderr=FALSE)
            IP <- system2("ipconfig", args=c("getifaddr", "en0"),
                                             stdout=TRUE, stderr=FALSE)
            ## Link docker to host display
            ## NOTE: Xquartz X11 Preferences/Security set "Allow connections from network clients"
            display_setup <- paste(paste0("--env DISPLAY=", IP, ":0"),
                                   "--env XAUTHORITY=/.Xauthority",
                                   "--volume /tmp/.X11-unix:/tmp/.X11-unix",
                                   paste0("--volume ", HOME, "/.Xauthority:/.Xauthority"))
            ##display_setup <- "--env DISPLAY=docker.for.mac.host.internal:0"
        } else if (os == "Windows") {
            ## Get IP info
            HOME <- system2("echo", args=c("$HOME"),
                            stdout=TRUE, stderr=FALSE)
            IP <- system2("ipconfig", args=c("getifaddr", "en0"),
                                             stdout=TRUE, stderr=FALSE)
            ## Link docker to host display
            ## NOTE: Xquartz X11 Preferences/Security set "Allow connections from network clients"
            display_setup <- c("--env", paste0("DISPLAY=", IP, ":0"),
                                   "--env", "XAUTHORITY=/.Xauthority",
                                   "--volume", "/tmp/.X11-unix:/tmp/.X11-unix",
                                   "--volume", paste0(HOME, "/.Xauthority:/.Xauthority"))
        } else {
            stop(paste("The host machine OS:", os,
                       "is not supported by RSeleniumEngine at this time."))
        }
    }
}

containerInfo <- function(name) {
    id <- system2("docker",
                  args=c("ps",  "--filter", paste0("'name=", name, "'"),
                         "--format", "'{{.ID}}'"),
                  stdout=TRUE, stderr=FALSE)
    running <- length(id) > 0
    if (running) {
        image <- system2("docker",
                         args=c("ps",  "--filter", paste0("'name=", name, "'"),
                                "--format", "'{{.Image}}'"),
                         stdout=TRUE, stderr=FALSE)
        mounts <- system2("docker",
                          args=c("inspect",  "-f", "'{{.Mounts}}'", id),
                          stdout=TRUE, stderr=FALSE)
        beg_string <- regexpr('(?<=bind  ).*', mounts, perl=TRUE)        
        end_string <- regexpr('.+?(?=/layout)', mounts, perl=TRUE)
        tmp_dir <- substr(mounts,
                          beg_string[1],
                          end_string[1] + attr(end_string, "match.length")[1])
        dir <- paste0(tmp_dir, "layoutEngineRSelenium")
    } else {
        id <- image <- dir <- NULL
    }
    list(id=id, running=running, image=image, dir=dir)
}

containerRun <- function(name, settings) {
    ## Get info of container if running
    info <- containerInfo(name)
    ## Determine if the container image has changed
    image <- dockerImage(settings$browser_type,
                         settings$headless,
                         settings$image_request)
    same_image <- identical(info$image, image)
    ## Container build/rebuild logic
    if (info$running && same_image && !settings$fresh_pull) {
        message(paste0("Docker container '", name,
                       "' is already running."))
    } else {
        if (info$running) containerClose(name)
        if (settings$fresh_pull) {
            system2("docker", args=c("pull", image))
            message(paste0("Docker container '", name,
                           "' is being rebuilt with freshly pulled image ",
                           image))
        }
        ## Generate input display setup
        display_setup <- displaySetup(settings$headless)
        ## Create tmp directory for docker instance
        dir <- createTmpDir()
        run_args <- c("run",  "-d",  "--rm ", "--name", name,
                      "--volume", paste0(dir, "/tmp/src"),
                      "--network", settings$network,
                      paste0("--shm-size=", settings$shm_size),
                      display_setup,
                      "-p ", paste0(settings$port, ":", settings$port),
                      image)
        system2("docker", run_args, stdout=TRUE, stderr=FALSE)
        Sys.sleep(1.8)
        info <- containerInfo(name)
        message(paste0("Docker container created with name=",
                       name, " and id=", info$id))
    }
}

containerClose <- function(name="rselenium-container") {
    info <- containerInfo(name)
    if (info$running) {
        closed <- system2("docker", args=c("stop", name), stdout=TRUE, stderr=FALSE)
        message(paste0("Docker container '", closed, "' stopped and removed."))
    } else {
        message(paste0("Docker container '", name, "' is not running."))
    }
}

dockerContainer <- function(settings) {
    ## Container name
    name <- "rselenium-container"

    getInfo <- function() {
        containerInfo(name)
    }

    run <- function () {
        containerRun(name, settings)
    }

    close <- function () {
        containerClose(name)      
    }

    list(name=name, getInfo=getInfo, run=run, close=close)
}
