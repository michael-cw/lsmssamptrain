#' Start the LSMS Sampling Trainer Application
#'
#' @description Launches the interactive 'Shiny' application for household
#' survey sampling training based on the World Bank LSMS Sampling Manual.
#'
#' @details This function initializes and launches the LSMS Sampling Trainer
#' application in the user's default web browser.
#'
#' @param launch.browser Logical; if `TRUE` (the default), the application opens
#'   in the system's default web browser.
#'
#' @return Invisible `NULL`, called for its side effect of launching the 'Shiny'
#'   application.
#'
#' @export
runSampleTrainer <- function(launch.browser = TRUE) {
    if (is.null(getOption("sp_startup_message"))) {
        options(sp_startup_message = "none")
    }
    if (Sys.getenv("_SP_STARTUP_MESSAGE_") == "") {
        Sys.setenv("_SP_STARTUP_MESSAGE_" = "none")
    }
    # add resource path to www
    shiny::addResourcePath("www", system.file("www", package = "lsmssamptrain"))
    shiny::addResourcePath("data", system.file("data", package = "lsmssamptrain"))
    # get original options
    original_options <- list(
        shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
        sp_startup_message = getOption("sp_startup_message")
    )
    # change options and revert on stop
    changeoptions <- function() {
        options(shiny.maxRequestSize = 500 * 1024^2, sp_startup_message = "none")
        Sys.setenv("_SP_STARTUP_MESSAGE_" = "none")

        # revert to original state at the end
        shiny::onStop(function() {
            if (!is.null(original_options)) {
                options(original_options)
            }
        })
    }
    # create app & run
    appObj <- shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
    shiny::runApp(appObj, launch.browser = launch.browser, quiet = TRUE)
}

#' @rdname runSampleTrainer
#' @export
run_app <- runSampleTrainer


