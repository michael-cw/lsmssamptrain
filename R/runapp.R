#' Start the LSMS Sampling Trainer Application application
#'
#'
#' @description Shiny application to ...
#'
#'
#'
#' @details This function is used to start the application.
#'
#' @inherit shiny::runApp
#'
#'
#' @export
#'


runSampleTrainer <- function(launch.browser = T) {
    # add resource path to www
    shiny::addResourcePath("www", system.file("www", package = "lsmssamptrain"))
    shiny::addResourcePath("data", system.file("data", package = "lsmssamptrain"))
    # get original options
    original_options <- list(shiny.maxRequestSize = getOption("shiny.maxRequestSize"))
    # change options and revert on stop
    changeoptions <- function() {
        options(shiny.maxRequestSize = 500 * 1024^2)

        # revert to original state at the end
        shiny::onStop(function() {
            if (!is.null(original_options)) {
                options(original_options)
            }
        })
    }
    # create app & run
    appObj <- shiny::shinyApp(ui = main_ui, server = main_server, onStart = changeoptions)
    shiny::runApp(appObj, launch.browser = launch.browser, quiet = T)
}

