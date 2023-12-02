#' UI element
#'
#'
#' @keywords internal
#' @noRd


pop.hh.map.srsUI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("pop.hh.map.srs"), width = "100%", height = "550px")
  )
}

#' @keywords internal
#' @noRd

hist_srsUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("hist_srs"))
  )
}

#' @keywords internal
#' @noRd

tab_srsUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("tab_srs"))
  )
}

#' @keywords internal
#' @noRd

tab_srs_sampleUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("tab_srs_sample"))
  )
}

#' @keywords internal
#' @noRd

samplesizeTable_srsUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("samplesizeTable_srs"))
  )
}


#' Server logic
#'
#'
#' @keywords internal
#' @noRd

srssampSRV <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Your server logic goes here ...

    # Display selected events
    output$selectedEvents <- renderDT({
      filteredData <- data.frame(
        Event = c("Event 1", "Event 2", "Event 3"),
        Date = c("2020-01-01", "2020-01-02", "2020-01-03"),
        stringsAsFactors = FALSE
      )
      datatable(filteredData, options = list(order = list(list(1, 'asc'))), rownames = FALSE)
    })

  })
}
