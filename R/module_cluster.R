#' @keywords internal
#' @noRd

pop.hh.map.cluUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("pop.hh.map.clu"), width = "100%", height = "550px")
  )
}

#' @keywords internal
#' @noRd

hist_cluUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    plotly::plotlyOutput(ns("hist_clu"))
  )
}

#' @keywords internal
#' @noRd

tab_cluUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("tab_clu"))
  )
}

#' @keywords internal
#' @noRd

tab_clu_sampleUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("tab_clu_sample"))
  )
}

#' @keywords internal
#' @noRd

samplesizeTableCluUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::DTOutput(ns("samplesizeTableClu"))
  )
}
#' Server logic
#'
#'
#' @keywords internal
#' @noRd

clusampSRV <- function(id, mapPop, mapPopHH, ETHSHP) {
  shiny::moduleServer(id, function(input, output, session) {
    smTab<-list(dom="t")    #(used for DT)
    
    output$pop.hh.map.clu <- leaflet::renderLeaflet({
      shiny::validate(shiny::need(mapPopHH(), message = F))
      h <- mapPopHH()
      eth.shp <- ETHSHP()
      popup.hh <- paste0(sep = "<br/>", "<b>HHID</b> ", h$hhidg)
      popup.distr <- paste0(sep = "<br/>", "<b>District</b> ", eth.shp$NAME_1)
      col_str <- leaflet::colorFactor("Spectral", eth.shp$NAME_1)
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("Esri.WorldImagery", layerId = 1, options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
        leaflet::addPolygons(data = eth.shp, weight = 1, color = "black", fillColor = ~ col_str(NAME_1), layerId = 2, fillOpacity = 0.7, popup = popup.distr) %>%
        leaflet::addMarkers(data = as.data.frame(h), lng = ~lon, lat = ~lat, popup = popup.hh, clusterOptions = leaflet::markerClusterOptions())
    })
    
  })
}
