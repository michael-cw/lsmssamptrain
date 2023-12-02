#' @keywords internal
"_PACKAGE"

## usethis namespace: start

#' @importFrom data.table .BY
#' @importFrom data.table .EACHI
#' @importFrom data.table .GRP
#' @importFrom data.table .I
#' @importFrom data.table .N
#' @importFrom data.table .NGRP
#' @importFrom data.table .SD
#' @importFrom data.table :=
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom DT DTOutput
#' @importFrom DT renderDT
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @importFrom waiter spin_fading_circles
#' @importFrom waiter use_waiter
#' @importFrom waiter waiter_hide
#' @importFrom waiter waiter_show
#' @importFrom shinydashboard tabBox tabItem
#' @importFrom leaflet addMarkers addPolygons addProviderTiles clearMarkerClusters
#' colorFactor leaflet leafletOutput leafletProxy markerClusterOptions providerTileOptions renderLeaflet
#' @importFrom data.table data.table fwrite setkeyv setorderv first is.data.table
#' @importFrom dplyr group_by mutate n n_distinct summarise ungroup
#' @importFrom ggplot2 aes annotate geom_histogram geom_segment geom_vline ggplot stat_function xlab xlim ylab theme alpha element_rect
#' element_text unit after_stat after_scale geom_ribbon geom_line geom_point geom_smooth scale_x_continuous scale_y_continuous
#' ggtitle labs theme_bw theme_minimal theme_void
#' @importFrom grDevices pdf
# #' @importFrom Matrix as.matrix head mean print summary
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
# #' @importFrom plyr . mutate summarise
# #' @importFrom PracTools strAlloc
# #' @importFrom raster as.data.frame as.factor as.matrix cut mean merge nrow subset
# #' @importFrom readstata13 save.dta13
# #' @importFrom rgdal summary
#' @importFrom sampling getdata HTestimator srswor strata
#' @importFrom stats dnorm median qnorm sd runif
#' @importFrom utils head read.csv
#' @importFrom sf st_read
## usethis namespace: end
NULL

utils::globalVariables(c(".", "lon", "hhidg",
                         "lat", "stratum", "countHH", "count", "density", "value",
                        "income", "hhsize", "cluster", "population", "population.hh",
                        "eth.shp", "x", "samp", "pik", "employment.status")
                       )
