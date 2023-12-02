#' UI element for base table
#'
#' @keywords internal
#' @noRd

baseTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("selectedEvents"))
  )
}

#' Server logic for base table
#'
#'
#' @keywords internal
#' @noRd

baseTableSRV <- function(id, mapPop) {
  moduleServer(id, function(input, output, session) {
    # Your server logic goes here ...
    smTab<-list(dom="t")    #(used for DT)
    # Display selected events
    output$selectedEvents <- renderDT({
      sumVar <- c("age", "gender", "employment.status", "income", "cluster")
      # options(digits=2)
      pers <- subset(mapPop(), select = sumVar)
      tab <- data.frame(matrix(nrow = 8, ncol = 3))
      names(tab) <- c("Male", "Female", "Total Population")
      Variable <- c(
        "Number of Persons", "Employment mean", "Employment sd", "Age mean", "Age sd", "Income mean", "Income sd",
        "Income ICC"
      )
      tab[1, 1] <- length(pers$employment.status[pers$gender == "male"])
      tab[1, 2] <- length(pers$employment.status[pers$gender == "female"])
      tab[1, 3] <- length(pers$employment.status)

      tab[2, 1] <- mean(pers$employment.status[pers$gender == "male"])
      tab[2, 2] <- mean(pers$employment.status[pers$gender == "female"])
      tab[2, 3] <- mean(pers$employment.status)
      tab[3, 1] <- sd(pers$employment.status[pers$gender == "male"])
      tab[3, 2] <- sd(pers$employment.status[pers$gender == "female"])
      tab[3, 3] <- sd(pers$employment.status)

      tab[4, 1] <- mean(pers$age[pers$gender == "male"])
      tab[4, 2] <- mean(pers$age[pers$gender == "female"])
      tab[4, 3] <- mean(pers$age)
      tab[5, 1] <- sd(pers$age[pers$gender == "male"])
      tab[5, 2] <- sd(pers$age[pers$gender == "female"])
      tab[5, 3] <- sd(pers$age)

      tab[6, 1] <- mean(pers$income[pers$gender == "male"])
      tab[6, 2] <- mean(pers$income[pers$gender == "female"])
      tab[6, 3] <- mean(pers$income)
      tab[7, 1] <- sd(pers$income[pers$gender == "male"])
      tab[7, 2] <- sd(pers$income[pers$gender == "female"])
      tab[7, 3] <- sd(pers$income)
      male <- pers[pers$gender == "male", ]
      female <- pers[pers$gender == "female", ]
      tab[8, 1] <- 0.6 # ICCbare(cluster, income, male)
      tab[8, 2] <- 0.6 # ICCbare(cluster, income, female)
      tab[8, 3] <- 0.6 # ICCbare(cluster, income, pers)

      tab <- apply(tab, 2, round, 2)
      tab <- (cbind(Variable, tab))
      tab
    },
    options = smTab,
    server = T,
    width = "100%",
    height = "auto"
    )

  })
}


#' UI element for frame table
#'
#' @keywords internal
#' @noRd

frameTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("selectedEvents"))
  )
}

#' Server logic for frame table
#'
#'
#' @keywords internal
#' @noRd

frameTableSRV <- function(id, mapPopHH) {
  moduleServer(id, function(input, output, session) {
    # Your server logic goes here ...
    smTab<-list(dom="t")    #(used for DT)
    # Display selected events
    output$selectedEvents <- renderDT({
      frame <- mapPopHH()
      tab <- data.frame(matrix(nrow = 3, ncol = 8))
      names(tab) <- c(
        "Stratum", "Number of EAs", "Number of Households",
        "Number of Persons", "Cluster size (Av/min", "Unit costs", "Unit standard deviation income", "ICC"
      )
      tab[1, 1] <- "Tigray"
      tab[2, 1] <- "Amhara"
      tab[3, 1] <- "Oromia"
      tab[1, 2] <- length(unique(frame$cluster[frame$stratum == "1"]))
      tab[2, 2] <- length(unique(frame$cluster[frame$stratum == "3"]))
      tab[3, 2] <- length(unique(frame$cluster[frame$stratum == "4"]))
      tab[1, 3] <- length(unique(frame$hhidg[frame$stratum == "1"]))
      tab[2, 3] <- length(unique(frame$hhidg[frame$stratum == "3"]))
      tab[3, 3] <- length(unique(frame$hhidg[frame$stratum == "4"]))
      tab[1, 4] <- round(sum(frame$count[frame$stratum == "1"]) * (30000 / sum(frame$count)))
      tab[2, 4] <- round(sum(frame$count[frame$stratum == "3"]) * (30000 / sum(frame$count)))
      tab[3, 4] <- round(sum(frame$count[frame$stratum == "4"]) * (30000 / sum(frame$count)))
      tab[1, 5] <- paste(round(mean(frame$countHH[frame$stratum == "1"], na.rm = T)), "/", min(frame$countHH[frame$stratum == "1"], na.rm = T))
      tab[2, 5] <- paste(round(mean(frame$countHH[frame$stratum == "3"], na.rm = T)), "/", min(frame$countHH[frame$stratum == "3"], na.rm = T))
      tab[3, 5] <- paste(round(mean(frame$countHH[frame$stratum == "4"], na.rm = T)), "/", min(frame$countHH[frame$stratum == "4"], na.rm = T))
      tab[1, 6] <- round(sum(frame$dist[frame$stratum == "1"]) / length(unique(frame$hhidg[frame$stratum == "1"])), digits = 2)
      tab[2, 6] <- round(sum(frame$dist[frame$stratum == "3"]) / length(unique(frame$hhidg[frame$stratum == "3"])), digits = 2)
      tab[3, 6] <- round(sum(frame$dist[frame$stratum == "4"]) / length(unique(frame$hhidg[frame$stratum == "4"])), digits = 2)
      tab[1, 7] <- round(sd(frame$income[frame$stratum == "1"]) / length(unique(frame$hhidg[frame$stratum == "1"])), digits = 2)
      tab[2, 7] <- round(sd(frame$income[frame$stratum == "3"]) / length(unique(frame$hhidg[frame$stratum == "3"])), digits = 2)
      tab[3, 7] <- round(sd(frame$income[frame$stratum == "4"]) / length(unique(frame$hhidg[frame$stratum == "4"])), digits = 2)
      str1 <- frame[frame$stratum == "1", ]
      str2 <- frame[frame$stratum == "3", ]
      str3 <- frame[frame$stratum == "4", ]
      tab[1, 8] <- 0.6 # ICCbare(cluster, income, str1)
      tab[2, 8] <- 0.6 # ICCbare(cluster, income, str2)
      tab[3, 8] <- 0.6 # ICCbare(cluster, income, str3)
      tab
    },
    options = smTab,
    server = T,
    height = "auto"
    )

  })
}


#' UI element for baseMap
#'
#' @keywords internal
#' @noRd

pop.hh.mapUI <- function(id) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("pop.hh.map"), width = "100%", height = "700px")
  )
}

#' Server logic for baseMap
#'
#'
#' @keywords internal
#' @noRd

pop.hh.mapSRV <- function(id, mapPopHH, shp) {
  moduleServer(id, function(input, output, session) {
    # Display selected events
    output$pop.hh.map <- renderLeaflet({
      h <- mapPopHH()
      eth.shp <- shp()
      ##  Create popups
      popup.hh <- paste0(
        sep = "<br/>", "<b>HHID</b> ",
        h$hhidg
      )
      popup.distr <- paste0(
        sep = "<br/>", "<b>HHID</b> ",
        eth.shp$NAME_1
      )

      ##  Select colors
      col_dist <- colorFactor("Spectral", h$distCat)
      col_str <- colorFactor("Spectral", eth.shp$NAME_1)
      ##  Create the map
      map <- leaflet() %>%
        addProviderTiles("Esri.WorldImagery",
                         layerId = 1,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addPolygons(
          data = eth.shp, weight = 1, color = "black", fillColor = ~ col_str(NAME_1), layerId = 2,
          fillOpacity = 0.7, popup = popup.distr
        ) %>%
        addMarkers(
          data = as.data.frame(h), lng = ~lon, lat = ~lat, popup = popup.hh,
          clusterOptions = markerClusterOptions()
        )
    })

  })
}
