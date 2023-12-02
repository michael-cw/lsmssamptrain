# ` Shiny server LSMS Sampling Trainer Application
#'
#'
#'
#' @keywords internal
#' @noRd



###########
main_server <- function(input, output, session) {

  # Style
  smTab<-list(dom="t")    #(used for DT)
  styleMain<-theme(legend.justification=c(0,0), legend.position=c(0,0),
                   legend.background=element_rect(fill=alpha('blue', 0.3)),
                   legend.title = element_text(colour = 'red', face = 'bold', size=12),
                   legend.text=element_text(colour = 'red', face = 'bold', size=11),
                   legend.key.size = unit(0.5, "cm"))
  styleMain_noLeg<-theme(legend.justification=c(0,0), legend.position="none",
                         legend.background=element_rect(fill=alpha('blue', 0.3)),
                         legend.title = element_text(colour = 'red', face = 'bold', size=12),
                         legend.text=element_text(colour = 'red', face = 'bold', size=11),
                         legend.key.size = unit(0.5, "cm"))


  mapPop <- reactive({
    ## Load from file, no SIMPOP here!
    #load("data/population.rda")
    population<-lsmssamptrain::population
    return(data.table::as.data.table(population))
  })

  mapPopHH <- reactive({
    shiny::validate(need(mapPop(), message = F))
    # load("data/population.hh.rda")
    population.hh<-lsmssamptrain::population.hh
    return(population.hh)
  })

  ETHSHP <- reactive({
    #load("data/eth.shp.rda")
    eth.shp<-lsmssamptrain::eth.shp
    return(eth.shp)
  })

  ##############################################################################################
  ##        START PAGE
  ##############################################################################################
  ##
  ##      - Creation of the base map
  ##      - Sampling distribution
  ##      - Country, pop size and other parameters
  ##      - Proportions, MoE and CoeffInt
  ##      - Display of inital sample size (plus Formula)
  ##################################################################################
  ##    Base map
  pop.hh.mapSRV("pop.hh.map", mapPopHH, ETHSHP)

  output$dens_plot_start <- renderPlotly({
    prec <- as.numeric(req(input$precision))/100

    #############
    ## PROPORTION
    if (input$sampsiType == 1) {
      m <- input$prop
      #sd <- round(((m * (1 - m) / srs_size()))^0.5, 3)
      cv<-prec/1.96
      sd<-m*cv

      dens<-plot_normal_distribution_with_ci(m, sd, 0.95)
      gg <- ggplotly(dens)
      # dev.off()
      gg
    }
    ############
    ## CONTINOUS
    if (input$sampsiType == 2) {
      m <- input$cont_mean
      # sd <- round(input$cont_sd / (srs_size())^0.5, 3)
      cv<-prec/1.96
      sd<-m*cv

      dens<-plot_normal_distribution_with_ci(m, sd, 0.95)
      gg <- ggplotly(dens)
      # dev.off()
      gg
    }
    return(gg)
  })
  ##################################################################################
  ##    Calculate the sample size
  ##    1. Update Inputs with pop vals
  observeEvent(input$sampsiType, {
    if (input$sampsiType == 2) {
      dataPP <- mapPop()
      true_mean_cont <- round(mean(dataPP$income))
      true_sd_cont <- round(sd(dataPP$income))
      updateNumericInput(
        session = session,
        "cont_mean",
        "Please specify the mean of the variable.",
        value = true_mean_cont,
        min = 100, max = 1000000,
        step = 10
      )
      updateNumericInput(
        session = session,
        "cont_sd",
        "Please specify the SD of the variable.",
        value = true_sd_cont,
        min = 100, max = 1000000,
        step = 10
      )
    }
  })
  srs_size <- reactive({
    elig <- req(input$share)
    avhh <- as.numeric(req(input$avhhsize))
    prec <- as.numeric(req(input$precision))/100

    ################
    ##  Proportions
    if (input$sampsiType == 1) {
      srs<-ReGenesees::n.prop(
        prec = prec,
        prec.ind =  "RME",
        P = input$prop, F = input$share, hhSize = avhh, alpha = 0.05, verbose = F)
    }
    #########
    ##  Mean
    if (input$sampsiType == 2 & !is.null(input$cont_mean)) {
      shiny::validate(need(input$cont_mean > 0 & input$cont_sd > 0, message = F))
      srs<-ReGenesees::n.mean(
        prec = prec,
        prec.ind =  "RME",
        sigmaY = input$cont_sd, muY = input$cont_mean,
        F = input$share, hhSize = avhh, alpha = 0.05, verbose = F)
    }

    return(srs)
  })

  #################################
  #   Send sample size to input   #
  #################################
  observeEvent(srs_size(), {
    req(srs_size())
    updateNumericInput(
      session = session,
      "sampSizeFinal",
      "Recommended Sample Size:",
      value = srs_size(),
      step = 1,
      min = srs_size(),
      max = Inf
    )
  })


  ##################################################################################
  ##  Summary tables
  ##################################################################################
  popStatistics <- reactiveValues()
  observe({
    data <- mapPopHH()
    sumVar <- c("income", "cluster")
    pers <- subset(mapPop(), select = sumVar)
    data$hhsize <- as.numeric(data$hhsize)
    avhh <- mean(data$hhsize, na.rm = T)
    # rho<-ICCbare(cluster, income, pers)
    popStatistics$avhh <- avhh
    popStatistics$rho <- 0.6
    popStatistics$deff <- round((1 + (0.6 * (10 - 1))), digits = 2)
    popStatistics$N_stratum <- length(unique(data$stratum))
  })

  ##################################################################################
  ##    Creat sample size table
  ##    - Does not show on first page anylonger!
  ##################################################################################
  sampsi_tab <- reactiveValues()
  observe({
    rho <- popStatistics$rho
    deff <- popStatistics$deff
    avhh <- as.numeric(input$avhhsize)
    # print(avhh)

    tab <- data.frame(matrix(nrow = 3, ncol = 5))
    names(tab) <- c("Design", "n HH", "n Pers.", "Mean", "SE")
    tab[1, 1] <- "SRS"
    tab[1, 2] <- input$sampSizeFinal
    tab[1, 3] <- round(input$sampSizeFinal * avhh)
    tab[1, 4] <- paste(as.character(as.numeric(input$cont_mean)))
    tab[1, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$cont_mean)))

    tab[2, 1] <- "STRSRS (equal precision across strata/domains)"
    tab[2, 2] <- input$sampSizeFinal * popStatistics$N_stratum
    tab[2, 3] <- ceiling(input$sampSizeFinal * popStatistics$N_stratum * avhh)
    tab[2, 4] <- paste(as.character(as.numeric(input$cont_mean)))
    tab[2, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$cont_mean)))

    tab[3, 1] <- "Cluster (incl. deff)"
    tab[3, 2] <- ceiling(input$sampSizeFinal * deff)
    tab[3, 3] <- ceiling(input$sampSizeFinal * deff * avhh)
    tab[3, 4] <- paste(as.character(as.numeric(input$cont_mean)))
    tab[3, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$cont_mean)))
    sampsi_tab$tab_mean <- tab
  })

  observe({
    if (input$sampsiType == 1) {
      rho <- popStatistics$rho
      deff <- popStatistics$deff
      avhh <- as.numeric(input$avhhsize)

      tab <- data.frame(matrix(nrow = 3, ncol = 5))
      names(tab) <- c("Design", "n HH", "n Pers.", "Prop.", "SE")
      srsSize <- input$sampSizeFinal
      tab[1, 1] <- "SRS"
      tab[1, 2] <- srsSize
      tab[1, 3] <- round(srsSize * avhh)
      tab[1, 4] <- paste(as.character(as.numeric(input$prop) * 100), "%")
      tab[1, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$prop) * 100), "%")

      tab[2, 1] <- "STRSRS (equal precision across strata/domains)"
      tab[2, 2] <- input$sampSizeFinal * popStatistics$N_stratum
      tab[2, 3] <- ceiling(input$sampSizeFinal * popStatistics$N_stratum * avhh)
      tab[2, 4] <- paste(as.character(as.numeric(input$prop) * 100), "%")
      tab[2, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$prop) * 100), "%")

      tab[3, 1] <- "Cluster (incl. deff)"
      tab[3, 2] <- ceiling(input$sampSizeFinal * deff)
      tab[3, 3] <- ceiling(input$sampSizeFinal * deff * avhh)
      tab[3, 4] <- paste(as.character(as.numeric(input$prop) * 100), "%")
      tab[3, 5] <- paste(as.character((as.numeric(input$precision)/100) * as.numeric(input$prop) * 100), "%")
      sampsi_tab$tab_prop <- tab
    }
  })


  ##    For PROP individually at each section
  output$samplesizeTable <- DT::renderDataTable(
    {
      if (input$sampsiType == 1) {
        tab <- sampsi_tab$tab_prop
        shiny::validate(need(tab, message = F))
        tab <- tab[1, ]
        tab <- tab
      }
      if (input$sampsiType == 2 & !is.null(input$cont_mean)) {
        tab <- sampsi_tab$tab_mean
        tab <- tab[1, ]
      }
      return(tab)
    },
    options = smTab,
    server = T,
    width = "100%",
    height = "auto"
  )


  ##################################################################################
  ##    Table with initial values
  baseTableSRV("baseTable",mapPop=mapPop)  # Server function for baseTable

  ##################################################################################
  ## Table with frame characteristics
  frameTableSRV("frameTable",mapPopHH=mapPopHH)  # Server function for frameTable)

  ##################################################################################
  ##  DOWNLOAD THE DATASET
  ##################################################################################
  output$stata_pop <- downloadHandler(
    filename = function() {
      paste("sampling_frame", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- mapPop()
      fwrite(data, file)
    }
  )


  ##################################################################################
  ##  MODAL DIOLOGUE
  ##  A. SLIDES
  ##  1. Introduction
  ##################################################################################

  # will be added later

  ##################################################################################
  ##  B. INFO&HELP
  observeEvent(input$infoProp,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Proportions (discrete variable)<big></font></strong>")
                   ),
                   renderText("The proportion in the population which you expect to be found in your target population.
                An expected unemployment rate of 10% would mean a proportion of 0.1. The graph to the right displays a hypothetical sampling distribution.
               You can get these estimates from past survey data, or sometimes even from censuses."),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  observeEvent(input$infoElig,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Eligebility<big></font></strong>")
                   ),
                   renderText("The share in the population which you expect to carry the respective attribute,
              i.e. beeing eligible for the characteristic. For the unemployment rate, this would be the
              share in the population at or above working age. The smaller this share,
              the larger has to be your sample size. You can get these estimates from past survey data, or sometimes even from censuses."),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  observeEvent(input$infoMoe,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Margin of Error (MOE)<big></font></strong>")
                   ),
                   renderText("The degree of precision is called the Margin of Error which is specified here in relative terms.
              It is half the intervall you allow your estimate to vary. The smaller you choose this value,
              the higher your sample size needs to be."),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  observeEvent(input$infoHHsize,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Average Household size<big></font></strong>")
                   ),
                   renderText("The larger your households are, the less you need to sample, to achieve the same number of Persons"),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  observeEvent(input$infoAver,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Average (continous variable)<big></font></strong>")
                   ),
                   renderText("You  need to select the expected mean and the standard deviation of the variable. The higher the standard deviation,
                the larger is the heterogenity in the population, and consequently the larger your sample size needs to be.
               You can get these estimates from past survey data, or sometimes even from censuses"),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  observeEvent(input$infoSD,
               {
                 showModal(modalDialog(
                   title = tags$div(
                     HTML("<strong><font color='red'><big>Standard Deviation (continous variable)<big></font></strong>")
                   ),
                   renderText("You  need to select the expected mean and the standard deviation of the variable. The higher the standard deviation,
               the larger is the heterogenity in the population, and consequently the larger your sample size needs to be.
               You can get these estimates from past survey data, or sometimes even from censuses"),
                   footer = NULL,
                   easyClose = TRUE, size = "s"
                 ))
               },
               suspended = FALSE
  )

  ##############################################################################################
  ##        SRS PAGE
  ##############################################################################################
  ##    - Simulation to show the distribution of the mean.
  ##    - Creation of the SRS map
  ##    -> colored markers for the selected sample
  ##    -> calculate costs with respects to distance to ADDIS ABBABA
  ##    -> calculate weights
  ##################################################################################
  ##  1. SRS SAMPLE
  ##  1.1. SIMU (every 100 value data is handed over for graph/table)
  sample_srs <- reactiveValues(counter = 0, gplot_sample = NULL, sampMean = NULL, sampMOE = NULL)
  buttonACT <- reactiveValues(gogo = 0)
  observeEvent(input$generate,
               {
                 buttonACT$gogo <- 1
                 buttonACT$sim <- input$sim
                 store$h <- data.frame(mean = as.numeric(character()))
                 store$moe <- data.frame(moe = as.numeric(character()))
                 sample_srs$counter <- 0
               },
               priority = 1
  )

  observe(
    {
      ##  a. Update control/session parameters
      gogo <- buttonACT$gogo
      simu <- buttonACT$sim
      validate(need(simu, message = F))
      ##  a.1. Simulation start
      if (simu != 1 & !is.na(input$sim) & gogo == 1) {
        maxSim <- simu / 100
        ##  a.2. Simulation reste
        if (isolate(sample_srs$counter) == (maxSim - 1) | input$stop == 1) {
          updateNumericInput(session, "sim", "Select the number of times you want to repeat the simulation", 100)
          gogo <- 0
          buttonACT$gogo <- gogo
        }
        ##  b. Load permanent Data
        isolate({
          elig <- req(input$share)
          avhh <- as.numeric(req(input$avhhsize))
          size <- input$sampSizeFinal
          dataHH <- data.table(mapPopHH(), key = "hhidg")
          dataPP <- data.table(mapPop(), key = "hhidg")
          sampMeanExp <- vector(mode = "numeric", length = 100)
          sampMoeExp <- vector(mode = "numeric", length = 100)
          pop <- length(dataPP$hhidg)
          popHH <- length(dataHH$hhidg)
          true_mean_prop <- mean(dataPP$employment.status)
          true_mean_cont <- mean(dataPP$income)
          store$true_mean_prop <- true_mean_prop
          store$true_mean_cont <- true_mean_cont
          ##  c.Inclusion Probabilities

          if (input$sampsiType == 1) {
            for (i in 1:100) {
              samp_temp <- dataHH[, samp := srswor(size, .N)]
              samp_temp[, pik := size / popHH]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP <- dataPP[samp_temp, nomatch = 0]


              samp_mean <- mean(samp_tempPP[samp == 1, employment.status])
              sampMeanExp[i] <- samp_mean

              sampMoeExp[i] <- ReGenesees::prec.prop(
                prec.ind =  "RME", n=size,
                P = (1-samp_mean), F = input$share, hhSize = avhh, alpha = 0.05, verbose = F)

            }
            sample_srs$sampMean <- sampMeanExp
            sample_srs$sampMOE <- sampMoeExp
            sample_srs$gplot_sample <- samp_temp[samp == 1]
            sample_srs$counter <- sample_srs$counter + 1
          } else {
            for (i in 1:100) {
              samp_temp <- dataHH[, samp := srswor(size, .N)]
              samp_temp[, pik := size / popHH]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP <- dataPP[samp_temp, nomatch = 0]

              samp_mean <- mean(samp_tempPP[samp == 1, income])
              sampMeanExp[i] <- samp_mean

              sampMoeExp[i] <- ReGenesees::prec.mean(
                prec.ind =  "RME", n=size, muY = samp_mean,
                sigmaY = sd(samp_tempPP[samp == 1, income]), F = input$share, hhSize = avhh, alpha = 0.05, verbose = F)
            }
            sample_srs$sampMean <- sampMeanExp
            sample_srs$sampMOE <- sampMoeExp
            sample_srs$gplot_sample <- samp_temp[samp == 1]
            sample_srs$counter <- sample_srs$counter + 1
          }
        })
        sample_srs$sampMeanFull <- sampMeanExp
        sample_srs$sampMoeFull <- sampMoeExp
        if (isolate(sample_srs$counter) < maxSim) {
          invalidateLater(0, session)
        }
      }
    },
    priority = 0
  )

  ##  c. Storage function for the reactiveValues of the vector of means
  store <- reactiveValues()
  store$h <- data.frame(mean = as.numeric(character()))
  store$moe <- data.frame(moe = as.numeric(character()))


  ##  e. Generate the message for interruption
  observeEvent(input$stop, {
    c <- sample_srs$counter * 100
    sample_srs$counter <- 0
    session$sendCustomMessage(type = "testmessage", message = list("You have decided to interrupt the simulation, values are shown until simulation number:", c))
    buttonACT$gogo <- 1
    buttonACT$sim <- 0
    store$h <- data.frame(mean = as.numeric(character()))
    store$moe <- data.frame(moe = as.numeric(character()))
    sample_srs$counter <- 0
    # print("reset")
  })

  ##################################################################################
  ##  MAPS
  ##  1. BASE map
  output$pop.hh.map.srs <- renderLeaflet({
    validate(need(mapPopHH(), message = F))
    eth.shp<-req(ETHSHP())
    h <- mapPopHH()
    ##  Create popups
    popup.hh <- paste0(
      sep = "<br/>", "<b>HHID</b> ",
      h$hhidg
    )
    popup.distr <- paste0(
      sep = "<br/>", "<b>District</b> ",
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
    return(map)
  })
  ##  2. SAMPLE map
  observe({
    s <- sample_srs$gplot_sample
    shiny::validate(
      need(mapPopHH(), message = F),
      need(exists("s"), message = F)
    )
    isolate({
      h <- mapPopHH()
      h <- data.table(h, key = "hhidg")
    })
    s <- data.table(s, key = "hhidg")
    if (nrow(s) == 0) {
      return(NULL)
    }
    h <- h[s, nomatch = 0]
    # print(head(h))
    leafletProxy("pop.hh.map.srs") %>%
      clearMarkerClusters() %>%
      addMarkers(
        data = as.data.frame(h), lng = ~lon, lat = ~lat,
        clusterOptions = markerClusterOptions()
      )
  })
  ##################################################################################
  ##  HISTOGRAM SAMPLE
  output$hist_srs <- renderPlotly({
    simu <- buttonACT$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    ##  A. Reading in old data
    maxSim <- simu / 100
    isolate({
      # maxSim<-input$sim/100
      h <- as.data.frame(store$h)
      moe <- as.data.frame(store$moe)
      p <- mapPop()
    })
    ##  B. Transform the data
    #m <- ifelse(input$sampsiType == 1, mean(p$employment.status), mean(p$income))
    m <- ifelse(input$sampsiType == 1, mean(p$employment.status), mean(sample_srs$sampMeanFull))
    h_new <- as.data.frame(sample_srs$sampMeanFull)
    moe_new <- as.data.frame(sample_srs$sampMoeFull)
    names(moe_new) <- c("moe")
    moe_new$moe <- mean(moe_new$moe, na.rm=T)

    if(input$sampsiType==1) {
      moe_new$moe <- moe_new$moe # / m
    } else {
      moe_new$moe <- moe_new$moe # / m
    }

    h <- rbind(h, h_new)
    moe <- rbind(moe, moe_new)
    ##  C. Reading out new data
    if (sample_srs$counter <= maxSim) {
      isolate({
        store$h <- h
      })
      isolate({
        store$moe <- moe
      })
    }
    names(h) <- "mean"
    ##  D. Create the plot
    if (input$sampsiType == 1) {
      hist <- ggplot(h, aes(x = mean, after_stat(count) / sum(after_stat(count)))) +
        geom_histogram(na.rm = F, binwidth = 0.0001, color = "#009FDA") +
        geom_vline(xintercept = m, color = "red", size = 1) +
        xlab("") +
        ylab("") +
        styleMain_noLeg
      hist1 <- ggplotly()
      if (exists("hist1")) {
        # dev.off()
        return(hist1)
      } else {
        return(NULL) ## THIS procedure is necessary as otherwis plotly exports the NULL and shows error
      }
    } else if (input$sampsiType == 2) {
      hist <- ggplot() +
        geom_histogram(
          data = h, aes(x = mean, y = after_stat(density)),
          bins = 50, fill = "#009FDA", color = "black"
        )
      # geom_vline(xintercept = m, color="red", size=1)+
      # xlab("") + ylab("")+styleMain_noLeg
      hist1 <- ggplotly()
      if (exists("hist1")) {
        # dev.off()
        return(hist1)
      } else {
        return(NULL) ## THIS procedure is necessary as otherwis plotly exports the NULL and shows error
      }
    }
  })

  ##################################################################################
  ##  TABLE Random
  ##  1. DISTRICT summary
  output$tab_srs <- renderDataTable(
    {
      simu <- buttonACT$sim
      shiny::validate(
        need(simu, message = F),
        need(mapPopHH(), message = F)
      )
      maxSim <- simu / 100
      if ((sample_srs$counter) == maxSim | input$stop == 1) {
        isolate({
          tab <- data.frame(matrix(nrow = 3, ncol = 5))
          names(tab) <- c(
            "Stratum", "Number of EAs", "Number of Households",
            "Number of Persons", "Total Costs"
          )
          p <- mapPop()
          h <- mapPopHH()
          h_srs <- store$h
          if (is.null(sample_srs$gplot_sample)) {
            return()
          } else {
            frame <- sample_srs$gplot_sample
            tab[1, 1] <- "Tigray"
            tab[2, 1] <- "Amhara"
            tab[3, 1] <- "Oromia"
            tab[1, 2] <- length(unique(frame$cluster[frame$stratum == "1"]))
            tab[2, 2] <- length(unique(frame$cluster[frame$stratum == "3"]))
            tab[3, 2] <- length(unique(frame$cluster[frame$stratum == "4"]))
            tab[1, 3] <- length(unique(frame$hhidg[frame$stratum == "1"]))
            tab[2, 3] <- length(unique(frame$hhidg[frame$stratum == "3"]))
            tab[3, 3] <- length(unique(frame$hhidg[frame$stratum == "4"]))
            tab[1, 4] <- round(sum(frame$count[frame$stratum == "1"]))
            tab[2, 4] <- round(sum(frame$count[frame$stratum == "3"]))
            tab[3, 4] <- round(sum(frame$count[frame$stratum == "4"]))
            tab[1, 5] <- ceiling((sum(frame$dist[frame$stratum == "1"])))
            tab[2, 5] <- ceiling((sum(frame$dist[frame$stratum == "3"])))
            tab[3, 5] <- ceiling((sum(frame$dist[frame$stratum == "4"])))
          }
          tab_srs_sum <- tab
        })
        tab
      }
    },
    options = smTab
  )

  ##  2. SAMPLE summary
  output$tab_srs_sample <- renderDataTable(
    {
      simu <- buttonACT$sim
      shiny::validate(
        need(simu, message = F),
        need(mapPopHH(), message = F)
      )
      maxSim <- simu / 100
      if ((sample_srs$counter) == maxSim | input$stop == 1) {
        isolate({
          tab <- data.frame(matrix(nrow = 2, ncol = 4))
          names(tab) <- c("Gender", "Employment share", "Age (mean)", "Income (mean)")
          p <- mapPop()
          h <- mapPopHH()
          h_srs <- store$h
          ### CALCULATE DISTANCES FOR COSTS ->> done in main household loading
          tab[1, 1] <- "Male"
          tab[2, 1] <- "Female"
          tab[1, 2] <- ifelse(input$sampsiType == 1, mean(p$employment.status), mean(p$income))
          tab[1, 3] <- 30000

          ## Restrict the sample operations when sample is not NULL
          if (!is.null(sample_srs$gplot_sample)) {
            s <- sample_srs$gplot_sample
            sh <- merge(s, h, by = "hhidg")
            sp <- merge(s[, .(hhidg)], p, by = "hhidg")
            tab[1, 2] <- mean(sp$employment.status[sp$gender == "male"])
            tab[2, 2] <- mean(sp$employment.status[sp$gender == "female"])
            tab[1, 3] <- mean(sp$age[sp$gender == "male"])
            tab[2, 3] <- mean(sp$age[sp$gender == "female"])
            tab[1, 4] <- mean(sp$income[sp$gender == "male"])
            tab[2, 4] <- mean(sp$income[sp$gender == "female"])
          }
        })
        tab
      }
    },
    options = smTab
  )


  ##    Creat sample size table
  output$samplesizeTable_srs <- DT::renderDataTable(
    {
      if (input$sampsiType == 1) {
        tab <- sampsi_tab$tab_prop
        shiny::validate(need(tab, message = F))
        tab <- tab[1, ]
      }
      if (input$sampsiType == 2 & !is.null(input$cont_mean)) {
        tab <- sampsi_tab$tab_mean
      }
      return(tab)
    },
    options = smTab,
    server = T,
    width = "100%",
    height = "auto"
  )

  ##################################################################################
  ##  MODALS SRS
  ##  1. MOE
  observe({
    simu <- buttonACT$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim <- simu / 100
    if ((sample_srs$counter) == (maxSim - 1) | input$stop == 1 & is.null(input$sim)) {
      isolate({
        moe <- store$moe
        moe <- mean(moe[, 1]) * 100
        print(moe)
      })
      showModal(modalDialog(
        title = tags$div(
          HTML("<strong><font color='red'><big>Margin of Error (MOE) (relative)<big></font></strong>")
        ),
        renderText(paste("Your relative Margin of Error is:", round(moe, digits = 3), "%")),
        footer = NULL,
        easyClose = TRUE, size = "s"
      ))
    }
  })



  ##################################################################################
  ##  DOWNLOAD THE DATASET
  output$stata_srs <- downloadHandler(
    filename = function() {
      paste("srs_sample", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      sample <- data.table(sample_srs$gplot_sample, key = "hhidg")
      data <- data.table(mapPopHH(), key = "hhidg")
      data <- data[sample, nomatch = 0]
      fwrite(data, file)
    }
  )
















  ##################################################################################

}
