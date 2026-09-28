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

  sim_store <- reactiveValues(
    srs = NULL,
    str_eq = NULL,
    str_prop = NULL,
    str_ney = NULL,
    str_opt = NULL,
    clu_srs = NULL,
    clu_pps = NULL,
    last_sample = NULL,
    last_design = NULL
  )

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
    if (input$sampsiType == 2 && !is.null(input$cont_mean) && length(input$cont_mean) > 0 && !is.na(input$cont_mean)) {
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
    }
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
          sim_store$srs <- list(
            design = "Simple Random Sampling (SRS)",
            type = "SRS",
            n_hh = input$sampSizeFinal,
            n_pers = round(input$sampSizeFinal * as.numeric(input$avhhsize)),
            true_val = ifelse(input$sampsiType == 1, mean(p$employment.status), mean(p$income)),
            est_val = mean(sample_srs$sampMeanFull, na.rm = TRUE),
            moe = mean(sample_srs$sampMoeFull, na.rm = TRUE),
            cost = sum(as.numeric(tab[["Total Costs"]]), na.rm = TRUE),
            eas = sum(as.numeric(tab[["Number of EAs"]]), na.rm = TRUE),
            table = tab,
            sample = frame
          )
          sim_store$last_sample <- frame
          sim_store$last_design <- "Simple Random Sampling (SRS)"
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

  ##################################################################################
  ##################################################################################
  ##    STRATIFICATION page:
  ##    Simulation to show the distribution of the mean.
  ##    Creation of the SRS map
  ##    -> colored markers for the selected sample
  ##    -> calculate costs with respects to distance to addis
  ##    -> calculate weights
  ##################################################################################
  ##    DO THE ALLOCATION
  sample_str<-reactiveValues(counter=0, gplot_sample=NULL, sampMean=NULL, sampMoeFull=NULL)
  buttonACT1<-reactiveValues(gogo=0)
  observeEvent(input$generate1,{
    buttonACT1$gogo<-1
    buttonACT1$sim<-input$sim1
    store1$h<-data.frame(mean=as.numeric(character()))
    store1$moe<-data.frame(moe=as.numeric(character()))
    sample_str$counter<-0
    #print("reset")
  }, priority = 1)
  
  observeEvent(input$alloc, {
    if(input$alloc==4) {
      dataHH<-mapPopHH()
      size<-input$sampSizeFinal
      min_budget<-ceiling(dataHH[,(sum(dist)/.N)*size])
      updateNumericInput(session, 
                         "budget", "Please specify the maximum available survey budget", 150000,
                         min = 1, max = 100000, value = min_budget)
    }
  })
  ##################################################################################
  ##  STRATIFICATION
  ##################################################################################
  
  observe({
    gogo<-buttonACT1$gogo
    simu<-buttonACT1$sim
    validate(need(simu, message = F))
    
    if (simu!=1&gogo==1){
      maxSim<-simu/100
      ##  a.2. Simulation reste
      if (isolate(sample_str$counter )== (maxSim-1)| input$stop==1){
        updateNumericInput(session, "sim1", "Select the number of times you want to repeat the simulation", 1)
        gogo<-0
        buttonACT1$gogo<-gogo
        #print("gogo")
      }
      
      isolate({
        # 1. Loading common files
        maxSim<-input$sim1/100
        size<-input$sampSizeFinal
        dataHH<-mapPopHH()
        dataHH<-data.table(dataHH, key="hhidg")
        dataPP<-mapPop()
        dataPP<-data.table(dataPP, key="hhidg")
        true_mean_prop<-mean(dataPP$employment.status, na.rm=T)
        true_mean_cont<-mean(dataPP$income, na.rm=T)
        pop<-nrow(dataPP)
        pop_str<-dataPP[,.N, by="stratum"]
        sampMeanExp<-vector(mode="numeric", length = 100)
        sampMoeExp<-vector(mode="numeric", length = 100)
      })
      ##    EQUAL 
      if(input$alloc==1){
        isolate({
          size_str1<-ceiling(size/3)
          size_str2<-ceiling(size/3)
          size_str3<-ceiling(size/3)
          dataHH[,pik:=size_str1/.N, by="stratum"]
          if(input$sampsiType==1) {
            for (i in 1:100){
              samp_temp<-dataHH[,.SD[sample(.N, size_str1)], by="stratum"]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(employment.status[employment.status==1], pik[employment.status==1]), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_prop)/true_mean_prop
            }
          } else {
            for (i in 1:100){
              samp_temp<-dataHH[,.SD[sample(.N, size_str1)], by="stratum"]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(income, pik), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_cont)/true_mean_cont
            }
            print(samp_mean)
          }
          
          sample_str$sampMean<-sampMeanExp
          sample_str$gplot_sample<-samp_temp
          sample_str$counter<-sample_str$counter+1
        })
        sample_str$sampMeanFull<-sampMeanExp
        sample_str$sampMoeFull<-sampMoeExp
        ##Escape to interrupt the simulation and interrupt button as condition, observeEvent did not work in this case (even not with isolate)       
        if (isolate(sample_str$counter) < maxSim&input$stop1==0){
          invalidateLater(0, session)
        }
      }
      
      ##################################################################################
      ##    PROPORTIONAL 
      if(input$alloc==2){
        isolate({
          size1<-pop_str$N[1]/pop
          size2<-pop_str$N[2]/pop
          size3<-pop_str$N[3]/pop
          size_str1<-ceiling(size*(size1))
          size_str2<-ceiling(size*(size2))
          size_str3<-ceiling(size*(size3))
          n_str<-c(size_str1,size_str2, size_str3)
          sampMeanExp<-vector(mode="numeric", length = 100)
          setorderv(dataHH, "stratum")
          if(input$sampsiType==1) {
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(employment.status[employment.status==1], pik[employment.status==1]), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_prop)/true_mean_prop
            }
          } else {
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, "hhidg")
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(income, pik), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_cont)/true_mean_cont
            }
          }
          sample_str$sampMean<-sampMeanExp
          sample_str$gplot_sample<-samp_temp
          sample_str$counter<-sample_str$counter+1
        })
        sample_str$sampMeanFull<-sampMeanExp
        sample_str$sampMoeFull<-sampMoeExp
        ##Escape to interrupt the simulation and interrupt button as condition, observeEvent did not work in this case (even not with isolate)       
        if (isolate(sample_str$counter) < maxSim&input$stop1==0){
          invalidateLater(0, session)
        }
      }
      ##################################################################################
      ##  NEYMAN
      if(input$alloc==3){
        isolate({
          setkeyv(dataPP, c("stratum", "hhidg"))
          sampMeanExp<-vector(mode="numeric", length = 100)
          setorderv(dataHH, "stratum")
          if(input$sampsiType==1) {
            sd_strat1<-sd(dataPP[.("1"),employment.status])
            sd_strat2<-sd(dataPP[.("3"),employment.status]) 
            sd_strat3<-sd(dataPP[.("4"),employment.status]) 
            sd<-c(sd_strat1, sd_strat2, sd_strat3)
            N_strat<-c(pop_str$N[1], pop_str$N[2], pop_str$N[3])
            alloc<-strAlloc(n.tot = size, Nh = N_strat,Sh = sd, alloc="neyman" )
            alloc<-as.vector(round(alloc$nh))
            
            size_str1<-alloc[1]
            size_str2<-alloc[2]
            size_str3<-alloc[3]
            n_str<-c(size_str1, size_str2, size_str3)
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, c("stratum","hhidg"))
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, stratum, pik)], nomatch=0]
              #samp_tempPP<-merge(dataPP, samp_temp[,.(hhidg, pik)], by="hhidg")
              samp_mean_str<-samp_tempPP[,HTestimator(employment.status[employment.status==1], pik[employment.status==1]), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_prop)/true_mean_prop
            }
          } else {
            sd_strat1<-sd(dataPP[.("1"),income])
            sd_strat2<-sd(dataPP[.("3"),income]) 
            sd_strat3<-sd(dataPP[.("4"),income]) 
            sd<-c(sd_strat1, sd_strat2, sd_strat3)
            N_strat<-c(pop_str$N[1], pop_str$N[2], pop_str$N[3])
            alloc<-strAlloc(n.tot = size, Nh = N_strat,Sh = sd, alloc="neyman" )
            alloc<-as.vector(round(alloc$nh))
            
            size_str1<-alloc[1]
            size_str2<-alloc[2]
            size_str3<-alloc[3]
            n_str<-c(size_str1, size_str2, size_str3)
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, c("stratum","hhidg"))
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, stratum ,pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(income, pik), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_cont)/true_mean_cont
            }
          }          
          sample_str$sampMean<-sampMeanExp
          sample_str$gplot_sample<-samp_temp
          sample_str$counter<-sample_str$counter+1
        })
        sample_str$sampMeanFull<-sampMeanExp
        sample_str$sampMoeFull<-sampMoeExp
        ##Escape to interrupt the simulation and interrupt button as condition, observeEvent did not work in this case (even not with isolate)       
        if (isolate(sample_str$counter) < maxSim&input$stop1==0){
          invalidateLater(0, session)
        }
      }
      
      ##################################################################################
      ##  OPTIMAL
      if(input$alloc==4){
        isolate({
          budget<-input$budget
          setkeyv(dataPP, c("stratum", "hhidg"))
          setkeyv(dataHH, c("stratum", "hhidg"))
          if(input$sampsiType==1) {
            sum_strat1<-dataHH[.("1"),sum(dist)/.N]
            sum_strat2<-dataHH[.("3"),sum(dist)/.N]
            sum_strat3<-dataHH[.("4"),sum(dist)/.N]
            var_cost<-c(sum_strat1, sum_strat2, sum_strat3)
            sd_strat1<-sd(dataPP[.("1"),employment.status], na.rm=T)
            sd_strat2<-sd(dataPP[.("3"),employment.status], na.rm=T) 
            sd_strat3<-sd(dataPP[.("4"),employment.status], na.rm=T) 
            sd<-c(sd_strat1, sd_strat2, sd_strat3)
            N_strat<-c(pop_str$N[1], pop_str$N[2], pop_str$N[3])
            alloc<-strAlloc(Nh = N_strat, Sh = sd, cost = budget, ch=var_cost, alloc="totcost" )
            alloc<-as.vector(round(alloc$nh))
            size_str1<-alloc[1]
            size_str2<-alloc[2]
            size_str3<-alloc[3]
            
            n_str<-c(size_str1, size_str2, size_str3)
            sampMeanExp<-vector(mode="numeric", length = 100)
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, c("stratum","hhidg"))
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, stratum, pik)], nomatch=0]
              #samp_tempPP<-merge(dataPP, samp_temp[,.(hhidg, pik)], by="hhidg")
              samp_mean_str<-samp_tempPP[,HTestimator(employment.status[employment.status==1], pik[employment.status==1]), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_prop)/true_mean_prop
            }
          } else {
            sum_strat1<-dataHH[.("1"),sum(dist)/.N]
            sum_strat2<-dataHH[.("3"),sum(dist)/.N]
            sum_strat3<-dataHH[.("4"),sum(dist)/.N]
            var_cost<-c(sum_strat1, sum_strat2, sum_strat3)
            sd_strat1<-sd(dataPP[.("1"),income], na.rm=T)
            sd_strat2<-sd(dataPP[.("3"),income], na.rm=T) 
            sd_strat3<-sd(dataPP[.("4"),income], na.rm=T) 
            sd<-c(sd_strat1, sd_strat2, sd_strat3)
            N_strat<-c(pop_str$N[1], pop_str$N[2], pop_str$N[3])
            alloc<-strAlloc(Nh = N_strat, Sh = sd, cost = budget, ch=var_cost, alloc="totcost" )
            alloc<-as.vector(round(alloc$nh))
            size_str1<-alloc[1]
            size_str2<-alloc[2]
            size_str3<-alloc[3]
            
            n_str<-c(size_str1, size_str2, size_str3)
            sampMeanExp<-vector(mode="numeric", length = 100)
            for (i in 1:100){
              st<-sampling::strata(dataHH,stratanames=c("stratum"),size=n_str, method="srswor")
              samp_temp<-dataHH[c(st$ID_unit)][,c("pik", "Stratum"):=.(st$Prob, st$Stratum)]
              setkeyv(samp_temp, c("stratum","hhidg"))
              samp_tempPP<-dataPP[samp_temp[,.(hhidg, stratum ,pik)], nomatch=0]
              samp_mean_str<-samp_tempPP[,HTestimator(income, pik), by="stratum"]
              samp_mean_str<-samp_mean_str[,2]/pop_str$N
              samp_mean<-sum(samp_mean_str$V1)/length(unique(dataHH$stratum))
              sampMeanExp[i]<-samp_mean
              sampMoeExp[i]<-abs(samp_mean-true_mean_cont)/true_mean_cont
            }
          }
          sample_str$sampMean<-sampMeanExp
          sample_str$gplot_sample<-samp_temp
          sample_str$counter<-sample_str$counter+1
        })
        sample_str$sampMeanFull<-sampMeanExp
        sample_str$sampMoeFull<-sampMoeExp
        ##Escape to interrupt the simulation and interrupt button as condition, observeEvent did not work in this case (even not with isolate)       
        if (isolate(sample_str$counter) < maxSim&input$stop1==0){
          invalidateLater(0, session)
        }
      }
    }
  }, priority = 0)
  
  ##Storage function for the reactiveValues of the vector of means
  store1<-reactiveValues()
  store1$h<-data.frame(mean=as.numeric(character()))
  store1$moe<-data.frame(moe=as.numeric(character()))
  
  ##Generate the message for interruption
  observeEvent(input$stop1, {
    c<-sample_str$counter*100
    session$sendCustomMessage(type="testmessage", message=list("You have decided to interrupt the simulation, values are shown until simulation number:", c))
    buttonACT1$sim<-0
    store1$h<-data.frame(mean=as.numeric(character()))
    store1$moe<-data.frame(moe=as.numeric(character()))
    sample_str$counter<-0
    #print("reset")
  })
  
  ##################################################################################
  ##  MAPS Stratified
  ##  1. BASE map
  output$pop.hh.map.strsrs<-renderLeaflet({
    validate(need(mapPopHH(), message = F))
    h<-mapPopHH()
    ##  Create popups
    popup.hh<-paste0(sep= "<br/>", "<b>HHID</b> ", 
                     h$hhidg)
    popup.distr<-paste0(sep= "<br/>", "<b>District</b> ", 
                        eth.shp$NAME_1)
    ##  Select colors
    col_dist<-colorFactor('Spectral', h$distCat)
    col_str<-colorFactor('Spectral', eth.shp$NAME_1)
    
    ##  Create the map
    map<-leaflet() %>%
      addProviderTiles("Esri.WorldImagery", layerId=1,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      addPolygons(data=eth.shp, weight = 1, color="black", fillColor =~col_str(NAME_1), layerId=2,
                  fillOpacity=0.7, popup=popup.distr) %>%
      addMarkers(data = as.data.frame(h), lng=~lon, lat=~lat, popup=popup.hh,
                 clusterOptions=markerClusterOptions()) 
    return(map)
  })
  
  ##  2. SAMPLE map  
  observe({
    s<-sample_str$gplot_sample
    shiny::validate(need(mapPopHH(), message = F),
                    need(exists("s"), message = F))
    isolate({
      h<-mapPopHH()
      h<-data.table(h, key="hhidg")
    })
    s<-data.table(s, key="hhidg")
    if(nrow(s)==0) return(NULL)
    h<-h[s, nomatch=0]
    #print(head(h))
    leafletProxy("pop.hh.map.strsrs")%>%
      clearMarkerClusters() %>%
      addMarkers(data = as.data.frame(h), lng=~lon, lat=~lat,
                 clusterOptions=markerClusterOptions())
  })
  
  
  
  
  ##################################################################################
  ##  HIST Stratified
  output$hist_str<-renderPlotly({
    simu<-buttonACT1$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    isolate({ 
      h<-as.data.frame(store1$h)
      moe<-as.data.frame(store1$moe)
      p<-mapPop()
    })
    ## Transform the data
    m<-mean(p$employment.status)
    h_new<-as.data.frame(sample_str$sampMeanFull)
    moe_new<-as.data.frame(sample_str$sampMoeFull)
    #if(nrow(moe_new)>1) 
    names(moe_new)<-c("moe")
    h<-rbind(h, as.data.frame(h_new))
    moe<-rbind(moe, moe_new)
    if (sample_str$counter <= maxSim){
      isolate({
        store1$h<-h
        store1$moe<-moe
      })
    }
    names(h)<-"mean"
    if (input$sampsiType==1) {
      hist<-ggplot(h, aes(x=mean, ..count../sum(..count..)))+geom_histogram(na.rm = F, binwidth = 0.0001, color="#009FDA")+geom_vline(xintercept = m, color="red", size=1)+ 
        xlab("") + ylab("")+styleMain_noLeg
      hist1<-ggplotly(hist)
      
      if(exists("hist1")){
        #dev.off()
        return(hist1)
      } else {
        return(NULL) ## THIS procedure is necessary as otherwis plotly exports the NULL and shows error
      }
    } else {
      hist<-ggplot()+geom_histogram(data=h,aes(x=mean, y=..density..), 
                                    bins = 50, fill="#009FDA", color="black")
      # geom_vline(xintercept = m, color="red", size=1)+
      # xlab("") + ylab("")+styleMain_noLeg
      hist1<-ggplotly()
      if(exists("hist1")){
        #dev.off()
        return(hist1)
      } else {
        return(NULL) ## THIS procedure is necessary as otherwis plotly exports the NULL and shows error
      }
      
    }
  })
  ##################################################################################
  ##  TABLE Stratified
  ##  A. SAMPLE
  ##    1. DISTRICT
  output$tab_str<-renderDataTable({
    simu<-buttonACT1$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if ((sample_str$counter) == maxSim|input$stop1==1){
      isolate({
        tab<-data.frame(matrix(nrow=3, ncol = 5))
        names(tab)<-c("Stratum", "Number of EAs", "Number of Households", "Number of Persons", "Total Costs")
        p<-mapPop()
        p<-data.table(p, key="hhidg")
        h<-mapPopHH()
        h<-data.table(h, key="hhidg")
        h_srs<-store1$h
        ##Restrict the sample operations when sample is not NULL
        if(is.null(sample_str$gplot_sample)){
          return()
        }
        else{
          frame<-sample_str$gplot_sample
          tab[1,1]<-"Tigray"
          tab[2,1]<-"Amhara"
          tab[3,1]<-"Oromia"
          tab[1,2]<-length(unique(frame$cluster[frame$stratum=="1"]))
          tab[2,2]<-length(unique(frame$cluster[frame$stratum=="3"]))
          tab[3,2]<-length(unique(frame$cluster[frame$stratum=="4"]))
          tab[1,3]<-length(unique(frame$hhidg[frame$stratum=="1"]))
          tab[2,3]<-length(unique(frame$hhidg[frame$stratum=="3"]))
          tab[3,3]<-length(unique(frame$hhidg[frame$stratum=="4"]))
          tab[1,4]<-round(sum(frame$count[frame$stratum=="1"]))
          tab[2,4]<-round(sum(frame$count[frame$stratum=="3"]))
          tab[3,4]<-round(sum(frame$count[frame$stratum=="4"]))
          tab[1,5]<-(sum(frame$dist[frame$stratum=="1"]))
          tab[2,5]<-(sum(frame$dist[frame$stratum=="3"]))
          tab[3,5]<-(sum(frame$dist[frame$stratum=="4"]))
          alloc_name <- switch(as.character(input$alloc),
            "1" = "Stratified SRS (Equal Allocation)",
            "2" = "Stratified SRS (Proportional Allocation)",
            "3" = "Stratified SRS (Neyman Allocation)",
            "4" = "Stratified SRS (Optimal Allocation)",
            "Stratified SRS"
          )
          alloc_key <- switch(as.character(input$alloc),
            "1" = "str_eq", "2" = "str_prop", "3" = "str_ney", "4" = "str_opt", "str_prop"
          )
          sim_store[[alloc_key]] <- list(
            design = alloc_name,
            type = "Stratified",
            n_hh = sum(as.numeric(tab[["Number of Households"]]), na.rm = TRUE),
            n_pers = sum(as.numeric(tab[["Number of Persons"]]), na.rm = TRUE),
            true_val = ifelse(input$sampsiType == 1, mean(p$employment.status), mean(p$income)),
            est_val = mean(sample_str$sampMeanFull, na.rm = TRUE),
            moe = mean(sample_str$sampMoeFull, na.rm = TRUE),
            cost = sum(as.numeric(tab[["Total Costs"]]), na.rm = TRUE),
            eas = sum(as.numeric(tab[["Number of EAs"]]), na.rm = TRUE),
            table = tab,
            sample = frame
          )
          sim_store$last_sample <- frame
          sim_store$last_design <- alloc_name
        }
      })
      tab
    }
  },
  options=smTab)
  
  ##  2. PERSON
  output$tab_str_sample<-renderDataTable({
    simu<-buttonACT1$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if (sample_str$counter == maxSim|input$stop1==1){
      isolate({
        tab<-data.frame(matrix(nrow=2, ncol = 4))
        names(tab)<-c("Gender", "Employment", "Age", "Income")
        p<-mapPop()
        h<-mapPopHH()
        h_str<-store$h
        ### CALCULATE DISTANCES FOR COSTS ->> done in main household loading
        tab[1,1]<-"Male"
        tab[2,1]<-"Female"
        tab[1,2]<-mean(p$employment.status)
        tab[1,3]<-30000
        ##Restrict the sample operations when sample is not NULL
        if(!is.null(sample_str$gplot_sample)){
          s<-sample_str$gplot_sample
          #print("STOP")
          #str(s)
          #str(p)
          sh<-merge(s,h, by="hhidg")
          sp<-merge(s,p, by="hhidg")
          
          #str(sp)
          tab[1,2]<-mean(sp$employment.status[sp$gender=="male"])
          tab[2,2]<-mean(sp$employment.status[sp$gender=="female"])
          tab[1,3]<-mean(sp$age[sp$gender=="male"])
          tab[2,3]<-mean(sp$age[sp$gender=="female"])
          tab[1,4]<-mean(sp$income.y[sp$gender=="male"])
          tab[2,4]<-mean(sp$income.y[sp$gender=="female"])
        }
        
      })
      tab
    }
    
    
  },
  options=smTab)
  
  
  ##    Creat sample size table
  output$samplesizeTable_str<-DT::renderDataTable({
    if(input$sampsiType==1){
      tab<-sampsi_tab$tab_prop
      shiny::validate(need(tab, message=F))
      tab<-tab[1:2,]
    }
    if(input$sampsiType==2&!is.null(input$cont_mean)){
      tab<-sampsi_tab$tab_mean
    }
    return(tab)
  },options=smTab, server=T, width = "100%", height = "auto", style="bootstrap")
  
  ##################################################################################
  ##  MODAL DIOLOGUE STRATIFIED
  ##  A. SLIDES
  ##  1. Introduction
  observeEvent(input$slides4, {
    showModal(modalDialog(
      title = tags$div(
        HTML("<strong><font color='red'><big>Session 4: Stratification<big></font></strong>")),
      renderUI(tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                           src="sess4.pdf")),
      footer = NULL,
      easyClose = TRUE, size = "l"
    ))
  }, suspended = FALSE)
  
  ##  2. MOE
  observe({
    simu<-buttonACT1$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if (sample_str$counter == (maxSim-1)|input$stop1==1){
      isolate({
        moe<-store1$moe
        moe<-mean(moe[,1])*100
      })
      showModal(modalDialog(
        title = tags$div(
          HTML("<strong><font color='red'><big>Margin of Error (MOE) (relative)<big></font></strong>")),
        renderText(paste("Your relative Margin of Error is:", round(moe, digits = 3), "%")),
        footer = NULL,
        easyClose = TRUE, size = "s"
      ))
    }
  })
  
  ##################################################################################
  ##  DOWNLOAD THE DATASET
  output$stata_str <- downloadHandler(
    filename = function() {
      paste("str_sample", Sys.Date(), '.dta', sep='')
    },
    content = function(file) {
      sample<-data.table(sample_str$gplot_sample, key="hhidg")
      data<-data.table(mapPopHH(), key="hhidg")
      data<-data[sample, nomatch=0]
      save.dta13(data, file, convert.underscore = T)
    }
  )
  ##################################################################################
  ##    CLUSTER page:
  ##    Table with initial values and ESTIMATES (survey package)
  ##    Creation of the cluster map
  ##    -> colored markers for the selected sample
  ##    -> calculate costs with respects to distance to addis
  ##    -> calculate weights
  #     -> show icc effects
  ##################################################################################
  ##  ICC adjustment
  ##  APPENDIX: User can create population with own ICC
  mapPopICC<-eventReactive(input$generate_icc, {
    survey.df <- read.csv("C:/Users/wb475260/Box Sync/ethiopiForSimul.csv", header = T)
    #str(survey.df)
    mean_male<-mean(survey.df$hh_s4q16[survey.df$hh_s1q03==1], na.rm = T)
    mean_female<-mean(survey.df$hh_s4q16[survey.df$hh_s1q03==2], na.rm = T)
    sd_male<-sd(survey.df$hh_s4q16[survey.df$hh_s1q03==1], na.rm = T)
    sd_female<-sd(survey.df$hh_s4q16[survey.df$hh_s1q03==2], na.rm = T)
    iccmod<-input$iccmod
    #print(iccmod)
    population<-create.pop(survey.df, size=30000, mean_male, mean_female, sd_male, sd_female, iccmod)
    population$employment.status<-(as.numeric(population$employment.status)-1)
    return(data.frame(population))
  })
  
  mapPopHHICC<-eventReactive(input$generate_icc,{
    
    h<-mapPop()
    #source("helpers.R")
    h<-h%>%group_by(hhidg, lon)%>%mutate(count=n())
    h<-as.data.frame(h)
    population.hh<-create.pop.hh(h)
    ##drop na observations at lon
    population.hh<-population.hh[!is.na(population.hh$lon),]
    ##Calculate costs as distance, create cost strata
    population.hh$dist<-mapply(hav_dist, population.hh[, "lon.hh"], population.hh[, "lat.hh"])
    #print(sum(is.na(population.hh$lon[population.hh$stratum=="3"])))
    
    population.hh<-(population.hh%>%group_by(stratum)%>%mutate(distCat=median(dist)))
    population.hh<-as.data.frame(population.hh)
    population.hh$distCat<-factor(population.hh$distCat, labels = c("Low", "Medium", "High"))
    population.hh$stratum<-as.factor(population.hh$stratum)
    population.hh$cluster<-as.factor(population.hh$cluster)
    #str(population.hh)
    return(data.frame(population.hh))
    
  })
  
  ##################################################################################
  ##  SAMPLING Cluster 
  
  sample_clu<-reactiveValues(counter=0, gplot_sample=NULL, sampMeanProp=NULL, sampMeanCont=NULL)
  buttonACT2<-reactiveValues(gogo=0)
  observeEvent(input$generate2,{
    buttonACT2$gogo<-1
    buttonACT2$sim<-input$sim2
    buttonACT2$size_hh<-input$n_clust
    store2$h<-data.frame(mean=as.numeric(character()))
    store2$moe<-data.frame(moe=as.numeric(character()))
    sample_clu$counter<-0
    #print("reset")
  }, priority = 1)
  
  observe({
    gogo<-buttonACT2$gogo
    simu<-buttonACT2$sim
    size_hh<-buttonACT2$size_hh  
    validate(need(simu, message = F))
    if (simu!=1&gogo==1){
      maxSim<-simu/100
      ##  a.2. Simulation reste
      if (isolate(sample_clu$counter) == (maxSim-1)| input$stop2==1){
        updateNumericInput(session, "sim2", "Select the number of times you want to repeat the simulation", 1)
        gogo<-0
        buttonACT1$gogo<-gogo
        #print("STOP")
      }
      
      isolate({
        # 1. Loading common files
        size_clu<-round(input$sampSizeFinal/size_hh)
        dataHH<-mapPopHH()
        dataHH<-data.table(dataHH, key = "cluster")
        size_clu<-min(length(unique(dataHH$cluster)), size_clu)
        dataPP<-mapPop()
        dataPP<-data.table(dataPP, key = "hhidg")
        sampMeanExpProp<-vector(mode="numeric", length = 100)
        sampMeanExpCont<-vector(mode="numeric", length = 100)
        sampMoeExpProp<-vector(mode="numeric", length = 100)
        dataCLU<-data.table(dataHH %>% group_by(cluster) %>% summarise(HH_count=n_distinct(hhidg), countHH=mean(countHH)), key="cluster")
        dataCLU<-dataCLU[dataCLU$HH_count>size_hh,]
        dataCLU[,p1:=size_clu/.N]
        dataHH<-dataHH[dataCLU, nomatch=0]
        dataHH[,p2:=size_hh/countHH]
        pop<-nrow(dataPP)
        true_mean<-mean(dataPP$employment.status)
        #testHH<<-dataHH
        #testCLU<<-dataCLU
        
        if(input$cludesign==1){
          dataHH[,p1:=NULL]
          for (i in 1:100){
            ##  a) SAMPLE THE CLUSTERS
            ##  STAGE 1
            samp_temp_clu<-dataCLU[,.SD[sample(.N, size_clu)]]
            setkeyv(samp_temp_clu, "cluster")
            sampCluHH_st1<-dataHH[samp_temp_clu, nomatch=0]
            ##  STAGE 2
            samp_temp_hh<-sampCluHH_st1[,.SD[sample(.N, size_hh)], by=cluster]
            samp_temp_hh[,pik:=p1*p2]
            ##  PERSONS
            samp_temp_hh<-samp_temp_hh[,.(hhidg, p1, p2, pik)]
            setkeyv(samp_temp_hh, "hhidg")
            samp_tempPP<-dataPP[samp_temp_hh, nomatch=0]
            ##  ESTIMATION
            samp_mean_prop<-HTestimator(samp_tempPP[employment.status==1, employment.status], samp_tempPP[employment.status==1, pik])
            samp_mean_prop<-samp_mean_prop/pop
            sampMeanExpProp[i]<-samp_mean_prop
            sampMoeExpProp[i]<-abs(samp_mean_prop-true_mean)/true_mean
          }
        }
        
        if(input$cludesign==2){
          dataHH[,p1:=NULL]
          for (i in 1:100){
            ##  a) SAMPLE THE CLUSTERS
            ##  STAGE 1
            #samp_temp_clu<-dataCLU[,.SD[sample(.N, size_clu)]]
            samp_temp_clu<-ppsDT(dataCLU, n=size_clu, sizevar = "countHH")
            setkeyv(samp_temp_clu, "cluster")
            .GlobalEnv$testSAMPclu<-dataHH
            sampCluHH_st1<-dataHH[samp_temp_clu, nomatch=0]
            ##  STAGE 2
            samp_temp_hh<-sampCluHH_st1[,.SD[sample(.N, size_hh)], by=cluster]
            samp_temp_hh[,pik:=p1*p2]
            testSAMPclu<-samp_temp_hh
            ##  PERSONS
            samp_temp_hh<-samp_temp_hh[,.(hhidg, p1, p2, pik)]
            setkeyv(samp_temp_hh, "hhidg")
            samp_tempPP<-dataPP[samp_temp_hh, nomatch=0]
            ##  ESTIMATION
            samp_mean_prop<-HTestimator(samp_tempPP[employment.status==1, employment.status], samp_tempPP[employment.status==1, pik])
            samp_mean_prop<-samp_mean_prop/pop
            sampMeanExpProp[i]<-samp_mean_prop
            sampMoeExpProp[i]<-abs(samp_mean_prop-true_mean)/true_mean
          }
        }
        
        
        sample_clu$gplot_sample<-samp_tempPP
        sample_clu$sampMean<-mean(sampMeanExpProp)
        sample_clu$sampMOE<-mean(sampMoeExpProp)
        #print(mean(sampMeanExpProp))
        sample_clu$counter<-sample_clu$counter+1
      })
      sample_clu$sampMeanFullProp<-sampMeanExpProp
      sample_clu$sampMeanFullCont<-sampMeanExpCont
      sample_clu$sampMoeFullProp<-sampMoeExpProp
      if (isolate(sample_clu$counter) < maxSim&input$stop2==0){
        invalidateLater(0, session)
      }
      
    }
  }, priority = 0)
  
  ##Storage function for the reactiveValues of the vector of means
  store2<-reactiveValues()
  store2$h<-data.frame(mean=as.numeric(character()))
  store2$moe<-data.frame(moe=as.numeric(character()))
  
  
  ##Generate the message for interruption
  observeEvent(input$stop2, {
    c<-sample_clu$counter*100
    session$sendCustomMessage(type="testmessage", message=list("You have decided to interrupt the simulation, values are shown until simulation number:", c))
    buttonACT2$sim<-input$sim2
    store2$h<-data.frame(mean=as.numeric(character()))
    store2$moe<-data.frame(moe=as.numeric(character()))
    sample_clu$counter<-0
  }) 
  
  
  ##################################################################################
  ##  MAPS
  ##  1. BASE map
  output$pop.hh.map.clu<-renderLeaflet({
    validate(need(mapPopHH(), message = F))
    h<-mapPopHH()
    ##  Create popups
    popup.hh<-paste0(sep= "<br/>", "<b>HHID</b> ", 
                     h$hhidg)
    popup.distr<-paste0(sep= "<br/>", "<b>District</b> ", 
                        eth.shp$NAME_1)
    ##  Select colors
    col_dist<-colorFactor('Spectral', h$distCat)
    col_str<-colorFactor('Spectral', eth.shp$NAME_1)
    
    ##  Create the map
    map<-leaflet() %>%
      addProviderTiles("Esri.WorldImagery", layerId=1,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% 
      addPolygons(data=eth.shp, weight = 1, color="black", fillColor =~col_str(NAME_1), layerId=2,
                  fillOpacity=0.7, popup=popup.distr) %>%
      addMarkers(data = as.data.frame(h), lng=~lon, lat=~lat, popup=popup.hh,
                 clusterOptions=markerClusterOptions()) 
    return(map)
  })
  
  
  ##  2. SAMPLE map  
  observe({
    s<-sample_clu$gplot_sample
    shiny::validate(need(mapPopHH(), message = F),
                    need(exists("s"), message = F))
    isolate({
      h<-mapPopHH()
      h<-data.table(h, key="hhidg")
    })
    s<-data.table(s, key="hhidg")
    if(nrow(s)==0) return(NULL)
    h<-h[s, nomatch=0]
    ##  Create popups
    popup.hh<-paste0(sep= "<br/>", "<b>HHID</b> ", 
                     h$hhidg)
    
    leafletProxy("pop.hh.map.clu")%>%
      clearMarkerClusters() %>%
      addMarkers(data = as.data.frame(h), lng=~lon, lat=~lat, popup=popup.hh,
                 clusterOptions=markerClusterOptions())
  })
  
  ##################################################################################  
  ##  HISTOGRAM SAMPLE
  output$hist_clu<-renderPlotly({
    simu<-buttonACT2$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    isolate({
      h<-as.data.frame(store2$h)
      moe<-as.data.frame(store2$moe)
      p<-mapPop()
    })
    ## Transform the data
    m<-mean(p$employment.status)
    h_new<-as.data.frame(sample_clu$sampMeanFullProp)
    moe_new<-as.data.frame(sample_clu$sampMoeFullProp)
    names(moe_new)<-c("moe")
    moe<-rbind(moe, moe_new)
    h<-rbind(h, h_new)
    if (sample_clu$counter <= maxSim){
      isolate({store2$h<-h})
      isolate({store2$moe<-moe})
    }
    names(h)<-"mean"
    ##  Create the plot
    hist<-ggplot(h, aes(x=mean, ..count../sum(..count..)))+geom_histogram(na.rm = F, binwidth = 0.0001, color="#009FDA")+geom_vline(xintercept = m, color="red", size=1)+ 
      xlab("") + ylab("")+styleMain_noLeg
    hist1<-ggplotly()
    if(exists("hist1")){
      #dev.off()
      return(hist1)
    } else {
      return(NULL) ## THIS procedure is necessary as otherwis plotly exports the NULL and shows error
    }
  })
  
  
  
  
  
  output$tab_clu_sample<-renderDataTable({
    simu<-buttonACT2$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if ((sample_clu$counter) == maxSim|input$stop2==1){
      isolate({
        tab<-data.frame(matrix(nrow=2, ncol = 4))
        names(tab)<-c("Gender", "Employment share", "Age (mean)", "Income (mean)")
        p<-mapPop()
        p<-data.table(p, key="hhidg")
        h<-mapPopHH()
        h<-data.table(h, key="hhidg")
        ### CALCULATE DISTANCES FOR COSTS ->> done in main household loading
        tab[1,1]<-"Male"
        tab[2,1]<-"Female"
        tab[1,2]<-mean(p$employment.status)
        tab[1,3]<-length(h$hhidg)
        
        ##Restrict the sample operations when sample is not NULL
        if(!is.null(sample_clu$gplot_sample)){
          s<-sample_clu$gplot_sample
          s<-data.table(s, key = "hhidg")
          frame<-h[s, nomatch=0]
          sp<-p[s, nomatch=0]
          tab[1,2]<-mean(sp$employment.status[sp$gender=="male"])
          tab[2,2]<-mean(sp$employment.status[sp$gender=="female"])
          tab[1,3]<-mean(sp$age[sp$gender=="male"])
          tab[2,3]<-mean(sp$age[sp$gender=="female"])
          tab[1,4]<-mean(sp$income[sp$gender=="male"])
          tab[2,4]<-mean(sp$income[sp$gender=="female"])
        }
        
      })
      tab
    }
  },
  options=smTab)
  
  output$tab_clu<-renderDataTable({
    simu<-buttonACT2$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if ((sample_clu$counter) == maxSim|input$stop2==1){
      isolate({
        tab<-data.frame(matrix(nrow=3, ncol = 5))
        names(tab)<-c("Stratum", "Number of EAs", "Number of Households", "Number of Persons", "Total Costs")
        p<-mapPop()
        p<-data.table(p, key="hhidg")
        h<-mapPopHH()
        h<-data.table(h, key="hhidg")
        if(is.null(sample_clu$gplot_sample)){
          return()
        }
        else{
          n_hh<-isolate(input$n_clust)
          s<-sample_clu$gplot_sample
          s<-data.table(s, key="hhidg")
          frame<-h[s, nomatch=0]
          frame[,distClu:=dist/n_hh]
          sp<-p[s, nomatch=0]
          tab[1,1]<-"Tigray"
          tab[2,1]<-"Amhara"
          tab[3,1]<-"Oromia"
          tab[1,2]<-length(unique(frame$cluster[frame$stratum=="1"]))
          tab[2,2]<-length(unique(frame$cluster[frame$stratum=="3"]))
          tab[3,2]<-length(unique(frame$cluster[frame$stratum=="4"]))
          tab[1,3]<-length(unique(frame$hhidg[frame$stratum=="1"]))
          tab[2,3]<-length(unique(frame$hhidg[frame$stratum=="3"]))
          tab[3,3]<-length(unique(frame$hhidg[frame$stratum=="4"]))
          tab[1,4]<-round(sum(frame$count[frame$stratum=="1"]))
          tab[2,4]<-round(sum(frame$count[frame$stratum=="3"]))
          tab[3,4]<-round(sum(frame$count[frame$stratum=="4"]))
          tab[1,5]<-(sum(frame$distClu[frame$stratum=="1"]))
          tab[2,5]<-(sum(frame$distClu[frame$stratum=="3"]))
          tab[3,5]<-(sum(frame$distClu[frame$stratum=="4"]))
        }
        clu_name <- switch(as.character(input$cludesign),
          "1" = "Two-Stage Cluster Sampling (SRS-SRS)",
          "2" = "Two-Stage Cluster Sampling (PPS-SRS)",
          "Two-Stage Cluster Sampling"
        )
        clu_key <- switch(as.character(input$cludesign),
          "1" = "clu_srs", "2" = "clu_pps", "clu_pps"
        )
        sim_store[[clu_key]] <- list(
          design = clu_name,
          type = "Cluster",
          n_hh = sum(as.numeric(tab[["Number of Households"]]), na.rm = TRUE),
          n_pers = sum(as.numeric(tab[["Number of Persons"]]), na.rm = TRUE),
          true_val = ifelse(input$sampsiType == 1, mean(p$employment.status), mean(p$income)),
          est_val = mean(sample_clu$sampMeanFullProp, na.rm = TRUE),
          moe = mean(sample_clu$sampMoeFullProp, na.rm = TRUE),
          cost = sum(as.numeric(tab[["Total Costs"]]), na.rm = TRUE),
          eas = sum(as.numeric(tab[["Number of EAs"]]), na.rm = TRUE),
          deff = round(1 + (popStatistics$rho * (as.numeric(input$n_clust) - 1)), 2),
          table = tab,
          sample = frame
        )
        sim_store$last_sample <- frame
        sim_store$last_design <- clu_name
        tab
      })
      
    }
  },
  options=smTab)
  
  ##    Creat sample size table
  output$samplesizeTableClu<-renderDataTable({
    if(input$sampsiType==1){
      tab<-sampsi_tab$tab_prop
      shiny::validate(need(tab, message=F))
      tab<-tab
    }
    if(input$sampsiType==2&!is.null(input$cont_mean)){
      tab<-sampsi_tab$tab_mean
    }
    return(tab)
  },
  options=smTab)
  
  ##################################################################################
  ##  MODAL DIOLOGUE
  ##  A. SLIDES
  ##  1. Introduction
  observeEvent(input$slides5, {
    showModal(modalDialog(
      title = tags$div(
        HTML("<strong><font color='red'><big>Session 5: Clustering<big></font></strong>")),
      renderUI(tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                           src="sess5.pdf")),
      footer = NULL,
      easyClose = TRUE, size = "l"
    ))
  }, suspended = FALSE)
  
  ##  2. PPS
  observeEvent(input$slides6, {
    showModal(modalDialog(
      title = tags$div(
        HTML("<strong><font color='red'><big>Session 6: PPS Sampling<big></font></strong>")),
      renderUI(tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                           src="sess6.pdf")),
      footer = NULL,
      easyClose = TRUE, size = "l"
    ))
  }, suspended = FALSE)
  
  ##  B. OTHER
  observe({
    simu<-buttonACT2$sim
    shiny::validate(
      need(simu, message = F),
      need(mapPopHH(), message = F)
    )
    maxSim<-simu/100
    if (sample_clu$counter == (maxSim-1)|input$stop2==1){
      isolate({
        moe<-store2$moe
        moe<-mean(moe[,1])*100
      })
      showModal(modalDialog(
        title = tags$div(
          HTML("<strong><font color='red'><big>Margin of Error (MOE) (relative)<big></font></strong>")),
        renderText(paste("Your relative Margin of Error is:", round(moe, digits = 3), "%")),
        footer = NULL,
        easyClose = TRUE, size = "s"
      ))
    }
  })
  
  ##################################################################################
  ##  DOWNLOAD THE DATASET
  output$stata_clu <- downloadHandler(
    filename = function() {
      paste("clu_sample", Sys.Date(), '.dta', sep='')
    },
    content = function(file) {
      sample<-data.table(sample_clu$gplot_sample, key="hhidg")
      data<-data.table(mapPopHH(), key="hhidg")
      data<-data[sample, nomatch=0]
      save.dta13(data, file, convert.underscore = T)
    }
  )

  ##################################################################################
  ##  SUMMARY REPORT & EXPORT
  ##################################################################################

  get_summary_table <- reactive({
    input$summary
    sim_keys <- c("srs", "str_eq", "str_prop", "str_ney", "str_opt", "clu_srs", "clu_pps")
    results <- list()
    for (k in sim_keys) {
      item <- sim_store[[k]]
      if (!is.null(item)) {
        is_prop <- (input$sampsiType == 1)
        true_str <- if (is_prop) paste0(round(item$true_val * 100, 2), "%") else as.character(round(item$true_val, 1))
        est_str <- if (is_prop) paste0(round(item$est_val * 100, 2), "%") else as.character(round(item$est_val, 1))
        moe_str <- paste0(round(item$moe * 100, 2), "%")
        bias_pct <- paste0(round(abs(item$est_val - item$true_val) / item$true_val * 100, 2), "%")
        cost_str <- paste0("$", formatC(round(item$cost), format = "d", big.mark = ","))
        
        results[[length(results) + 1]] <- data.frame(
          `Design` = item$design,
          `Sample HHs` = item$n_hh,
          `Sample Persons` = item$n_pers,
          `EAs (Clusters)` = item$eas,
          `True Value` = true_str,
          `Simulated Estimate` = est_str,
          `Relative Error` = bias_pct,
          `Achieved MOE` = moe_str,
          `Total Travel Cost` = cost_str,
          `Status` = "Simulated",
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }
    
    if (length(results) == 0) {
      is_prop <- (input$sampsiType == 1)
      srs_n <- as.numeric(input$sampSizeFinal)
      avhh <- as.numeric(input$avhhsize)
      deff <- popStatistics$deff
      n_strata <- popStatistics$N_stratum
      
      p_data <- mapPop()
      h_data <- mapPopHH()
      true_val <- if (is_prop) mean(p_data$employment.status, na.rm = TRUE) else mean(p_data$income, na.rm = TRUE)
      true_str <- if (is_prop) paste0(round(true_val * 100, 2), "%") else as.character(round(true_val, 1))
      target_moe_str <- paste0(round(as.numeric(input$precision), 1), "%")
      avg_dist <- mean(h_data$dist, na.rm = TRUE)
      
      results[[1]] <- data.frame(
        `Design` = "Simple Random Sampling (SRS)",
        `Sample HHs` = srs_n,
        `Sample Persons` = round(srs_n * avhh),
        `EAs (Clusters)` = srs_n,
        `True Value` = true_str,
        `Simulated Estimate` = "Pending simulation",
        `Relative Error` = "-",
        `Achieved MOE` = target_moe_str,
        `Total Travel Cost` = paste0("$", formatC(round(srs_n * avg_dist), format = "d", big.mark = ",")),
        `Status` = "Theoretical Target",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      results[[2]] <- data.frame(
        `Design` = "Stratified SRS (Equal Allocation across Strata)",
        `Sample HHs` = srs_n * n_strata,
        `Sample Persons` = round(srs_n * n_strata * avhh),
        `EAs (Clusters)` = srs_n * n_strata,
        `True Value` = true_str,
        `Simulated Estimate` = "Pending simulation",
        `Relative Error` = "-",
        `Achieved MOE` = target_moe_str,
        `Total Travel Cost` = paste0("$", formatC(round(srs_n * n_strata * avg_dist), format = "d", big.mark = ",")),
        `Status` = "Theoretical Target",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      results[[3]] <- data.frame(
        `Design` = "Two-Stage Cluster Sampling (with DEFF)",
        `Sample HHs` = ceiling(srs_n * deff),
        `Sample Persons` = ceiling(srs_n * deff * avhh),
        `EAs (Clusters)` = ceiling((srs_n * deff) / 10),
        `True Value` = true_str,
        `Simulated Estimate` = "Pending simulation",
        `Relative Error` = "-",
        `Achieved MOE` = target_moe_str,
        `Total Travel Cost` = paste0("$", formatC(round(ceiling(srs_n * deff) * (avg_dist / 10)), format = "d", big.mark = ",")),
        `Status` = "Theoretical Target",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }
    
    do.call(rbind, results)
  })

  get_stratum_table <- reactive({
    input$summary
    sim_keys <- c("clu_pps", "clu_srs", "str_opt", "str_ney", "str_prop", "str_eq", "srs")
    for (k in sim_keys) {
      item <- sim_store[[k]]
      if (!is.null(item) && !is.null(item$table)) {
        tab <- item$table
        tab$Design <- item$design
        return(tab[, c("Design", setdiff(names(tab), "Design"))])
      }
    }
    
    h_data <- mapPopHH()
    tab <- data.frame(
      Stratum = c("Tigray", "Amhara", "Oromia"),
      `Number of EAs` = c(
        length(unique(h_data$cluster[h_data$stratum == "1"])),
        length(unique(h_data$cluster[h_data$stratum == "3"])),
        length(unique(h_data$cluster[h_data$stratum == "4"]))
      ),
      `Number of Households` = c(
        length(unique(h_data$hhidg[h_data$stratum == "1"])),
        length(unique(h_data$hhidg[h_data$stratum == "3"])),
        length(unique(h_data$hhidg[h_data$stratum == "4"]))
      ),
      `Number of Persons` = c(
        round(sum(h_data$count[h_data$stratum == "1"]) * (30000 / sum(h_data$count))),
        round(sum(h_data$count[h_data$stratum == "3"]) * (30000 / sum(h_data$count))),
        round(sum(h_data$count[h_data$stratum == "4"]) * (30000 / sum(h_data$count)))
      ),
      `Total Costs` = c(
        round(sum(h_data$dist[h_data$stratum == "1"])),
        round(sum(h_data$dist[h_data$stratum == "3"])),
        round(sum(h_data$dist[h_data$stratum == "4"]))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    tab$Design <- "Population Sampling Frame"
    tab[, c("Design", setdiff(names(tab), "Design"))]
  })

  output$tab_summary <- DT::renderDT({
    get_summary_table()
  }, options = list(dom = "t", paging = FALSE, ordering = FALSE), rownames = FALSE)

  output$tab_summary_stratum <- DT::renderDT({
    get_stratum_table()
  }, options = list(dom = "t", paging = FALSE, ordering = FALSE), rownames = FALSE)

  output$summary_narrative <- renderUI({
    is_prop <- (input$sampsiType == 1)
    target_ind <- ifelse(is_prop, "Individual Employment Status (Binary Proportion)", "Annual Household Income (Continuous)")
    p_data <- mapPop()
    true_val <- if (is_prop) mean(p_data$employment.status, na.rm = TRUE) else mean(p_data$income, na.rm = TRUE)
    true_str <- if (is_prop) paste0(round(true_val * 100, 2), "%") else paste0("$", formatC(round(true_val), format = "d", big.mark = ","))
    target_moe <- paste0(round(as.numeric(input$precision), 1), "%")
    deff_val <- popStatistics$deff
    
    sim_count <- sum(!vapply(c("srs", "str_eq", "str_prop", "str_ney", "str_opt", "clu_srs", "clu_pps"), function(k) is.null(sim_store[[k]]), logical(1)))
    
    tags$div(
      style = "line-height: 1.6; font-size: 15px; color: #333;",
      tags$div(
        style = "background-color: #F4F8FB; border-left: 5px solid #002244; padding: 15px; margin-bottom: 20px; border-radius: 4px;",
        tags$h4(style = "color: #002244; margin-top: 0;", "1. Training Objective & Study Context"),
        tags$p(
          "The LSMS Sampling Trainer demonstrates the foundational principles of survey sampling in developing country contexts, utilizing a synthetic population modeled after Ethiopia. The primary goal is to guide survey practitioners in selecting an optimal sample design that balances statistical precision against field operational constraints, travel logistics, and survey budget."
        )
      ),
      tags$div(
        style = "background-color: #FFFFFF; border: 1px solid #E0E0E0; padding: 15px; margin-bottom: 20px; border-radius: 4px;",
        tags$h4(style = "color: #002244; margin-top: 0;", "2. Baseline Target Parameters"),
        tags$ul(
          tags$li(tags$b("Target Indicator: "), target_ind),
          tags$li(tags$b("Population True Value: "), true_str),
          tags$li(tags$b("Target Margin of Error: "), paste0(target_moe, " (relative precision at 95% confidence)")),
          tags$li(tags$b("Recommended Base Sample Size: "), paste0(input$sampSizeFinal, " households")),
          tags$li(tags$b("Clustering Design Effect (DEFF): "), paste0(deff_val, " (reflecting intracluster correlation ICC = 0.60)"))
        )
      ),
      tags$div(
        style = "background-color: #F9FBFD; border-left: 5px solid #009FDA; padding: 15px; margin-bottom: 20px; border-radius: 4px;",
        tags$h4(style = "color: #002244; margin-top: 0;", "3. Sampling Diagnostics & Design Trade-offs"),
        tags$p(
          tags$b("Simple Random Sampling (SRS): "),
          "SRS provides an unbiased estimate with maximum theoretical variance efficiency per sampled unit. However, because households are chosen uniformly across the entire geographical territory, field travel costs are the highest among all designs ($", 
          formatC(round(input$sampSizeFinal * mean(mapPopHH()$dist, na.rm = TRUE)), format = "d", big.mark = ","), " estimated)."
        ),
        tags$p(
          tags$b("Stratified Sampling: "),
          "Stratification divides the population into distinct regional domains (Tigray, Amhara, and Oromia). Equal allocation ensures uniform precision for regional reporting, whereas Neyman and Optimal allocations yield lower nationwide sampling variance and account for differential unit travel distances."
        ),
        tags$p(
          tags$b("Two-Stage Cluster Sampling: "),
          "Cluster sampling groups interviews into Primary Sampling Units (EAs), substantially reducing enumerator travel distances and administrative costs. However, due to household homogeneity within clusters (ICC = 0.60), the design incurs a design effect penalty (DEFF = ", deff_val, "), requiring an expanded sample size to achieve the same target precision as SRS."
        ),
        tags$p(
          tags$b("Simulation Status: "),
          if (sim_count > 0) {
            paste0("A total of ", sim_count, " simulation(s) have been completed. Empirical estimates closely track the synthetic population benchmark.")
          } else {
            "Simulations have not yet been executed in this session. Default theoretical targets are displayed above. Run simulations in the design tabs to view empirical distributions."
          }
        )
      )
    )
  })

  output$summary_map <- leaflet::renderLeaflet({
    input$summary
    h <- mapPopHH()
    eth.shp <- ETHSHP()
    
    # Use the most recent sample if available
    if (!is.null(sim_store$last_sample)) {
      s <- data.table::data.table(sim_store$last_sample, key = "hhidg")
      h <- h[s, nomatch = 0]
    } else {
      # Show a representative subset for preview
      h <- h[1:min(500, nrow(h)), ]
    }

    col_str <- leaflet::colorFactor("Spectral", eth.shp$NAME_1)
    
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldImagery", layerId = 1, options = leaflet::providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addPolygons(data = eth.shp, weight = 1.5, color = "#002244", fillColor = ~col_str(NAME_1), fillOpacity = 0.35, layerId = 2) %>%
      leaflet::addMarkers(data = as.data.frame(h), lng = ~lon, lat = ~lat,
                          popup = paste0("<b>Household ID:</b> ", h$hhidg, "<br/><b>Stratum:</b> ", h$stratum),
                          clusterOptions = leaflet::markerClusterOptions())
  })

  output$download_word <- shiny::downloadHandler(
    filename = function() {
      paste("LSMS_Sampling_Report_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      doc <- officer::read_docx()
      
      # 1. World Bank Logo
      logo_path <- system.file("www", "logoWBDG.png", package = "lsmssamptrain")
      if (file.exists(logo_path)) {
        doc <- officer::body_add_img(doc, src = logo_path, width = 2.0, height = 2.0, style = "centered")
      }
      
      # 2. Document Title & Header
      doc <- officer::body_add_par(doc, "LSMS Sampling Trainer Report", style = "heading 1")
      doc <- officer::body_add_par(doc, "Survey Design Simulation, Diagnostics & Comparative Evaluation", style = "heading 2")
      doc <- officer::body_add_par(doc, paste("World Bank Living Standards Measurement Study (LSMS) | Date:", Sys.Date()), style = "Normal")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 3. Executive Summary Narrative
      doc <- officer::body_add_par(doc, "1. Executive Summary & Purpose", style = "heading 2")
      doc <- officer::body_add_par(doc, "This report provides a formal synthesis of survey sampling designs simulated using the LSMS Sampling Trainer. The application is built upon the World Bank LSMS Household Survey Sampling Manual to train practitioners in balancing statistical accuracy against field travel logistics and operational survey budgets.", style = "Normal")
      doc <- officer::body_add_par(doc, "Using a synthetic population of Ethiopia spanning three administrative regions (Tigray, Amhara, and Oromia), this report evaluates the statistical performance, achieved precision (Margin of Error), and cost efficiency of Simple Random Sampling (SRS), Stratified Random Sampling, and Two-Stage Cluster Sampling.", style = "Normal")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 4. Survey Design Parameters
      is_prop <- (input$sampsiType == 1)
      p_data <- mapPop()
      true_val <- if (is_prop) mean(p_data$employment.status, na.rm = TRUE) else mean(p_data$income, na.rm = TRUE)
      true_str <- if (is_prop) paste0(round(true_val * 100, 2), "%") else paste0("$", formatC(round(true_val), format = "d", big.mark = ","))
      
      param_df <- data.frame(
        `Survey Parameter` = c(
          "Target Indicator",
          "Synthetic Population Size",
          "Population True Value",
          "Target Margin of Error (Relative)",
          "Confidence Level",
          "Average Household Size",
          "Target Population Eligibility Share",
          "Calculated Base Sample Size (SRS)",
          "Cluster Design Effect (DEFF)"
        ),
        `Value` = c(
          ifelse(is_prop, "Individual Employment Status (Proportion)", "Annual Household Income (Continuous)"),
          "30,000 Households / ~150,000 Individuals",
          true_str,
          paste0(round(as.numeric(input$precision), 1), "%"),
          "95% (z = 1.96)",
          paste0(input$avhhsize, " persons/HH"),
          paste0(round(as.numeric(input$share) * 100, 1), "%"),
          paste0(input$sampSizeFinal, " households"),
          as.character(popStatistics$deff)
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      doc <- officer::body_add_par(doc, "2. Survey Design Parameters & Targets", style = "heading 2")
      ft_param <- flextable::flextable(param_df)
      ft_param <- flextable::theme_vanilla(ft_param)
      ft_param <- flextable::bg(ft_param, bg = "#002244", part = "header")
      ft_param <- flextable::color(ft_param, color = "white", part = "header")
      ft_param <- flextable::autofit(ft_param)
      doc <- flextable::body_add_flextable(doc, value = ft_param)
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 5. Comparative Performance Table
      doc <- officer::body_add_par(doc, "3. Comparative Sampling Design & Simulation Results", style = "heading 2")
      summary_df <- get_summary_table()
      ft_sum <- flextable::flextable(summary_df)
      ft_sum <- flextable::theme_vanilla(ft_sum)
      ft_sum <- flextable::bg(ft_sum, bg = "#002244", part = "header")
      ft_sum <- flextable::color(ft_sum, color = "white", part = "header")
      ft_sum <- flextable::autofit(ft_sum)
      doc <- flextable::body_add_flextable(doc, value = ft_sum)
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 6. Stratum Breakdown Table
      doc <- officer::body_add_par(doc, "4. Regional Stratum Sample Allocation", style = "heading 2")
      stratum_df <- get_stratum_table()
      ft_strat <- flextable::flextable(stratum_df)
      ft_strat <- flextable::theme_vanilla(ft_strat)
      ft_strat <- flextable::bg(ft_strat, bg = "#002244", part = "header")
      ft_strat <- flextable::color(ft_strat, color = "white", part = "header")
      ft_strat <- flextable::autofit(ft_strat)
      doc <- flextable::body_add_flextable(doc, value = ft_strat)
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 7. Spatial Distribution Map Image
      doc <- officer::body_add_par(doc, "5. Spatial Sample Distribution", style = "heading 2")
      eth.shp <- ETHSHP()
      h_map <- if (!is.null(sim_store$last_sample)) {
        sim_store$last_sample
      } else {
        mapPopHH()[1:min(500, nrow(mapPopHH())), ]
      }
      
      p_map <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = eth.shp, fill = "#F5F8FA", color = "#002244", linewidth = 0.5) +
        ggplot2::geom_point(data = as.data.frame(h_map), ggplot2::aes(x = lon, y = lat, color = stratum), alpha = 0.65, size = 1.6) +
        ggplot2::scale_color_manual(values = c("1" = "#002244", "3" = "#009FDA", "4" = "#B73338"),
                                    labels = c("1" = "Tigray", "3" = "Amhara", "4" = "Oromia"),
                                    name = "Region / Stratum") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Geographical Distribution of Sampled Households",
                      subtitle = "Synthetic Population of Ethiopia",
                      x = "Longitude", y = "Latitude") +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "#002244", face = "bold", size = 13),
                       plot.subtitle = ggplot2::element_text(color = "#555555", size = 10))
      
      img_file <- tempfile(fileext = ".png")
      ggplot2::ggsave(img_file, p_map, width = 6.2, height = 4.6, dpi = 180)
      doc <- officer::body_add_img(doc, src = img_file, width = 5.8, height = 4.3, style = "centered")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      
      # 8. Diagnostics & Methodological Discussion
      doc <- officer::body_add_par(doc, "6. Methodological Diagnostics & Discussion", style = "heading 2")
      doc <- officer::body_add_par(doc, "Field Travel Costs vs. Statistical Precision:", style = "heading 3")
      doc <- officer::body_add_par(doc, "Simple Random Sampling uniformly scatters households across all enumeration areas in the country. While this produces optimal statistical variance, field operational costs are prohibitive because interviewers must travel to isolated dwellings across vast distances. In contrast, cluster sampling groups interviews into Primary Sampling Units, drastically curtailing enumerator transit distances and fieldwork expenditures.", style = "Normal")
      
      doc <- officer::body_add_par(doc, "Impact of Clustering & Design Effect (DEFF):", style = "heading 3")
      doc <- officer::body_add_par(doc, paste0("Because households located within the same village or enumeration area tend to exhibit correlated socio-economic characteristics (measured by the intracluster correlation ICC = 0.60), each additional household in a cluster contributes less independent information than an unclustered household. This penalty is quantified by the Design Effect (DEFF = ", popStatistics$deff, "). To achieve the target margin of error, the survey team must expand the overall sample size proportionally by DEFF."), style = "Normal")
      
      doc <- officer::body_add_par(doc, "Stratification & Domain Reporting:", style = "heading 3")
      doc <- officer::body_add_par(doc, "Stratified random sampling ensures that each sub-national domain (Tigray, Amhara, and Oromia) has sufficient statistical power for independent reporting. Proportional allocation reflects true national demographics, while Neyman allocation distributes sample weights according to stratum heterogeneity, and Optimal allocation balances heterogeneity against regional travel unit costs.", style = "Normal")
      
      doc <- officer::body_add_par(doc, "Policy & Operational Recommendations:", style = "heading 3")
      doc <- officer::body_add_par(doc, "1. For national household surveys with budget constraints, two-stage cluster sampling with Probability Proportional to Size (PPS) selection of clusters is strongly recommended.", style = "Normal")
      doc <- officer::body_add_par(doc, "2. When regional policy requires equally reliable statistics across administrative domains, equal or Neyman allocation should be used across strata.", style = "Normal")
      doc <- officer::body_add_par(doc, "3. Survey budgets must explicitly account for the cluster DEFF multiplier during initial sample size determination.", style = "Normal")
      
      print(doc, target = file)
    }
  )

}
