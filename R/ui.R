#` Shiny UI LSMS Sampling Trainer Application
#'
#'
#'
#' @keywords internal
#' @noRd


main_ui<-function(request){

  # styles
  infoButtStyle <- "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%;
                padding-top: 1px; padding-bottom: 2px;padding-left: 2px;padding-right: 2px;display:block;
                height: 20px; width: 20px; border-radius: 50%"

  fpwww <- system.file("www", package = "lsmssamptrain")
  style<-sass::sass(sass::sass_file(file.path(fpwww, "styles.scss")))
  fluidPage(
    shiny::tags$head(
      shiny::tags$style((style))
    ),
    shiny::tags$header(
      style = "padding-bottom: 0px; background-color: #002244; color: white; text-align: center; height: 5vh",
      shiny::div(
        style = "float: left;",
        shiny::img(src = file.path("www", "logoWBDG.png"), height = "63vh")  # Adjust image path and size
      ),
      shiny::h2("LSMS Sampling Trainer Application", style = "margin-left: 60px;")  # Adjust margin to align with your image
    ),
    waiter::use_waiter(),
    navbarPage("LSMS",
               tabPanel(
                 "Population and Sample Size",
                 sidebarPanel(
                   fluidPage(
                     fluidRow(
                       column(
                         12, h3("Population and sample size"),
                         radioButtons("sampsiType", "Please select the type of variable for your sample size calculation",
                                      c("Discrete (e.g Unemployment rate)" = 1, "Continous (e.g. Income)" = 2),
                                      selected = 1, inline = T
                         ),
                         br()
                       )
                     ),
                     conditionalPanel(
                       condition = "input.sampsiType==1",
                       ##  1. Proportions
                       fluidRow(
                         column(
                           12,
                           actionButton("infoProp", "",
                                        icon = icon("info-circle"),
                                        style = infoButtStyle
                           ),
                           sliderInput("prop", "Please select the proportion you want to estimate.", value = 0.25, min = 0.01, max = 0.5, step = 0.01)
                         )
                       )
                     ),
                     conditionalPanel(
                       condition = "input.sampsiType==2",
                       ##  3. Averages
                       fluidRow(
                         column(
                           6,
                           actionButton("infoAver", "",
                                        icon = icon("info-circle"),
                                        style = infoButtStyle
                           ),
                           numericInput("cont_mean", "Please specify the mean of the variable.",
                                        value = 1400, min = 100, max = 1000000, step = 100
                           )
                         ),
                         column(
                           6,
                           actionButton("infoSD", "",
                                        icon = icon("info-circle"),
                                        style = infoButtStyle
                           ),
                           numericInput("cont_sd", "Please specify the SD of the variable.",
                                        value = 700, min = 100, max = 1000000, step = 50
                           )
                         )
                       )
                     ),
                     ##  2. Eligebility
                     fluidRow(
                       column(
                         12,
                         actionButton("infoElig", "",
                                      icon = icon("info-circle"),
                                      style = infoButtStyle
                         ),
                         sliderInput("share", "Please select the share of the population eligible for your survey.", value = 1, min = 0.01, max = 1, step = 0.01)
                       )
                     ),

                     # selectInput("conf", "Please specify the confidence intervall", c("99%"=0.99, "95%"=0.95, "90%"=0.90), selected=0.95),
                     # helpText("The Confidence interval has a very different meaning in survey sampling.A confidence interval of 95% tells you,
                     #        that if we draw the according sample size and the sampling design infinitly many time, our estimate would fall 95% of
                     #         the time in the respective interval"),
                     ##  4. MoE
                     fluidRow(
                       column(
                         6,
                         actionButton("infoHHsize", "",
                                      icon = icon("info-circle"),
                                      style = infoButtStyle
                         ),
                         selectInput("avhhsize", "Please specify average household size", c(
                           "1" = 1, "2" = 2, "3" = 3,
                           "4" = 4, "5" = 5, "6" = 6,
                           "7" = 7, "8" = 8, "9" = 9,
                           "10" = 10, "11" = 11, "12" = 12
                         ), selected = 6)
                       ),
                       column(
                         6,
                         actionButton("infoMoe", "",
                                      icon = icon("info-circle"),
                                      style = infoButtStyle
                         ),
                         # selectInput("precision", "Please specify the desired precision",
                         #             c("rel. MoE=1%" = 0.01, "rel. MoE=5%" = 0.05, "rel. MoE=10%" = 0.1), selected = 0.05)
                         sliderInput("precision",
                                     "Please specify the desired precision",
                                     value = 5,
                                     min = 1,
                                     max = 10,
                                     step = 1, post = "%")
                       )
                     ),
                     ##  5. Sample Size Table
                     h4("Sample Size Overview and Selection"),
                     # helpText("Options depend on selected tab. Click on the row, to select the sample size"),
                     # DT::dataTableOutput("samplesizeTable"),
                     ##  6. Advanced Panel Controls
                     # radioButtons("advanced", "Use Advanced Population Controls", c("No"=2), selected=2, inline = T),
                     #  helpText("Advanced population controls allow you to select your own population size, however as the population size is build up from a survey
                     # dataset this may take quite some time."),
                     fluidRow(
                       column(
                         8
                         # radioButtons("alloc", "Type of DOMAIN allocation",
                         #              c("Equal"=1, "Proportional"=2, "Neyman"=3, "Optimal"=4), selected = 1, inline = T)
                       ),
                       column(
                         4,
                         radioButtons("cludesign", "Type of PSU sampling", c("SRS" = 1, "PPS" = 2), selected = 1, inline = T)
                       )
                     ),
                     fluidRow(
                       column(4),
                       column(
                         4,
                         numericInput("n_clust", "Households per cluster?", 10, min = 8, max = 40)
                       ),
                       column(
                         4,
                         numericInput("sampSizeFinal", "Recommended Sample Size:",
                                      value = 1000,
                                      min = 100,
                                      max = Inf,
                                      step = 1
                         )
                       )
                     )
                     # selectInput("country", "Select the country", country, selected = "Ethiopia"),
                     # sliderInput("pop.size", "Please select the size of the Population", min = 30000, max=500000, step=30000, value = 30000),
                     # helpText("Please select the size of the population. The higher this value, the longer your simulation will take.")
                   )
                 ),
                 mainPanel(
                   fluidPage(
                     fluidRow(
                       column(
                         10, br(),
                         conditionalPanel(
                           "input.popCharDisplay==2&input.pop=='Graph/Table'",
                           h3("Sampling distribution (Proportions)")
                         ),
                         conditionalPanel(
                           "input.popCharDisplay==1|input.pop=='Map'",
                           h3("Population Overview")
                         )
                       ),
                       ##  FOR later: background-image: url('stata_logo.gif');
                       column(2, downloadButton("stata_pop", "Download CSV Dataset",
                                                style = "
                                                          color: #fff;
                                                          background-color: #002244;
                                                          border-color: #008e00;
                                                          font-face: bold;
                                                          font-size:100%"
                       ))
                     ),
                     fluidRow(
                       tabBox(
                         width = 11, id = "pop", height = "700px",
                         tabItem("Tab1",
                                 title = "Graph/Table",
                                 radioButtons("popCharDisplay", "", c("Table" = 1, "Graph (Sampling Distribution)" = 2),
                                              selected = 2, inline = T
                                 ),
                                 conditionalPanel(
                                   "input.popCharDisplay==1",
                                   fluidRow(
                                     column(
                                       8,
                                       baseTableUI("baseTable")
                                     ),
                                     column(
                                       4,
                                       h3("Slides"),
                                       actionButton("slides1", "Slides Session 1 - Intro",
                                                    icon = NULL, width = "100%",
                                                    style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                                       ), br(), br(),
                                       actionButton("slides2", "Slides Session 2 - Sampling proportions",
                                                    icon = NULL, width = "100%",
                                                    style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                                       ), br(), br(),
                                       actionButton("slides3", "Slides Session 3 - Sampling averages",
                                                    icon = NULL, width = "100%",
                                                    style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                                       )
                                     ),
                                     fluidRow(
                                       column(
                                         7,
                                         #DT::dataTableOutput("frameTable", width = "50%", height = "100px")
                                         frameTableUI("frameTable")
                                       )
                                     )
                                   )
                                 ),
                                 conditionalPanel(
                                   "input.popCharDisplay==2",
                                   plotly::plotlyOutput("dens_plot_start", width = "100%", height = "500px")
                                 )
                         ),
                         tabItem("Tab2",
                                 title = "Map",
                                 # leaflet::leafletOutput("pop.hh.map", width = "100%", height = "700px")
                                 pop.hh.mapUI("pop.hh.map")
                         )
                       )
                     ),
                     fluidRow(
                       column(8),
                       column(2),
                       column(2)
                     )
                   )
                 )
               ),
               tabPanel(
                 "Simple random Sampling",
                 sidebarPanel(
                   h3("Simulation Controls"),
                   numericInput("sim", "Select the number of times you want to repeat the simulation", 100,
                                step = 100,
                                min = 100, max = 1000
                   ),
                   helpText("The maximum number of draws is 1000. This means 1000 samples are drawn from the underlying population,
                                                    and their means are plotted in the bar chart to the right. What you see there is called the
                                                    sampling distribution. The difference between the true mean (red line) and the estimate each sample
                                                    produces is the RMSE, it is defined as: MSE=Bias+Var(y)."),
                   actionButton("generate", "Start the simulation",
                                icon("play"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                   ),
                   br(),
                   br(),
                   actionButton("stop", "Interrupt",
                                icon("stop"),
                                style = "color: #fff; background-color: #b73338; border-color: #b73338"
                   ),
                   br(), br(),
                   h4("Sample Size Overview and Selection"),
                   helpText("Options depend on selected tab. Click on the row, to select the sample size"),
                   DT::dataTableOutput("samplesizeTable_srs"),
                   br(),
                   br(),
                   br()
                 ),
                 mainPanel(
                   fluidPage(
                     fluidRow(
                       column(
                         10,
                         h3("Simulation Monitor")
                       ),
                       column(2, downloadButton("stata_srs", "Download CSV Dataset",
                                                style = "background-image: url('www/stata_logo.gif');
                                                                 color: #fff;
                                                                 background-color: #002244;
                                                                 border-color: #008e00;
                                                                 font-size:100%"
                       ))
                     ),
                     fluidRow(
                       column(
                         7, radioButtons("simuRes", "", c("Table" = 1, "Graph" = 2),
                                         selected = 2, inline = T
                         ),
                         conditionalPanel(
                           "input.simuRes==1",
                           dataTableOutput("tab_srs_sample")
                         ),
                         conditionalPanel(
                           "input.simuRes==2",
                           plotly::plotlyOutput("hist_srs")
                         ),
                         br(),
                         br(),
                         br(),
                         dataTableOutput("tab_srs"),
                         singleton(
                           tags$head(tags$script(src = "message-handler.js"))
                         )
                       ),
                       column(
                         5,
                         leafletOutput("pop.hh.map.srs", width = "100%", height = "550px")
                       )
                     ),
                     fluidRow(br()),
                     fluidRow()
                   )
                 )
               ),
               tabPanel(
                 "Stratification",
                 sidebarPanel(
                   h3("Simulation Controls"),
                   numericInput("sim1", "Select the number of times you want to repeat the simulation", 1,
                                min = 1, max = 1000
                   ),
                   helpText("The maximum number of draws is 1000. This means 1000 samples are drawn from the underlying population, and their means are plotted in the
                                       bar chart to the right. What you see there is called the sampling distribution"),
                   numericInput("budget", "Please specify the maximum available survey budget", 150000,
                                min = 1, max = 100000
                   ),
                   helpText("The maximum budget is needed for the optimal allocation mode. Currently 1 km refers to 1 USD"),
                   radioButtons("alloc", "Select the type of stratum allocation",
                                c("Equal" = 1, "Proportional" = 2, "Neyman" = 3, "Optimal" = 4),
                                selected = 1, inline = T
                   ),
                   actionButton("generate1", "Start the simulation",
                                icon("stop"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                   ),
                   br(),
                   br(),
                   actionButton("stop1", "Interrupt/Reset",
                                icon("stop"),
                                style = "color: #fff; background-color: #b73338; border-color: #b73338"
                   ),
                   br(), br(),
                   h4("Sample Size Overview and Selection"),
                   helpText("Options depend on selected tab. Click on the row, to select the sample size"),
                   DT::dataTableOutput("samplesizeTable_str"),
                   br(),
                   br(),
                   br()
                 ),
                 mainPanel(
                   fluidPage(
                     fluidRow(
                       column(
                         10,
                         h3("Simulation Monitor")
                       ),
                       column(2, downloadButton("stata_str", "Download CSV Dataset",
                                                style = "background-image: url('www/stata_logo.gif');
                                                                 color: #fff;
                                                                 background-color: #002244;
                                                                 border-color: #008e00;
                                                                 font-size:100%"
                       ))
                     ),
                     fluidRow(
                       column(
                         7, radioButtons("simuRes1", "", c("Table" = 1, "Graph" = 2),
                                         selected = 2, inline = T
                         ),
                         conditionalPanel(
                           "input.simuRes1==2",
                           plotly::plotlyOutput("hist_str")
                         ),
                         conditionalPanel(
                           "input.simuRes1==1",
                           dataTableOutput("tab_str_sample")
                         ),
                         br(),
                         br(),
                         br()
                       ),
                       column(
                         5,
                         leafletOutput("pop.hh.map.strsrs", width = "100%", height = "550px")
                       )
                     ),
                     fluidRow(
                       column(
                         7,
                         dataTableOutput("tab_str")
                       ),
                       column(
                         5, br(),
                         h3("Slides"),
                         actionButton("slides4", "Slides Session 4 - Stratification",
                                      icon = NULL, width = "100%",
                                      style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                         )
                       )
                     )
                   )
                 )
               ),
               tabPanel(
                 "Clustering",
                 sidebarPanel(
                   h3("Simulation Controls"),
                   numericInput("sim2", "Select the number of times you want to repeat the simulation", 1,
                                min = 1, max = 1000
                   ),
                   helpText("The maximum number of draws is 1000 You have seen the sampling distribution already, in this part of the simulator we will
                              only present you the result tables"),
                   numericInput("n_clust", "How many units are you planning to sample per cluster?", 10, min = 8, max = 40),
                   radioButtons("cludesign", "Please select the type of cluster sampling", c("SRS" = 1, "PPS" = 2), selected = 1, inline = T),
                   actionButton("generate2", "Start the simulation",
                                icon("stop"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                   ),
                   br(), br(),
                   actionButton("stop2", "Interrupt/Reset",
                                icon("stop"),
                                style = "color: #fff; background-color: #b73338; border-color: #b73338"
                   ),
                   br(), br(),
                   h4("Sample Size Overview"),
                   dataTableOutput("samplesizeTableClu"),
                   ##  6. Advanced Panel Controls
                   radioButtons("advanced1", "Use Advanced Population Controls", c("No" = 2), selected = 2, inline = T),
                   helpText("Advanced population controls allow you to select your own population size, however as the population size is build up from a survey
                                         dataset this may take quite some time."),
                   conditionalPanel(
                     "input.advanced1==1",
                     br(),
                     br(),
                     sliderInput("iccmod", "Please select the degree of ICC (low, moderate, high, very high)",
                                 value = 2,
                                 min = 1, max = 4, step = 1
                     ),
                     actionButton("generate_icc", "Re-generate population with new ICC",
                                  icon("stop"),
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     )
                   )
                 ),
                 mainPanel(
                   fluidPage(
                     fluidRow(
                       column(
                         10,
                         h3("Simulation Monitor")
                       ),
                       column(2, downloadButton("stata_clu", "Download CSV Dataset",
                                                style = "background-image: url('www/stata_logo.gif');
                                                          color: #fff;
                                                          background-color: #002244;
                                                          border-color: #008e00;
                                                          font-size:100%"
                       ))
                     ),
                     fluidRow(
                       column(
                         7,
                         radioButtons("simuRes2", "", c("Table" = 1, "Graph" = 2),
                                      selected = 2, inline = T
                         ),
                         conditionalPanel(
                           "input.simuRes2==2",
                           plotly::plotlyOutput("hist_clu")
                         ),
                         conditionalPanel(
                           "input.simuRes2==1",
                           dataTableOutput("tab_clu_sample")
                         ),
                         br(),
                         br(),
                         br()
                       ),
                       column(
                         5,
                         leafletOutput("pop.hh.map.clu", width = "100%", height = "550px")
                       )
                     ),
                     fluidRow(
                       column(
                         7,
                         dataTableOutput("tab_clu")
                       ),
                       column(
                         5,
                         br(),
                         h3("Slides"),
                         actionButton("slides5", "Slides Session 5 - Clustering",
                                      icon = NULL, width = "100%",
                                      style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                         ), br(), br(),
                         actionButton("slides6", "Slides Session 6 - PPS sampling",
                                      icon = NULL, width = "100%",
                                      style = "color: #fff; background-color: #002244; border-color: #008e00; font-size:100%"
                         )
                       )
                     )
                   )
                 )
               )
    )

    # sidebarLayout(
    #   sidebarPanel(
    #     moduleUI1("mymodule") # Use the module UI here
    #   ),
    #   mainPanel(
    #     moduleUI2("mymodule") # Use the table UI here
    #   )
    # )
  )
}
