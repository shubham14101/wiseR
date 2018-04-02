library('bnlearn')
library('networkD3')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
library('shinyWidgets')
library('missRanger')
library('tools')
library('shinyalert')
library('shinycssloaders')
library('rintrojs')
library('arules')
library('rcompanion')
library('psych')
library('DescTools')
library("DT")
library("linkcomm")
library('igraph')
library("parallel")
library("snow")
library("shinyBS")
source('error.bar.R')
source('graph.custom.R')
source('graph.custom.assoc.R')
source('custom.discretize.R')
source('check.NA.R')
source('check.discrete.R')
source('custom.association.R')
source('custom.Modules.R')


myDashboardHeader <- function (..., title = NULL, titleWidth = NULL, disable = FALSE,
                               .list = NULL) {
  items <- c(list(...), .list)
  # lapply(items, tagAssert, type = "li", class = "dropdown")
  titleWidth <- validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_",
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n .main-header > .navbar {\n  margin-left: _WIDTH_;text-align: left;\n }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(class = "main-header", custom_css, style = if (disable)
    "display: none;", span(class = "logo", title), tags$nav(class = "navbar navbar-static-top",
                                                            role = "navigation", span(shiny::icon("bars"), style = "display:none;"),
                                                           # a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas",
                                                           #  role = "button", span(class = "sr-only", "Toggle navigation")),
                                                            div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav",
                                                                                                      items))))
}

dashboardPage(skin = "blue",
              myDashboardHeader(title = "ShinyBN",
                                titleWidth = "400"
                                #,tags$li(class = "dropdown", bsButton("homeIntro", label = NULL, icon = icon("question-circle", lib="font-awesome"), style = "primary", size = "large"))
              ),
              dashboardSidebar(width = 50,
                               sidebarMenu(id = "sidebarMenu",
                                           menuItem(text = "",
                                                    tabName = "Home",
                                                    icon = icon("home")
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("globe"),
                                                    tabName = "Structure"
                                           ),
                                           menuItem(text = "",
                                                    icon = shiny::icon("github"),
                                                    href = "https://github.com/SAFE-ICU/ShinyBN"),
                                          menuItem(text = "",
                                                    icon = shiny::icon("info"),
                                                  tabName = "About")
                                          )
                               ),
              dashboardBody(id ="dashboardBody",
                            # Include shinyalert Ui
                            useShinyalert(),
                            # Include introjs UI
                            rintrojs::introjsUI(),
                            #shinythemes::themeSelector(),
                            #theme = shinytheme("united"),
                            tags$script(HTML("$('body').addClass('fixed');")),
                            shinydashboard::tabItems(
                            shinydashboard::tabItem(tabName = "Home",
                                                      fluidRow(box(#title = "",
                                                                   status = "primary",
                                                                   width = 12,
                                                                   div(style="text-align:center",
                                                                       shiny::img(src = "placeholder-logo.png",height = 110,width = 110),
                                                                       shiny::h1("ShinyBN"),
                                                                       shiny::h2("Democratizing Bayesian Networks for Data Driven Decisions")
                                                                   ),
                                                                   br(),
                                                                   hr(),
                                                                   fluidRow(
                                                                     style = "margin-left:40px;padding:10px;",
                                                                     column(width=3, align = "center", h4('Learn Knowledge Network')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Engineer and Assess Insights')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Take Decisions'))
                                                                   ),
                                                                   hr(),
                                                                   div(style="text-align:center",
                                                                       actionButton("start", "Start Analyzing", style  = "background-color:#2E86C1;color:white;height:50px;font-size:20px", width = '300px', align = "center")
                                                                   )
                                                               )

                                                      )),
                              shinydashboard::tabItem(tabName = "Structure",
                                                          tabBox(id = "visula_tabs",
                                                                 width = 12,
                                                                 tabPanel('Data',

                                                                          shinyWidgets::radioGroupButtons(inputId = "dataoption",
                                                                                                          choices = c("Dataset","Distribution","Other Data"),
                                                                                                          selected = "Dataset",
                                                                                                          justified = FALSE
                                                                          ),

                                                                          conditionalPanel(
                                                                            "input.dataoption =='Dataset'",
                                                                            fluidRow(style="padding:0px",
                                                                              shiny::column(1, dropdownButton(
                                                                                shiny::h4("Upload Data:"),
                                                                                shiny::helpText("Select prefered input format(RData suggested for data > 100mb)"),
                                                                                h5('Data Format:'),
                                                                                shiny::selectInput('format',label = NULL,c(".RData",".CSV")),
                                                                                h5('File Input:'),
                                                                                shiny::fileInput('dataFile',
                                                                                                 label = NULL,
                                                                                                 accept = c('.RData','.csv')
                                                                                ),
                                                                                label = "upload",circle = F, status = "primary", icon = icon("upload"), width = "500px",tooltip = tooltipOptions(title = "upload data")
                                                                              )),
                                                                              shiny::column(1, dropdownButton(
                                                                                hr(),
                                                                                div(id="dataImpute",
                                                                                    shiny::h4("Impute Missing Data:"),
                                                                                    actionButton('impute','Impute')),
                                                                                hr(),
                                                                                div(id="dataDiscretize",
                                                                                    shiny::h4('Discretize Data'),
                                                                                    h5('Discretization Type:'),
                                                                                    shiny::fluidRow(column(6,shiny::selectInput('dtype',label = NULL,c("interval","quantile","frequency","cluster","hybrid"))),column(6,actionButton('discretize',"Discretize")))
                                                                                    #h5("subset columns in data using the tables")

                                                                                ),
                                                                                label = "Process",circle = F, status = "primary", icon = icon("edit"), width = "500px",tooltip = tooltipOptions(title = "preprocess data")
                                                                              )),
                                                                              shiny::column(1, downloadButton("downloadDataset", "Download", class = "butt"))),
                                                                              tags$head(tags$style(".butt{background-color:#2E86C1;} .butt{color:white;} .butt{border:#2E86C1;}")

                                                                            ),
                                                                            hr(),
                                                                            withSpinner(DT::dataTableOutput("datasetTable"),color = "#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.dataoption=='Distribution'",
                                                                            selectInput("freqSelect",label = "Variable",""),
                                                                            withSpinner(plotOutput("freqPlot",height = "600px"),color="#2E86C1")
                                                                            ),
                                                                          conditionalPanel(
                                                                            "input.dataoption=='Other Data'",
                                                                            shiny::fluidRow(shiny::column(4,selectInput("tableName",label = NULL,"")),shiny::column(1,downloadButton("downloadData", "Download"))),
                                                                            withSpinner(DT::dataTableOutput("tableOut"),color = "#2E86C1")
                                                                          )
                                                                          ),
                                                                 tabPanel("Association Network",
                                                                          shiny::fluidRow(
                                                                            shiny::column(1,
                                                                                          div(style="width: 500px;",
                                                                                          dropdownButton(
                                                                                            h5("Association Network"),
                                                                                            shiny::fluidRow(column(6,shiny::selectInput('assocType',label = NULL,c("cramer's V","Cohen's D","Goodman Kruskal lambda","Tschuprow's T"))),column(6,actionButton('association',"Build"))),
                                                                                            sliderInput("threshold", label = "Association Threshold",min = 0, max = 1,value = 0.75),

                                                                                            label = "` Build",circle = F, status = "primary", icon = icon("glyphicon glyphicon-wrench",lib = "glyphicon"), width = "300px",tooltip = tooltipOptions(title = "Build association graph")
                                                                                            ))
                                                                                          ),
                                                                            shiny::column(1, style='margin-right:0px;',
                                                                                          dropdownButton(
                                                                                            div(id="Agraph",
                                                                                                h4('Group of variables'),
                                                                                                shiny::fluidRow(shiny::column(6,selectInput('Avarselect',label = "Variables","",multiple = T)),
                                                                                                                shiny::column(3,selectInput('Avarshape',label = "Shape","")),
                                                                                                                shiny::column(3,actionButton('Agroup','Group', style="margin-top:25px;"))


                                                                                                ),
                                                                                                hr(),
                                                                                                h4('Vector of indices'),
                                                                                                shiny::fluidRow(shiny::column(6,textInput('Avarselectvector',label = "Variables")),
                                                                                                                shiny::column(3,selectInput('Avarshape2',label = "Shape","")),
                                                                                                                shiny::column(3,actionButton('Agroup2','Group', style="margin-top:25px;"))
                                                                                                ),
                                                                                                hr(),
                                                                                                shiny::fluidRow(shiny::column(6,h4('Visible Neighbors chain'),div(id = "AgraphChain",
                                                                                                                                    sliderInput("Adegree", label = NULL,
                                                                                                                                                min = 1, max = 10,
                                                                                                                                                value = 2
                                                                                                                                    ))),
                                                                                                                shiny::column(6,h4('Nth Neighbors'), div(id = "ANChain",
                                                                                                                                    sliderInput("AdegreeN", label = NULL,
                                                                                                                                                min = 1, max = 10,
                                                                                                                                                value = 2
                                                                                                                                    )))

                                                                                                ),
                                                                                                hr(),
                                                                                                div(id="AgraphLayout",
                                                                                                    h4("Select Graph Layout"),
                                                                                                    shiny::selectInput('Agraph_layout',label = NULL,"layout_nicely"))
                                                                                            ),
                                                                                            label = "Settings",circle = F, status = "primary", icon = icon("gear"), width = "500px",tooltip = tooltipOptions(title = "graph settings")
                                                                                            )
                                                                                          ),
                                                                            shiny::column(1,bsButton('graphBtn2', 'Refresh', icon = icon("refresh"),style = "primary")),
                                                                            shiny::column(3,
                                                                                          div(style = "position:absolute;right:0.1em;",
                                                                                          h5("N-distance neighors:"))),

                                                                            shiny::column(4,

                                                                                          shiny::selectInput("Aneighbornodes",label = NULL,choices = "")
                                                                                          )
                                                                          ),
                                                                          br(),
                                                                          withSpinner(visNetworkOutput("assocPlot",height = "550px"), color= "#2E86C1")
                                                                 ),
                                                                 tabPanel("Bayesian Network",
                                                                          fluidPage(
                                                                            shiny::fluidRow(
                                                                              shiny::column(3,shinyWidgets::radioGroupButtons(inputId = "bayesianOption",
                                                                                                                              choices = c("Graph","CP Distribution", "Inference Plot"),
                                                                                                                              selected = "Graph",
                                                                                                                              justified = FALSE
                                                                              )),
                                                                              shiny::column(1,dropdownButton(
                                                                                h5("Paramter learning type"),
                                                                                selectizeInput('paramMethod',label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                                hr(),
                                                                                div(style ='overflow-y:scroll',
                                                                                    # File input
                                                                                    shiny::p("Note: Upload .RData file"),
                                                                                    shiny::fileInput(
                                                                                      'structFile',
                                                                                      strong('File Input:'),
                                                                                      accept = c('.RData')
                                                                                    )
                                                                                ),
                                                                                label = "Upload",circle = F, status = "primary", icon = icon("upload"), width = "400px",tooltip = tooltipOptions(title = "Upload structure")
                                                                              )),
                                                                              shiny::column(1,dropdownButton(
                                                                                div(style ='overflow-y:scroll;height:600px;padding-right:20px;',

                                                                                    # Structural learning algorithm input select
                                                                                    shiny::selectizeInput(
                                                                                      inputId = "alg",
                                                                                      shiny::h5("Learning Algorithm:"),
                                                                                      choices = list(
                                                                                        "Score-based Learning(recommended)" =
                                                                                          c("Hill Climbing" = "hc",
                                                                                            "Tabu" = "tabu"),
                                                                                        "Constraint-based Learning" =
                                                                                          c("Grow-Shrink" = "gs",
                                                                                            "Incremental Association" = "iamb",
                                                                                            "Fast IAMB" = "fast.iamb",
                                                                                            "Inter IAMB" = "inter.iamb",
                                                                                            "PC" = "pc.stable"
                                                                                          ),
                                                                                        "Hybrid Learning" =
                                                                                          c("Max-Min Hill Climbing" = "mmhc",
                                                                                            "2-phase Restricted Maximization" = 'rsmax2'
                                                                                          ),
                                                                                        "Local Discovery Learning" =
                                                                                          c("Max-Min Parents and Children" = 'mmpc',
                                                                                            "Semi-Interleaved HITON-PC" = "si.hiton.pc",
                                                                                            "ARACNE" = "aracne",
                                                                                            "Chow-Liu" = "chow.liu"
                                                                                          )
                                                                                      )
                                                                                    ),
                                                                                    hr(),
                                                                                    fluidRow(
                                                                                      column(6, h5("Bootstrap replicates"),
                                                                                             sliderInput("boot", label = NULL,
                                                                                                         min = 1, max = 1000,
                                                                                                         value = 10)),
                                                                                      column(6, h5("Proportion of sample for Bootstrap:"),
                                                                                             sliderInput("SampleSize", label = NULL,
                                                                                                         min = 0, max = 1,
                                                                                                         value = 0.7))
                                                                                    ),

                                                                                    hr(),
                                                                                    fluidRow(
                                                                                      column(6,h5("Edge Strength"),
                                                                                             sliderInput("edgeStrength", label = NULL,
                                                                                                         min = 0, max = 1,
                                                                                                         value = 0.5)),
                                                                                      column(6,h5("Direction Confidence:"),
                                                                                             sliderInput("directionStrength", label = NULL,
                                                                                                         min = 0, max = 1,
                                                                                                         value = 0.5))
                                                                                    ),


                                                                                    hr(),

                                                                                    h5("Parameter Learning Type"),
                                                                                    selectizeInput("paramMethod2",label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                                    h5("Inject Expert Knowledge by Forcing/Prohibiting Edges"),
                                                                                    shiny::fluidRow(shiny::column(6,selectInput("listType",label = NULL,choices = c("Blacklist","Whitelist"))),shiny::column(6,shiny::fileInput('listFile',label = NULL,accept = c('.csv')))),
                                                                                    actionButton('learnBtn', 'Bootstrap'),
                                                                                    actionButton('learnSBtn','Direct'),
                                                                                    hr(),
                                                                                    shiny::h5("Save learned structure"),
                                                                                    downloadButton('saveBtn','Save')
                                                                                ),
                                                                                label = "`  Learn",circle = F, status = "primary", icon = icon("wrench"), width = "500px",tooltip = tooltipOptions(title = "Learn structure")
                                                                              )),
                                                                              shiny::column(1, dropdownButton(
                                                                                h5("Validation Method"),
                                                                                shiny::selectInput('crossFunc',label = NULL,choices = c("k-fold","hold-out")),
                                                                                h5("Parameter Fitting Method"),
                                                                                shiny::selectInput('paramMethod3',label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes")),
                                                                                h5("Loss function"),
                                                                                shiny::selectInput('lossFunc',label = NULL,choices = c("pred","pred-lw")),
                                                                                shiny::actionButton("calLoss","Cross Validate"),
                                                                                h5("Log-Likelihood Loss of the learned model"),
                                                                                shiny::verbatimTextOutput("valLoss"),
                                                                                label = "Validate",circle = F, status = "primary", icon = icon("check"), width = "400px",tooltip = tooltipOptions(title = "Validate structure")
                                                                              )),
                                                                              shiny::column(1, dropdownButton(
                                                                                shiny::h4("Display inference plot"),
                                                                                shiny::fluidRow(shiny::column(5,actionButton('plotBtn', 'Simple Plot')),shiny::column(4,actionButton('plotStrengthBtn', 'Confidence Plot'))),
                                                                                hr(),
                                                                                shiny::h4("No of iterations for confidence plot"),
                                                                                sliderInput("numInterval", label = NULL,
                                                                                            min = 1, max = 500,
                                                                                            value = 25
                                                                                ),
                                                                                hr(),
                                                                                h4("Select evidence to add to the model"),
                                                                                shiny::fluidRow(shiny::column(6,actionButton('insertBtn', 'Insert')),
                                                                                                shiny::column(6,actionButton('removeBtn', 'Remove'))
                                                                                ),
                                                                                shiny::fluidRow(shiny::column(6,tags$div(id = 'placeholder1')),
                                                                                                shiny::column(6,tags$div(id = 'placeholder2'))
                                                                                ),
                                                                                hr(),
                                                                                h4("Select an event of interest"),
                                                                                shiny::h5("Event Node:"),
                                                                                shiny::selectInput("event",
                                                                                                   label = NULL,
                                                                                                   ""),
                                                                                label = "Inference",circle = F, status = "primary", icon = icon("bar-chart-o"), width = "400px",tooltip = tooltipOptions(title = "Learn Inferences")
                                                                              ))),
                                                                            #hr(),

                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Graph'",
                                                                              shiny::column(11,
                                                                                            shiny::fluidRow(

                                                                                              shiny::column(2,
                                                                                                            div(
                                                                                                                h5("Nth Neigbors of selected node:"))),

                                                                                              shiny::column(3,style="padding-right:0px",
                                                                                                            shiny::selectInput("neighbornodes",label = NULL,choices = "")),
                                                                                              shiny::column(1,
                                                                                                            div(style = "position:absolute;right:0em;",
                                                                                                                h5("Modules:"))),
                                                                                              shiny::column(2,style="padding-right:0px",
                                                                                                            shiny::selectInput("moduleSelection",label = NULL,"graph")),
                                                                                              shiny::column(1,bsButton("Bcommunities","Build Modules", style="primary"), style="margin-right:20px"),


                                                                                              shiny::column(1,style = "margin-right:8px",
                                                                                                            dropdownButton(right = FALSE,
                                                                                                              div(id="Bgraph",
                                                                                                                  h4('Group of variables:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectizeInput('varselect',label = "Variables","",multiple = T)),
                                                                                                                                  shiny::column(3,selectInput('varshape',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group','Group', style="margin-top:25px;"))

                                                                                                                  ),

                                                                                                                  #hr(),
                                                                                                                  h4('Vector of index:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,textInput('varselectvector',label = "Variables")),
                                                                                                                                  shiny::column(3,selectInput('varshape2',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group2','Group', style="margin-top:25px;"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors chain'),div(id = "graphChain",
                                                                                                                                                                                    sliderInput("degree", label = NULL,
                                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                                value = 2
                                                                                                                                                                                    ))),
                                                                                                                                  shiny::column(6,h4('Nth Neighbors'), div(id = "NChain",
                                                                                                                                                                           sliderInput("degreeN", label = NULL,
                                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                                       value = 2
                                                                                                                                                                           )))

                                                                                                                  ),
                                                                                                                  #hr(),
                                                                                                                  div(id="graphLayout",
                                                                                                                      h4("Select Graph Layout"),
                                                                                                                      shiny::selectInput('graph_layout',label = NULL,"layout_nicely"))
                                                                                                              ),
                                                                                                              label = "Settings",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "graph settings")
                                                                                                            )
                                                                                              ),
                                                                                              shiny::column(1, bsButton('graphBtn', 'Refresh', icon = icon("refresh"),style = "primary"))),

                                                                                            withSpinner(visNetworkOutput("netPlot",height = "480px"), color= "#2E86C1")
                                                                                            )
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Inference Plot'",
                                                                              sliderInput("NumBar", label = "No. of bars",min = 0, max = 1,value = 1,step=1),
                                                                              actionButton("sortPlot","Sort X-axis"),
                                                                              withSpinner(plotOutput("distPlot",height = "450px"), color="#2E86C1")
                                                                            ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='CP Distribution'",
                                                                              selectInput("paramSelect",label = "Variable",""),
                                                                              withSpinner(plotOutput("parameterPlot",height = "450px"),color="#2E86C1")
                                                                            )
                                                                            )
                                                                         ),
                                                                 tabPanel("App Settings",
                                                                          shiny::fluidRow(
                                                                            column(3,materialSwitch(inputId = "parallel", label = "Go parallel", status = "primary", right = TRUE), style="margin:30px;"),
                                                                            column(3,selectInput("clusters",choices = c(1:20),label = "Number of clusters")))
                                                                 )
                                                                 )



                              ),
                              tabItem(tabName = "About",
                                      fluidRow(box(
                                        status = "primary",
                                        width = 12,
                                        div(style="text-align:center",
                                            h1('Creators')
                                        ),
                                        fluidRow(
                                          style = "margin-left:50px;padding:10px;",
                                          column(width=3, align = "center",
                                                 img(src = "shubham.jpg",style = "max-width: 50%; width: 50%; height: auto"),
                                                 h4('Shubham Maheshwari'),
                                                 h5('Data Scientist, Stockroom.io'),
                                                 h5('B.Tech Computer Science, IIIT-Delhi'),


                                                 fluidRow(width = 12,
                                                   column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/shubham14101"),target = "_blank"),
                                                   column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/shubham.maheshwari3"),target = "_blank"),
                                                   column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.linkedin.com/in/shubham-maheshwari-93a35b108/"),target = "_blank"),
                                                   column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/real_SM96"),target = "_blank")
                                                 )),
                                          column(width=1, align = "center", img(src = "vertical-line.png",style = "max-width: 100%; width: 100%; height: 100%;")),
                                          column(width=3, align = "center",
                                                 img(src = "anant.jpg", style = "max-width: 50%; width: 50%; height: auto"),
                                                 h4('Anant Mittal'),
                                                 h5('Data Scientist, Egregore Labs'),
                                                 h5('B.Tech Computer Science, IIIT-Delhi'),


                                                 fluidRow(width = 12,
                                                          column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/anant15"), target = "_blank"),
                                                          column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/shubham.maheshwari3"), target = "_blank"),
                                                          column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.linkedin.com/in/shubham-maheshwari-93a35b108/"), target = "_blank"),
                                                          column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/real_SM96"),target = "_blank")
                                                 )),
                                          column(width=1, align = "center", img(src = "vertical-line.png",style = "max-width: 100%; width: 100%; height: auto;")),
                                          column(width=3, align = "center",
                                                 img(src = "tps.jpg", style = "max-width: 50%; width: 50%; height: auto;"),
                                                 h4('Tavpritesh Sethi'),
                                                 h5('Assistant Professor, IIIT-Delhi'),
                                                 h5('Visiting Assistant Professor, Stanford Medicine'),

                                                 fluidRow(width = 12,
                                                          column(width=3, a(img(src = "github.png", width = '30px', height = '30px'), href = "https://github.com/SAFE-ICU?tab=repositories"), target = "_blank"),
                                                          column(width=3, a(img(src = "facebook.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://www.facebook.com/tavpritesh.sethi"), target = "_blank"),
                                                          column(width=3, a(img(src = "linkedin.png", style = "margin:5px; width: 20px; height: 20px"), href = "https://in.linkedin.com/in/tavpritesh"), target = "_blank"),
                                                          column(width=3, a(img(src = "twitter.png", style = "margin:6px; width: 18px; height: 18px"), href = "https://twitter.com/tavpritesh"), target = "_blank")
                                                 ))
                                        ),
                                        hr(),
                                        div(style="text-align:center",
                                            h4("ShinyBN: Democratizing Bayesian Network Analysis in Complex Multivariate Data (submitted)"),
                                            hr(),
                                            h4("Correspondence: tavpriteshsethi@iiitd.ac.in"),
                                            hr(),
                                            h4("Acknowledgements - We acknowledge the useful inputs provided
                                               by Prof. Rakesh Lodha, Professor, All India Institute of Medical Sciences, New Delhi, India")
                                        )


                                      )

                                      )
                                    )
),
tags$footer("Funding Support: The Wellcome Trust/DBT India Alliance grant IA/CPHE/14/1/501504 to Tavpritesh Sethi", align = "center", style = "
position:absolute;
            bottom:0;
            width:100%;
            height:30px;
            padding:5px;
            background-color: white;z-index:1200;")
)


)
