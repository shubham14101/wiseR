library('bnlearn')
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
source('dashboardthemes.R')


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
              myDashboardHeader(title = "wiseR",
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
                            shinyDashboardThemes(
                              theme = "grey_light"
                            ),
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
                                                                       shiny::img(src = "wiseR.png",height = 300,width = 400)
                                                                   ),
                                                                   br(),
                                                                   hr(),
                                                                   fluidRow(
                                                                     style = "margin-left:40px;padding:10px;",
                                                                     column(width=3, align = "center", h4('Discover Dark Knowledge')),
                                                                     column(width=1, align = "center", img(src = "arrow.png",height = 40,width = 60)),
                                                                     column(width=3, align = "center", h4('Assess Impact')),
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
                                                                 tabPanel("App Settings",
                                                                          shiny::fluidRow(
                                                                            column(3,materialSwitch(inputId = "parallel", label = "Enable Parallel Computing", status = "primary", right = TRUE), style="margin:30px;"),
                                                                            column(3,selectInput("clusters",choices = c(1:20),label = "Number of clusters")))
                                                                 ),
                                                                 tabPanel('Data',

                                                                          shiny::fluidRow(
                                                                            shiny::column(3,
                                                                                          shinyWidgets::radioGroupButtons(inputId = "dataoption",
                                                                                                                          choices = c("Dataset","Explore"),
                                                                                                                          selected = "Dataset",
                                                                                                                          justified = FALSE
                                                                                          )
                                                                                          )
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.dataoption =='Dataset'",
                                                                            fluidRow(style="padding:0px",
                                                                              shiny::column(2, dropdownButton(
                                                                                h5('Choose default dataset'),
                                                                                fluidRow(column(9,selectInput('defData',label = NULL,choices = c("Alarm","Asia","Coronary","Lizards","Marks","Insurance","Hailfinder"))),column(3,actionButton('loadDef','load'))),
                                                                                h5('Data Format:'),
                                                                                shiny::selectInput('format',label = NULL,c(".RData",".CSV")),
                                                                                h5('File Input:'),
                                                                                shiny::fileInput('dataFile',
                                                                                                 label = NULL,
                                                                                                 accept = c('.RData','.csv')
                                                                                ),
                                                                                label = "upload",circle = F, status = "primary", icon = icon("upload"), width = "500px",tooltip = tooltipOptions(title = "upload data as csv or RData")
                                                                              )),
                                                                              shiny::column(2, dropdownButton(
                                                                                div(id="dataImpute",
                                                                                    shiny::h4("Impute Missing Data:"),
                                                                                    actionButton('impute','Impute')),
                                                                                div(id="dataDiscretize",
                                                                                    shiny::h4('Discretize Data'),
                                                                                    h5('Discretization Type:'),
                                                                                    shiny::fluidRow(column(9,shiny::selectInput('dtype',label = NULL,c("hybrid discretization(Recommended)"="hybrid","interval discretization"="interval","quantile discretization"="quantile","frequency discretization"="frequency","K-means clustering"="cluster"))),column(3,actionButton('discretize',"Discretize")))
                                                                                    #h5("subset columns in data using the tables")

                                                                                ),
                                                                                div(id="dataTranspose",
                                                                                    shiny::h4("Transpose data frame:"),
                                                                                    actionButton('transpose','Transpose')),
                                                                                div(id="dataSort",
                                                                                    shiny::h4("Sort data frame:"),
                                                                                    actionButton('sort','Arrange Columns')),
                                                                                div(id="dataDelete",
                                                                                    shiny::h4("Delete variables"),
                                                                                    shiny::fluidRow(shiny::column(6,selectInput('delSelect',label = NULL,"",multiple = T)),shiny::column(3,actionButton('delete','Delete')),shiny::column(3,actionButton('reset','Reset')))
                                                                                ),
                                                                                label = "Pre-Process",circle = F, status = "primary", icon = icon("edit"), width = "500px",tooltip = tooltipOptions(title = "prepare data for bayesian network analysis")
                                                                              )),
                                                                              shiny::column(2, downloadButton("downloadDataset", "Download", class = "butt"))),
                                                                              tags$head(tags$style(".butt{background-color:#2E86C1;} .butt{color:white;} .butt{border:#2E86C1;}")

                                                                            ),
                                                                            hr(),
                                                                            withSpinner(DT::dataTableOutput("datasetTable"),color = "#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.dataoption=='Explore'",
                                                                            selectInput("freqSelect",label = "Variable",""),
                                                                            withSpinner(plotOutput("freqPlot",height = "600px"),color="#2E86C1")
                                                                            )
                                                                          ),
                                                                 tabPanel("Association Network",
                                                                          shiny::fluidRow(
                                                                            column(5,h5("")),
                                                                            column(4,shinyWidgets::radioGroupButtons(inputId = "assocOption",
                                                                                                                     choices = c("Graph","Table"),
                                                                                                                     selected = "Graph",
                                                                                                                     justified = FALSE
                                                                            ))
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.assocOption=='Graph'",
                                                                            shiny::fluidRow(
                                                                              shiny::column(1,
                                                                                            div(style="width: 500px;",
                                                                                                dropdownButton(
                                                                                                  h5("Association Network"),
                                                                                                  shiny::fluidRow(column(8,shiny::selectInput('assocType',label = NULL,c("cramer's V (Recommended)"="cramer's V","Cohen's D","Goodman Kruskal lambda","Tschuprow's T"))),column(4,actionButton('association',"Build"))),
                                                                                                  sliderInput("threshold", label = "Association Threshold",min = 0, max = 1,value = 0.75),

                                                                                                  label = "` Build",circle = F, status = "primary", icon = icon("glyphicon glyphicon-wrench",lib = "glyphicon"), width = "400px",tooltip = tooltipOptions(title = "Build association Network")
                                                                                                ))
                                                                              ),
                                                                              shiny::column(2, style='margin-right:0px;',
                                                                                            dropdownButton(
                                                                                              div(id="Agraph",
                                                                                                  h4('Highlight Variables'),
                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('Avarselect',label = "Variables Names","",multiple = T)),
                                                                                                                  shiny::column(3,selectInput('Avarshape',label = "Shape","")),
                                                                                                                  shiny::column(3,actionButton('Agroup','Group', style="margin-top:25px;"))


                                                                                                  ),
                                                                                                  h4("Or"),
                                                                                                  shiny::fluidRow(shiny::column(6,textInput('Avarselectvector',label = "Column indices")),
                                                                                                                  shiny::column(3,selectInput('Avarshape2',label = "Shape","")),
                                                                                                                  shiny::column(3,actionButton('Agroup2','Group', style="margin-top:25px;"))
                                                                                                  ),
                                                                                                  hr(),
                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors chain'),div(id = "AgraphChain",
                                                                                                                                                                    sliderInput("Adegree", label = NULL,
                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                value = 1
                                                                                                                                                                    ))),
                                                                                                                  shiny::column(6,h4('Nth Order Neighbors'), div(id = "ANChain",
                                                                                                                                                           sliderInput("AdegreeN", label = NULL,
                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                       value = 1
                                                                                                                                                           )))

                                                                                                  ),
                                                                                                  hr(),
                                                                                                  div(id="AgraphLayout",
                                                                                                      h4("Select Graph Layout"),
                                                                                                      shiny::selectInput('Agraph_layout',label = NULL,"layout_nicely"))
                                                                                              ),
                                                                                              label = "Visual Settings",circle = F, status = "primary", icon = icon("gear"), width = "500px",tooltip = tooltipOptions(title = "graph visualization settings")
                                                                                            )
                                                                              ),
                                                                              shiny::column(1,bsButton('graphBtn2', 'Refresh', icon = icon("refresh"),style = "primary")),
                                                                              shiny::column(3,
                                                                                            div(style = "position:absolute;right:0.1em;",
                                                                                                h5("Nth Order neighbors:"))),

                                                                              shiny::column(4,

                                                                                            shiny::selectInput("Aneighbornodes",label = NULL,choices = "")
                                                                              )
                                                                            ),
                                                                            br(),
                                                                            withSpinner(visNetworkOutput("assocPlot",height = "550px"), color= "#2E86C1")
                                                                          ),
                                                                          conditionalPanel(
                                                                            "input.assocOption=='Table'",
                                                                            downloadButton('assocDownload','Download'),
                                                                            withSpinner(DT::dataTableOutput("assocTable"),color = "#2E86C1")
                                                                          )
                                                                 ),
                                                                 tabPanel("Bayesian Network",
                                                                          fluidPage(
                                                                            shiny::fluidRow(
                                                                              shiny::column(1,dropdownButton(
                                                                                shinyWidgets::radioGroupButtons(inputId = "structureOption",
                                                                                                                choices = c("Upload Network","Initialize","Learn New","Edit","Validate Network"),
                                                                                                                selected = "Upload Network",
                                                                                                                justified = FALSE
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Upload Network'",
                                                                                  h5("Paramter learning type"),
                                                                                  selectizeInput('paramMethod',label = NULL,choices = c("Bayesian parameter estimation" = "bayes","Maximum Likelihood parameter estimation" = "mle")),
                                                                                  hr(),
                                                                                  shinyWidgets::radioGroupButtons(inputId = "uploadOption",
                                                                                                                  choices = c("Averaged Network","Bootstraped Network"),
                                                                                                                  selected = "Averaged Network",
                                                                                                                  justified = FALSE
                                                                                  ),
                                                                                  shiny::conditionalPanel(
                                                                                    "input.uploadOption=='Averaged Network'",
                                                                                    div(
                                                                                      # File input
                                                                                      shiny::p("Note: Upload .RData file"),
                                                                                      shiny::fileInput(
                                                                                        'structFile',
                                                                                        strong('File Input:'),
                                                                                        accept = c('.RData')
                                                                                      )

                                                                                    )
                                                                                  ),
                                                                                  shiny::conditionalPanel(
                                                                                    "input.uploadOption=='Bootstraped Network'",
                                                                                    div(
                                                                                      # File input
                                                                                      shiny::p("Note: Upload .RData file"),
                                                                                      shiny::fileInput(
                                                                                        'bootFile',
                                                                                        strong('File Input:'),
                                                                                        accept = c('.RData')
                                                                                      ),
                                                                                      fluidRow(
                                                                                        column(6,h5("Edge Strength"),
                                                                                               sliderInput("edgeStrengthU", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5)),
                                                                                        column(6,h5("Direction Confidence:"),
                                                                                               sliderInput("directionStrengthU", label = NULL,
                                                                                                           min = 0, max = 1,
                                                                                                           value = 0.5))
                                                                                      )
                                                                                    )
                                                                                  ),
                                                                                  actionButton("parameterTuningU","Parameter Tuning")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Initialize'",
                                                                                  shiny::fluidRow(column(5,h5("Upload Prior Structure Object"))),
                                                                                  shiny::fluidRow(column(5,shiny::fileInput('priorFile',label = NULL,accept = c('.RData')))),
                                                                                  shiny::fluidRow(shiny::column(3,h5("from")),shiny::column(3,h5("to")),shiny::column(3,h5("")),shiny::column(3,h5("Select from table"))),
                                                                                  shiny::fluidRow(shiny::column(3,selectInput("fromarc1",label = NULL,choices=c())),shiny::column(3,selectInput("toarc1",label = NULL,choices=c())),column(3,actionButton("addarc1","Add")),actionButton("RemoveArc","Remove"),actionButton("ReverseArc","Reverse")),
                                                                                  withSpinner(DT::dataTableOutput("priorout"),color = "#2E86C1")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Learn New'",
                                                                                  div(style ='overflow-y:scroll;height:600px;padding-right:20px;',

                                                                                      # Structural learning algorithm input select
                                                                                      shiny::fluidRow(
                                                                                        shiny::column(5,
                                                                                                      shiny::selectizeInput(
                                                                                                        inputId = "alg",
                                                                                                        label="Learning Algorithm",
                                                                                                        choices = list(
                                                                                                          "Score-based Learning" =
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
                                                                                                      )
                                                                                        ),
                                                                                        shiny::column(7,
                                                                                                      selectizeInput("paramMethod2",label = "Parameter Fitting Method",choices = c("Bayesian parameter estimation" = "bayes","Maximum Likelihood parameter estimation" = "mle"))
                                                                                        )
                                                                                      ),
                                                                                      h5("Inject Expert Knowledge by Forcing/Prohibiting Edges"),
                                                                                      shiny::fluidRow(shiny::column(6,selectInput("listType",label = NULL,choices = c("Blacklist","Whitelist"))),shiny::column(6,shiny::fileInput('listFile',label = NULL,accept = c('.csv')))),
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
                                                                                      actionButton('learnBtn', 'Bootstrap'),
                                                                                      actionButton('learnSBtn','Direct'),
                                                                                      actionButton('PruneBtn','Parameter Tuning'),
                                                                                      hr(),
                                                                                      shiny::h5("Save learned structure"),
                                                                                      downloadButton('saveBtn','Save')
                                                                                  )
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Edit'",
                                                                                  shiny::fluidRow(shiny::column(3,h5("from")),shiny::column(3,h5("to")),shiny::column(3,h5("")),shiny::column(3,h5("Select from table"))),
                                                                                  shiny::fluidRow(shiny::column(3,selectInput("fromarc",label = NULL,choices=c())),shiny::column(3,selectInput("toarc",label = NULL,choices=c())),column(3,actionButton("addarc","Add")),actionButton("RemoveArc2","Remove"),actionButton("ReverseArc2","Reverse")),
                                                                                  withSpinner(DT::dataTableOutput("postout"),color = "#2E86C1")
                                                                                ),
                                                                                shiny::conditionalPanel(
                                                                                  "input.structureOption=='Validate Network'",
                                                                                  shiny::fluidRow(shiny::column(6,shiny::selectInput('crossFunc',label = "Validation Method",choices = c("k-fold","hold-out"))),shiny::column(6,shiny::selectInput('lossFunc',label = "Loss Function",choices = c("pred","pred-lw")))),
                                                                                  h5("Parameter Fitting Method"),
                                                                                  shiny::fluidRow(shiny::column(8,shiny::selectInput('paramMethod3',label = NULL,choices = c("Maximum Likelihood parameter estimation" = "mle","Bayesian parameter estimation" = "bayes"))),shiny::column(4,shiny::actionButton("calLoss","Cross Validate"))),
                                                                                  h5("Log-Likelihood Loss of the learned model"),
                                                                                  shiny::verbatimTextOutput("valLoss"),
                                                                                  h5("Network Score"),
                                                                                  shiny::fluidRow(shiny::column(6,selectInput("scoreAlgo",label = NULL,choices = c("modified Bayesian Dirichelt equivalent"="mbde","log-likelihood"="loglik","Akaike Information Criterion"="aic","Bayesian Information Criterion"="bic","Bayesian Dirichelt sparse"="bds","locally averaged Bayesian Dirichelt"="bdla"))),shiny::column(2,actionButton("getScore","Score")),shiny::column(4,shiny::verbatimTextOutput("netScore")))
                                                                                ),
                                                                                label = "Structure",circle = F, status = "primary", icon = icon("wrench"), width = "700px",tooltip = tooltipOptions(title = "Upload structure")
                                                                              )),
                                                                              shiny::column(2, dropdownButton(
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
                                                                                shiny::h4("Display inference plot"),
                                                                                shiny::fluidRow(shiny::column(5,actionButton('plotBtn', 'Simple Plot')),shiny::column(4,actionButton('plotStrengthBtn', 'Confidence Plot'))),
                                                                                hr(),
                                                                                shiny::h4("No of iterations for confidence plot"),
                                                                                textInput("numInterval", label = NULL,placeholder = 25),
                                                                                label = "Inference",circle = F, status = "primary", icon = icon("bar-chart-o"), width = "300px",tooltip = tooltipOptions(title = "Learn Inferences")
                                                                              )),
                                                                              shiny::column(5,shinyWidgets::radioGroupButtons(inputId = "bayesianOption",
                                                                                                                              choices = c("Graph","CP Distribution", "Inference Plot","Tables"),
                                                                                                                              selected = "Graph",
                                                                                                                              justified = FALSE
                                                                              ))
                                                                              ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='Graph'",
                                                                              shiny::column(11,
                                                                                            shiny::fluidRow(

                                                                                              shiny::column(2,
                                                                                                            div(
                                                                                                                h5("Nth Neigbors:"))),

                                                                                              shiny::column(3,style="padding-right:0px",
                                                                                                            shiny::selectInput("neighbornodes",label = NULL,choices = "")),
                                                                                              shiny::column(1,
                                                                                                            div(style = "position:absolute;right:0em;",
                                                                                                                h5("Modules:"))),
                                                                                              shiny::column(2,style="padding-right:0px",
                                                                                                            shiny::selectInput("moduleSelection",label = NULL,"graph")),
                                                                                              shiny::column(1,style="margin-right:20px",dropdownButton(
                                                                                                shiny::fluidRow(shiny::column(6,selectInput('moduleAlgo',label = NULL,choices = c("ward.D","ward.D2", "single", "complete", "average", "mcquitty", "median","centroid"))),shiny::column(1,bsButton("Bcommunities","Build Modules", style="primary"))),
                                                                                                label = "Modules",circle = F, status = "primary", width = "300px",tooltip = tooltipOptions(title = "Build modules in the graph")
                                                                                              )),
                                                                                              shiny::column(1,style = "margin-right:8px",
                                                                                                            dropdownButton(
                                                                                                              div(id="Bgraph",
                                                                                                                  h4('Group of variables:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectizeInput('varselect',label = "Variables","",multiple = T)),
                                                                                                                                  shiny::column(3,selectInput('varshape',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group','Group', style="margin-top:25px;"))

                                                                                                                  ),

                                                                                                                  hr(),
                                                                                                                  h4('Vector of index:'),
                                                                                                                  shiny::fluidRow(shiny::column(6,textInput('varselectvector',label = "Variables")),
                                                                                                                                  shiny::column(3,selectInput('varshape2',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group2','Group', style="margin-top:25px;"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,selectInput('modGroup',label = "modules",choices = "")),
                                                                                                                                  shiny::column(3,selectInput('varshape3',label = "Shape","")),
                                                                                                                                  shiny::column(3, actionButton('group3','Group', style="margin-top:25px;"))
                                                                                                                  ),
                                                                                                                  shiny::fluidRow(shiny::column(6,h4('Visible Neighbors'),div(id = "graphChain",
                                                                                                                                                                                    sliderInput("degree", label = NULL,
                                                                                                                                                                                                min = 1, max = 10,
                                                                                                                                                                                                value = 1
                                                                                                                                                                                    ))),
                                                                                                                                  shiny::column(6,h4('Nth Neighbors'), div(id = "NChain",
                                                                                                                                                                           sliderInput("degreeN", label = NULL,
                                                                                                                                                                                       min = 1, max = 10,
                                                                                                                                                                                       value = 1
                                                                                                                                                                           )))

                                                                                                                  ),
                                                                                                                  hr(),
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
                                                                              dropdownButton(
                                                                                sliderInput("NumBar", label = "No. of bars",min = 0, max = 1,value = 1,step=1),
                                                                                actionButton("sortPlot","Sort X-axis"),
                                                                                label = "Plot",circle = F, status = "primary", icon = icon("gear"), width = "400px",tooltip = tooltipOptions(title = "plot settings")
                                                                              ),
                                                                              withSpinner(plotOutput("distPlot",height = "450px"), color="#2E86C1")
                                                                            ),
                                                                            shiny::conditionalPanel(
                                                                              "input.bayesianOption=='CP Distribution'",
                                                                              selectInput("paramSelect",label = "Variable",""),
                                                                              withSpinner(plotOutput("parameterPlot",height = "450px"),color="#2E86C1")
                                                                            ),
                                                                            conditionalPanel(
                                                                              "input.bayesianOption=='Tables'",
                                                                              shiny::fluidRow(shiny::column(4,selectInput("tableName",label = NULL,"")),shiny::column(1,downloadButton("downloadData", "Download"))),
                                                                              withSpinner(DT::dataTableOutput("tableOut"),color = "#2E86C1")
                                                                            )
                                                                            )
                                                                         ),
                                                                 tabPanel("Custom Dashboard",
                                                                          shiny::fluidRow(
                                                                            column(3,h5("Name")),
                                                                            column(3,h5("Theme"))),
                                                                          shiny::fluidRow(
                                                                            column(3,textInput("name",placeholder = NULL,label = NULL)),
                                                                            column(3,selectInput("theme",label = NULL,choices = c("Blue gradient"="blue_gradient","BoE website"="boe_website","Grey light"="grey_light","Grey dark"="grey_dark","OneNote"="onenote","Poor man's Flatly"="poor_mans_flatly","Purple gradient"="purple_gradient"))),
                                                                            column(2,actionButton("build",'build')),
                                                                            column(3,downloadButton('dashboard','Custom Dashboard')))
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
                                                 #img(src = "shubham.jpg",style = "max-width: 50%; width: 50%; height: auto"),
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
                                                 #img(src = "anant.jpg", style = "max-width: 50%; width: 50%; height: auto"),
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
                                                 #img(src = "tps.jpg", style = "max-width: 50%; width: 50%; height: auto;"),
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
                                            h4("Correspondence: tavpriteshsethi@iiitd.ac.in"),
                                            hr(),
                                            h4("Acknowledgements - We acknowledge the insights provided
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
