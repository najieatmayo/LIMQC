#
### Title:
header <- dashboardHeader(title = 'LIMQC' )

### SideBar:
sidebar <- dashboardSidebar(
  
  dateRangeInput('dateRange',
                 label = 'Date range input: yyyy-mm-dd', format = "yyyy-mm-dd",
                 start = min(pt$pdate, na.rm  = T), end = Sys.Date()), ## min(pt$pdate, na.rm  = T) "2016-01-01"
  radioButtons("rb", "Input Data:",
               choices = c("Demo Data", "Upload Your Own Data"),
               selected = "Demo Data"),
  useShinyjs(),
  conditionalPanel(condition = 'input.rb == "Upload Your Own Data"',
                   fileInput("inFile", "", accept = c(".csv"))
  ),
  
  hr(),
  sidebarMenu(id="menu1",
              menuItem("Start", tabName = "start", icon = icon("info-circle")),
              
              menuItem("Data Examinations", tabName = "data", icon = icon("th")),
              
              menuItem("Descriptive statistics", tabName = "datades", icon = icon("list-alt")),
              
              menuItem("QC Dashboard", tabName = "QCdata", icon = icon("stats",lib='glyphicon')),
              
              menuItem("Interactive Graphs", tabName = "graphs", icon = icon("navicon"),
                       menuSubItem("Trending", tabName = "trend", icon = icon("line-chart")), 
                       menuSubItem("Correlation", tabName = "corr", icon = icon("random")),
                       menuSubItem("Comparison", tabName = "group", icon = icon("tasks")),
                       menuSubItem("Multi-panel examination", tabName = "mult", icon = icon("fas fa-cogs"))
                       ),
              ##menuItem("Report", tabName = "report", icon = icon("list-alt")),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              br(),
              hr(),
              
              menuItem("About", tabName = "about", icon = icon("question-circle")),
              menuItem("Disclaimer", tabName = "disclaimer", icon = icon("bell"))
             )
  )




  ### Dashboard:
body <- dashboardBody(
  
  ### Tabintes:
  
  tabItems(
    
    ### TAB 0 =  dashboard:
    
    tabItem(tabName = "start",

            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("start.md"))
            )
            ),
    
    ### TAB 1 = data dashboard:
    # 
    tabItem(tabName = "data",
            ##tags$hr(),
            fluidRow(
                     valueBoxOutput("vbox1", width = 3),
                     valueBoxOutput("vbox2", width = 3),

                box(
                  title = "Sample Type",
                  width = 3, height = 110,
  
                  shinyWidgets::pickerInput(
                    inputId = "ind_sample_groups",
                    label = "",
                    choices = NULL,
                    options = list('actions-box' = TRUE),
                    multiple = TRUE,
                    selected = NULL
                  )
                ),
                box(
                  title = "Cols to display",
                  width = 3, height = 110,
                  shinyWidgets::pickerInput(
                    inputId = "col2show",
                    label = "",
                    choices = NULL,
                    options = list('actions-box' = TRUE),
                    multiple = TRUE,
                    selected = NULL
                  )
                )
              ),
            fluidRow(
              box(title = "sample info",
                  width = 12,
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('display.da.tab')))
              ),
            fluidRow(title = "Explore:", 
              column(width = 6,
                     box(
                       title = "Cont Filter",
                       width = NULL,
                       shinyWidgets::pickerInput(
                         inputId = "ConditioningVariablesR",
                         label = "Conditioning Cont variables (choose one or more)",
                         choices = NULL,
                         options = list('actions-box' = TRUE),
                         multiple = TRUE,
                         selected = NULL
                       ),
                       
                       uiOutput("ControlWidgetsofConditioningVariablesR")
                     )
              ),
              column(width = 6,
                     box(
                       title = "Cat Filter",
                       width = NULL,
                       shinyWidgets::pickerInput(
                         inputId = "ConditioningVariables2R",
                         label = "Conditioning Cat variables (choose one or more)",
                         choices = NULL,
                         options = list('actions-box' = TRUE),
                         multiple = TRUE,
                         selected = NULL
                       ),
                       
                       uiOutput("ControlWidgetsofConditioningVariables2R")
                       
                     )))
            ),
    ### TAB 2 = data description dashboard:
    #
    tabItem(tabName = "datades",
            
            fluidPage( tabsetPanel(type = "tabs",
                                   tabPanel("Summary", tableOutput("arsTable")),
                                   tabPanel("Association", 
                                            sidebarLayout(
                                              
                                              # Define the sidebar with one input
                                              sidebarPanel(
                                                ##selectInput('varIntersted', 'Variable of interest:', choices = NULL,  selected = NULL),
                                                shinyWidgets::pickerInput(
                                                  inputId = "varIntersted",
                                                  label = "Variable of interest:",
                                                  choices = NULL,
                                                  options = list('actions-box' = TRUE),
                                                  multiple = FALSE,
                                                  selected = NULL
                                                ),
                                                shinyWidgets::pickerInput(
                                                  inputId = "AgainstVars",
                                                  label = "Against:",
                                                  choices = NULL,
                                                  options = list('actions-box' = TRUE),
                                                  multiple = TRUE,
                                                  selected = NULL
                                                )
                                              ),
                                              
                                              # Create a spot for the plot
                                              mainPanel(
                                                DT::dataTableOutput("assocTable"))
                                              )
                                            
                                            )))
    ),
    ### TAB 3 = QC stat data dashboard:
    # 
    tabItem(tabName = "QCdata",
            ##tags$hr(),
            fluidRow(
            
              box(
                title = "Sample Type",
                width = 4,
                
                shinyWidgets::pickerInput(
                  inputId = "ind_sample_groupsQC",
                  label = "",
                  choices = NULL,
                  options = list('actions-box' = TRUE),
                  multiple = TRUE,
                  selected = NULL
                )
              ),
              
              valueBoxOutput("vbox3", width = 4), ## number of QC pass
              valueBoxOutput("vbox4", width = 4)), ## number of QC warning
            fluidRow(
              box(title = "Monthly Test Volume",
                  width = 6,
                  plotlyOutput("TVolume")),
              box(title = "Monthly QC Warning Rate",
                  width = 6,
                  plotlyOutput("Wrate"))
            ),
            fluidRow(
              box(title = "QC warning samples",
                  width = 12,
                  div(style = 'overflow-x: scroll', DT::dataTableOutput('display.qcda.tab'))))
              
             
    ),
    
    # TAB 4 =     graphs dashboard: trending
    tabItem(tabName = "trend",
              fluidRow(
                column(8,h4("Trending"),
                       ##Tabset per sample or perrun ----
                       tabsetPanel(type = "tabs",
                                   tabPanel("PerSample", plotlyOutput("trending")),
                                   tabPanel("BoxPerDate", plotlyOutput("datetrending")),
                                   tabPanel("MedianPerDate", plotlyOutput("Mdatetrending")),
                                   tabPanel("BoxPerRun", plotlyOutput("runtrending")),
                                   tabPanel("MedianPerRun", plotlyOutput("Mruntrending")),
                                   tabPanel("MovingWindow", plotlyOutput("MWtrending")))
                       ),
                column(4,h4("Distribution"),
                       tabsetPanel(type = "tabs",
                                   tabPanel("Histogram", plotlyOutput("trending2")),
                                   tabPanel("Stat", verbatimTextOutput("stats"))
                                   )
                )
            ),
            fluidRow(
              column(width = 4,
                     box(
                       title = "metric to plot",
                       width = NULL,
                       shinyWidgets::pickerInput(
                         inputId = "ind_metric",
                         label = "",
                         choices = NULL,
                         options = list('actions-box' = TRUE),
                         multiple = FALSE,
                         selected = NULL)
                     )),
              column(width = 4,
                     box(
                       title = "Cont Filter",
                       width = NULL,
                       shinyWidgets::pickerInput(
                         inputId = "ConditioningVariables",
                         label = "Conditioning Cont variables (choose one or more)",
                         choices = NULL,
                         options = list('actions-box' = TRUE),
                         multiple = TRUE,
                         selected = NULL
                       ),
                       
                       uiOutput("ControlWidgetsofConditioningVariables")
                     )
              ),
              column(width = 4,
                     box(
                       title = "Cat Filter",
                       width = NULL,
                       shinyWidgets::pickerInput(
                         inputId = "ConditioningVariables2",
                         label = "Conditioning Cat variables (choose one or more)",
                         choices = NULL,
                         options = list('actions-box' = TRUE),
                         multiple = TRUE,
                         selected = NULL
                       ),
                       
                       uiOutput("ControlWidgetsofConditioningVariables2")
                       
                     )))
    ),
    ##TAB 3 =     graphs dashboard: correlation
    tabItem(tabName = "corr",

            sidebarLayout(

              # Define the sidebar with one input
              sidebarPanel(
                selectInput('CorrST', 'Sample Type:', choices = NULL,  selected = NULL),
                
                selectInput('metric_x', 'Metric 1:', choices = NULL,  selected = NULL),
                checkboxInput('FilterX', "Filter", value = FALSE),
                conditionalPanel(condition = "input.FilterX",
                                 sliderInput("FX",
                                             "Select Metric1 Range",
                                             min = -999,
                                             max = 100,
                                             step = 1,
                                             dragRange =  TRUE,
                                             value = c(-900,25))
                                  ),
                
                selectInput('metric_y', 'Metric 2:', choices = NULL, selected = NULL),
                checkboxInput('FilterY', "Filter", value = FALSE),
                conditionalPanel(condition = "input.FilterY",
                                 sliderInput("FY",
                                             "Select Metric2 Range",
                                             min = -999,
                                             max = 100,
                                             step = 1,
                                             dragRange =  TRUE,
                                             value = c(-900,25))
                ),
                
                br(),
                shinyWidgets::pickerInput(
                  inputId = "lab_metric",
                  label = "label point with",
                  choices = anames,
                  options = list('actions-box' = TRUE),
                  multiple = TRUE,
                  selected = anames[1]),
                checkboxInput('logx', "log scale on x", value = FALSE),
                checkboxInput('logy', "log scale on y", value = FALSE),
                checkboxInput('addtrend', "Add trend line", value = FALSE)
                
              ),

              # Create a spot for the plot
              mainPanel(
                plotlyOutput("Corr"),
                conditionalPanel(
                  condition = "input.addtrend == true",
                  verbatimTextOutput("corrstats") 
                )
              )

            )
    ),
    # TAB 3 =     graphs dashboard: group
    tabItem(tabName = "group",

            sidebarLayout(

              # Define the sidebar with one input
              sidebarPanel(
                selectInput('sampleTG', 'Sample Type:', choices = NULL,  selected = NULL),
                selectInput('metric_by', 'Metric:', choices = NULL,  selected = NULL),
                selectInput('by_group', 'Group by:', choices = NULL, selected = NULL)
              ),

              # Create a spot for the plot
              mainPanel(
                plotlyOutput("Comp")
              )

            )
    ),
    # TAB 4 =     graphs dashboard: mult
    tabItem(tabName = "mult",
            
            sidebarLayout(
              
              # Define the sidebar with one input
              sidebarPanel(
                selectInput('sampleTG2', 'Sample Type:', choices = NULL,  selected = NULL),
                selectInput('px', 'X:', choices = NULL,  selected = NULL),
                selectInput('py', 'Y:', choices = NULL,  selected = NULL),
                selectInput('pz', 'Z:', choices = NULL,  selected = NULL), 
                selectInput('by_group2', 'Group by:', choices = NULL, selected = NULL)
              ),
              
              # Create a spot for the plot
              mainPanel(
                fluidRow(
                  column(6, plotlyOutput("p1")),
                  column(6, plotlyOutput("p2"))
                ),
                fluidRow(
                  box(title = "selected sample info",
                      width = 12,
                      div(style = 'overflow-x: scroll', DT::dataTableOutput('display.sel.da.tab')))
                )
              )
              
            )
    ),
    # # TAB 4 = report dashboard
    # tabItem(tabName = "report",
    #         fluidPage(
    #           box(width = 10,status = "success",
    #               shiny::includeMarkdown("README.md"))
    #         )
    # ),
    # TAB 5? = About
    tabItem(tabName = "about",
             fluidPage(
               box(width = 10,status = "success",
                   shiny::includeMarkdown("README.md"))
             )
    ), ## end of About tab
    # TAB 6 = Disclaimer
    tabItem(tabName = "disclaimer",
            fluidPage(
              box(width = 10,status = "success",
                  shiny::includeMarkdown("Disclaimer.md"))
            )
    ) ## end of tab 6
   )
)

dashboardPage(header, sidebar, body)

