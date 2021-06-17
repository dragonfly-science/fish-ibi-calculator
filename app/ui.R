library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)

fileInputOnlyButton <- function(..., label="") {
  temp <- fileInput(..., label=label)
  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  temp$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$attribs$class <- NULL
  temp
}

shinyUI(
    fluidPage(

        useShinydashboard(),
        title = 'IBI calculator',
        ## Header
        titlePanel(
            windowTitle = 'IBI calculator',
            title = span(
                tags$img(src="header.png", width = '100%')
            )
        ),
        
        ## headerPanel(
        ##     title=tags$a(tags$img(src='header.png'), target="_blank")
        ## ),
        ##tags$head(tags$script(HTML(js))),
        useShinyjs(),  # Set up shinyjs

        mainPanel(width = 12, id = 'mainpanel',
                  tags$head(
                           tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                       ),

                  ## Input widgets
                  tabsetPanel(
                      type = 'tabs', id = 'myFirst',

                      ## * 1. Upload your file
                      tabPanel(
                          "1. Upload your file",
                          fluidRow(
                              br(),
                              column(
                                  6,
                                  includeMarkdown('./text/welcome.rmd')
                              ),
                              column(
                                  5, offset = 1,
                                  fluidRow(
                                      column(6,
                                             fileInputOnlyButton('target_upload', 
                                                                 buttonLabel='Upload CSV file',
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values",
                                                                            ".csv"), width='100%')),
                                      column(6,
                                             actionButton('exbtn', 'Use demo table'))
                                  ),
                                  br(), 
                                  includeMarkdown('./text/needed.rmd')
                              )
                          ),
                          br(),
                          hr(),
                          br(),
                          includeMarkdown('./text/about.rmd'),
                          br(),
                          span(
                              tags$img(src="fishing.png", width = '100%')
                          )
                      ),
                      
                      ## * 2. Match headers
                      tabPanel(
                          "2. Match headers",
                          fluidRow(
                              br(),
                              column(5,
                                     includeMarkdown('./text/matching.rmd')
                                     ),
                              column(4, offset = 1,
                                     br(), br(),
                                     strong("Mandatory fields:"),
                                     uiOutput('mandfields')
                                     )
                            , disabled(
                                  column(2,
                                         br(), br(),
                                         actionButton('checkData', "Check input data")
                                         )
                              )
                              ## , column(4, box(width = 12, verbatimTextOutput('logtxt'))) # Comment-out this line if not testing
                          ),
                          br(),
                          fluidRow(
                              tags$head(
                                       tags$link(
                                                rel = "stylesheet",
                                                href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
                                            ),
                                       tags$script(
                                                src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
                                            )
                                   ),
                              DT::DTOutput("dtable")
                          )
                      ),

                      ## * 3. Check input data
                      tabPanel(
                          "3. Check input data",
                          br(), br(),
                          fluidRow(
                              column(6,
                                     wellPanel(
                                         id = 'issues-panel',
                                         fluidRow(
                                             column(3, imageOutput('issueImg', height='100px', width='100px')),
                                             column(9, h2(htmlOutput('issuesTxt')))
                                         ),
                                         p(textOutput('issuesSubTxt'))
                                     ),
                                     ),
                              column(2, offset=4,
                                     actionButton('remIssuesBtn', 'Ignore issues'),
                                     br(),br(),
                                     actionButton('to4btn', 'Calculate IBI score')
                                     ## , actionButton('testbtn', 'Test')
                                     )
                          ),
                          fluidRow(DT::DTOutput("newTable"))
                      ),
            ## * 4. Calculate IBI score
            tabPanel("4. Calculate IBI score",
                     fluidRow(DT::DTOutput("ibiTable")))
        )
    )
    )
)
