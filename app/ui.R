library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(leaflet)

fileInputOnlyButton <- function(..., label="") {
    temp <- fileInput(..., label=label)
    temp$children[[1]] <- NULL ## Cut away the label
    ## Cut away the input field (after label is cut, this is position 1 now)
    temp$children[[1]]$children[[2]] <- NULL
    ## Remove input group classes (makes button flat on one side)
    temp$children[[1]]$attribs$class <- NULL
    temp$children[[1]]$children[[1]]$attribs$class <- NULL
    temp
}

withspinner <- function(...)  withSpinner(..., type = 5, color="#003547")

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
        
        useShinyjs(),  # Set up shinyjs
        
        mainPanel(
            width = 12, id = 'mainpanel',
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
                        br(),br(),
                        column(
                            6,
                            h2(includeMarkdown('text/page-1-title.md')),
                            br(),
                            h5(includeMarkdown('text/page-1-description.md'))
                        ),
                        column(
                            5, offset = 1,
                            fluidRow(
                                column(
                                    6,
                                    fileInputOnlyButton('target_upload', 
                                                        buttonLabel =
                                                            div(HTML('Upload CSV file&nbsp;&nbsp;'), icon('upload')),
                                                        accept = c("text/csv",
                                                                   "text/comma-separated-values",
                                                                   ".csv"), width='100%')),
                                column(6,
                                       actionButton('exbtn', 'Use demo table'))
                            ),
                            br(), 
                            h6(includeMarkdown('text/page-1-requirements.md'))
                        )
                    ),
                    br(),
                    hr(),
                    br(),
                    h3(includeMarkdown('text/page-1-about-title.md')),
                    div(class = 'twocols', h5(includeMarkdown('text/page-1-about.md'))),
                    br(),br(),
                    span(
                        tags$img(src="fishing.png", width = '100%'),
                        h5(includeMarkdown('text/page-1-image-caption.md'))
                    )
                ),
                ## div(class='tab-pane disabled', id = 'sep1', title = '>'),
                ## HTML("<li>\></li>"), #tabPanel('>'),

                ## tabPanel('>', id='sep1'),
                ## tabPanel(' ', id='sep1'),
                tabPanel(icon('menu-right', class='arrowico', lib='glyphicon'), id='sep1'),
                ## tabPanel(div('a', class = 'arrow'), id='sep1'),
                ## tabPanel(title=tags$img(src="arrow-right.png"), id='sep1'),
                
                ## * 2. Match headers
                tabPanel(
                    "2. Match headers",
                    br(), br(),
                    fluidRow(
                        column(7,
                               h2(includeMarkdown('text/page-2-title.md')),
                               br(),
                               h5(includeMarkdown('text/page-2-description.md'))
                               ),
                        column(3, offset = 2 #, align = 'right'
                             , disabled(
                                   actionButton('checkData', "Check input data")
                               )
                             , br(), br(), br()
                             , uiOutput('mandfields')
                               )
                    ),
                    ## , column(4, box(width = 12, verbatimTextOutput('logtxt'))) # Comment-out this line if not testing
                    br(), br(),
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
                ## tabPanel('>'),
                tabPanel(icon('menu-right', class='arrowico', lib='glyphicon'), id='sep1'),

                ## * 3. Check input data
                tabPanel(
                    "3. Check input data",
                    br(), br(),
                    fluidRow(
                        column(9, id='rescol1',
                               wellPanel(
                                   id = 'issues-panel',
                                   fluidRow(
                                       column(3, class='smallcol', align='center',
                                              uiOutput('issuesIcon')
                                             ),
                                              ## imageOutput('issueImg', height='100px', width='100px')),
                                       column(9, h2(htmlOutput('issuesTxt')))
                                   ),
                                   h5(textOutput('issuesSubTxt'))
                               ),
                               ),
                        column(3, id='rescol2', align = 'right',
                               actionButton('remIssuesBtn', 'Exclude issues'),
                               br(),br(),
                               actionButton('to4btn', 'Calculate IBI score')
                               ## , actionButton('testbtn', 'Test')
                               )
                    ),
                    br(), br(),
                    fluidRow(DT::DTOutput("newTable"))
                ),
                ## tabPanel('>'),
                tabPanel(icon('menu-right', class='arrowico', lib='glyphicon'), id='sep1'),
                
                ## * 4. Calculate IBI score
                tabPanel(
                    "4. Calculate IBI score",
                    br(),br(),
                    fluidRow(
                        column(
                            7,
                            h2(includeMarkdown('text/page-4-title.md'))
                        ),
                        column(
                            5, align='right',
                            downloadButton('download', 
                                           label=div(HTML("Download results&nbsp;&nbsp;"),
                                                     icon("download"), style='display: inline-block !important'),
                                           icon = icon(""), style='width: 250px; height: 55px')
                        )
                    ),
                    br(), br(),
                    fluidRow(column(2, "View by"),
                             column(width = 3,
                                    selectInput("download", "", choices = c("NPS-FM category"))),
                             column(width = 3,
                                    selectInput("download", "", choices = c("NPS-FM category"))),
                             offset = 4),
                    br(),
                    hr(), 
                    ##br(),
                    fluidRow(column(6
                                  , strong("Scores across number of sites"), br(), br()
                                  , withspinner(plotOutput("npsGraph")))
                           , column(6
                                  , strong("Map of locations"), br(), br()
                                  , withspinner(leafletOutput('map')))
                             ),
                    ## br(),
                    ## fluidRow(column(width = 6, )),
                    br(), br(),
                    fluidRow(
                        column(width= 2, 
                               h5(strong("A")),
                               h5("≥ 34"),
                               hr(),
                               includeMarkdown('text/nps-a.md')),
                        column(width= 2, 
                               h5(strong("B")),
                               h5("< 34 and ≥ 28"),
                               hr(),
                               includeMarkdown('text/nps-b.md')),
                        column(width= 2, 
                               h5(strong("C")),
                               h5("< 28 and ≥ 18"),
                               hr(),
                               includeMarkdown('text/nps-c.md')),
                        column(width= 2, 
                               h5(strong("D")),
                               h5("< 18"),
                               hr(),
                               includeMarkdown('text/nps-d.md')),
                        column(width= 2, 
                               h5(strong("No fish")),
                               h5("-"),
                               hr())
                    ),
                    br(),
                    hr(),
                    br(),
                    fluidRow(strong("Table of results")),
                    br(),
                    fluidRow(withspinner(DT::DTOutput("ibiTable"))),
                    )
                    ),
                    
                    span(
                        br(),br(), br(),
                        tags$img(src="footer.png", width = '100%')
                    )
                )
                
    )
)
