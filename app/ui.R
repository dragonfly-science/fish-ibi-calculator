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
        title = 'IBI Calculator',
        titlePanel(
            windowTitle = 'IBI calculator',
            title = NULL
        ),
        tags$header(id="header",
            fluidRow(id="header-div",
                column(4, class='header-col header-img-container',
                    (tags$a(href="https://environment.govt.nz/", target="_blank" , img(class='header-img', src="images/MFELogo.png")))
            ),
                column(4, class='header-col', id="title-container",
                    h1(
                        id="main-title", 
                        "Fish IBI Calculator"
                    ),
                ),
                column(4, class='header-col')
            ),
            a(class='cr-text', href='https://www.rodmorris.co.nz/', p('© Rod Morris'))
        ),
        useShinyjs(),  # Set up shinyjs
        
        mainPanel(
            width = 12, id = 'mainpanel',
            tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles/custom.css"),
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles/buttons.css"),
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles/tables.css")
                 ),

            ## Input widgets
            tabsetPanel(
                type = 'tabs', id = 'myFirst',

                ## * 1. Upload your file
                tabPanel(
                    "1. Upload your file",
                    tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href="styles/tabPanel1.css"),
                    ),
                    fluidRow(
                        br(),br(),
                        column(
                            6,
                            h2(includeMarkdown('text/page-1-title.md')),
                            br(),
                            h5(includeMarkdown('text/page-1-description.md'))
                        ),
                        column(
                            6,
                            fluidRow(
                                column(
                                    6,
                                    fileInputOnlyButton('target_upload',
                                                        buttonLabel =
                                                            div(class="button-inner", span(id="button-inner-text", 'Upload CSV file'), img(class="arrow-up", src="icons/buttonArrow.svg")),
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
                    div(id="outer-img-container",
                        div(id="inner-img-container",
                            tags$img(id="home-image",src="images/electric_fishing.png", width = '100%'),
                            h5(style="margin-right: auto;", includeMarkdown('text/page-1-image-caption.md'))
                        )
                    )
                ),

                tabPanel(img(src='icons/arrow.svg')),
                
                ## * 2. Match headers
                tabPanel(
                    "2. Match headers",
                    br(), br(),
                    fluidRow(
                        column(6,
                               h2(includeMarkdown('text/page-2-title.md')),
                               br(),
                               h5(includeMarkdown('text/page-2-description.md'))
                               ),
                        column(3, offset = 3 #, align = 'right'
                             , disabled(
                                   actionButton('checkData', div(class='button-inner', span("Check input data"), img(class='arrow-next', src='icons/buttonArrow.svg')))
                               )
                             , br(), br(), br()
                             , uiOutput('mandfields')
                               )
                    ),
                    ## fluidRow(verbatimTextOutput('logtxt')), # Comment-out this line if not testing
                    br(), br(),
                    fluidRow(
                        tags$head(
                                 tags$link(
                                         rel = "stylesheet",
                                        href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
                                 ),
                                 tags$link(
                                          rel = "stylesheet",
                                          href = "styles/contextMenu.css"
                                 ),
                                 tags$script(
                                          src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
                                      )
                             ),
                        ## div(class='table-container', withspinner(DT::DTOutput("dtable")))
                        fluidRow(column(12, align='center', withspinner(DT::DTOutput("dtable"))))
                    )
                ),
                tabPanel(img(src='icons/arrow.svg')),

                ## * 3. Check input data
                tabPanel(
                    "3. Check input data",
                    br(), br(),
                    fluidRow(
                        column(6, id='rescol1',
                               wellPanel(
                                   id = 'issues-panel',
                                   fluidRow(
                                       column(3, class='smallcol', align='center',
                                              uiOutput('issuesIcon')
                                             ),
                                              ## imageOutput('issueImg', height='100px', width='100px')),
                                       column(9, h2(htmlOutput('issuesTxt')))
                                   ),
                                   br(),
                                   h5(textOutput('issuesSubTxt'))
                               ),
                               ),
                        column(
                            6,
                            fluidRow(
                                column(
                                    6,
                                    id='rescol2', align = 'right',
                               actionButton('remIssuesBtn', 'Exclude issues')
                               ),
                               # br(),br(),
                               column(
                                   6,
                                   actionButton('to4btn', div(class="button-inner",
                                    span('Calculate IBI score'), 
                                    img(class='arrow-next', src='icons/buttonArrow.svg')
                                   ))
                               ## , actionButton('testbtn', 'Test')
                               )
                            )
                        )
                    ),
                    br(), br(),
                    withspinner(DT::DTOutput("newTable"))
                ),
                ## tabPanel('>'),
                tabPanel(img(src='icons/arrow.svg')),
                
                ## * 4. Calculate IBI score
                tabPanel(
                    "4. Calculate IBI score",
                    tags$head(
                        tags$link(rel="stylesheet", type="text/css", href="styles/tabPanel4.css")
                    ),
                    br(),br(),
                    fluidRow(
                        column(
                            8,
                            div(style="margin-bottom: 40px;",
                                h2(includeMarkdown('text/page-4-title.md'))
                            ),
                            div(id="select-container",
                                span(class="select-label", HTML("View by: "), style="height: 100%; margin-top: auto; margin-bottom: auto;"),
                                span(class="select-menu",
                                     selectInput("sel_score", "",
                                                 choices = c("NPS-FM category" = 'nps_score',
                                                             "IBI score" = 'ibi_score')))## ,
                                ## span(id="select-2", selectInput("download2", "", choices = c("NPS-FM category")))
                            )
                        ),
                        column(
                            4, align='right',
                            downloadButton('download', 
                                           label=span(id="dl-button-label", "Download Results",
                                                     img(class='arrow-download', src='icons/buttonArrow.svg')), icon=NULL),
                        )
                    ),
                    br(),
                    hr(), 
                    ##br(),
                    fluidRow(column(6
                                  , fluidRow(column(8, div(class='subheader', "Scores across number of sites")),
                                             column(4, downloadButton('plotdl', 'Download plot')))
                                  , div(class="key-line", withspinner(plotOutput("scoresPlot"))))
                           , column(6
                                  , fluidRow(column(8, div(class = 'subheader', "Map of locations")),
                                             column(4, downloadButton('mapdl', 'Download map')))
                                  , div(class="key-line", withspinner(leafletOutput('map'))))
                             ),
                    br(), br(),
                    fluidRow(
                        uiOutput('text')
                        # splitLayout(cellWidths = rep("20%", 5),
                        #             cellArgs = list(style='white-space: normal;'),
                        #             column(width= 12, 
                        #                    h5(strong("A")),
                        #                    h5("≥ 34"),
                        #                    hr(),
                        #                    h6(includeMarkdown('text/nps-a.md'))),
                        #             column(width= 12, 
                        #                    h5(strong("B")),
                        #                    h5("< 34 and ≥ 28"),
                        #                    hr(),
                        #                    h6(includeMarkdown('text/nps-b.md'))),
                        #             column(width= 12, 
                        #                    h5(strong("C")),
                        #                    h5("< 28 and ≥ 18"),
                        #                    hr(),
                        #                    h6(includeMarkdown('text/nps-c.md'))),
                        #             column(width= 12, 
                        #                    h5(strong("D")),
                        #                    h5("< 18"),
                        #                    hr(),
                        #                    h6(includeMarkdown('text/nps-d.md'))),
                        #             column(width= 12, 
                        #                    h5(strong("No fish")),
                        #                    h6(h5("-")),
                        #                    hr())
                        #             )
                    ),
                    br(),
                    hr(),
                    br(),
                    fluidRow(h5(class="subheader", "Table of results")),
                    br(),
                    fluidRow(withspinner(DT::DTOutput("ibiTable")))
                    )
                    ),
                    
                    span(
                        br(),br(), br(),
                        tags$footer(id='footer',
                            fluidRow(align = "center", id = "footer-div",
                                column(1, div()),
                                column(2, align = "center", 
                                div(class="footer-img-container", tags$a(href="https://environment.govt.nz/", target="_blank", img(class="footer-img", src="images/MFELogo.png")))
                                ),
                                column(2, offset = 6,
                                div(class="footer-img-container", img(class="footer-img", src="images/nzgovlogo.png"))
                                ),
                                column(1, div())
                            )
                        )
                    )
                )
                
    )
)
