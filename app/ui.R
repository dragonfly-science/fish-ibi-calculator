library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(data.table)
library(leaflet)
library(reactable)
library(sass)

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

mydownloadbutton <- function(outputId, label = "Download", class = NULL, ...,
                             icon = shiny::icon("arrow-down")) {
  aTag <- tags$a(id = outputId, class = paste("my-dl-btn shiny-download-link", class),
                 href = "", target = "_blank", download = NA, shiny:::validateIcon(icon), span(label), ...)
}

withspinner <- function(...)  withSpinner(..., type = 5, color="#003547")

## Compile the scss into css
sass(
  sass_file('styles/main.scss'),
  cache = FALSE,
  output = 'www/main.css',
  options = sass_options(
    output_path = 'www/main.css',
    output_style = "compressed",
    source_map_embed = TRUE
  )
)

shinyUI(
  fluidPage(
    useShinydashboard(),
    title = 'IBI Calculator',
    titlePanel(
      windowTitle = 'IBI calculator',
      title = NULL
    ),
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css"),
      # tags$style(css)
    ),
    tags$header(id="header",
                fluidRow(id="header-div",
                         column(4, class = 'header-col header-img-container',
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
      tags$script(src = 'js/headers.js'),
      ## Input widgets
      tabsetPanel(
        type = 'tabs', id = 'myFirst',

        ## * 1. Upload your file
        tabPanel(
          "1. Upload your file",
          fluidRow(
            br(), br(),
            column(
              6,
              h2(includeMarkdown('text/page-1-title.md')),
              br(),
              p(includeMarkdown('text/page-1-description.md')),
              br(),
              selectInput(inputId  = "region",
                multiple = FALSE,
                label    = div(class = 'input-label', "Select your region (optional):"),
                choices  = c("No Region","Northland","Auckland","Waikato","Bay of Plenty","Gisborne","Hawke's Bay","Taranaki","Manawatū-Whanganui","Wellington","West Coast","Canterbury","Otago","Southland","Tasman","Nelson","Marlborough"))
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
              h6(includeMarkdown('text/page-1-requirements.md'))
            )
          ),
          hr(),
          br(),
          h3(includeMarkdown('text/page-1-about-title.md')),
          div(class = 'twocols', includeMarkdown('text/page-1-about.md')),
          br(), br(),
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
                   p(includeMarkdown('text/page-2-description.md'))
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
              tags$script(
                src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
              )
            ),
            ## div(class='table-container', withspinner(DT::DTOutput("dtable")))
            ## fluidRow(column(12, align='center', withspinner(DT::DTOutput("dtable"))))
            ## fluidRow(column(12, align='center', withspinner(tableOutput("dtable"))))
            fluidRow(column(12, align='center', withspinner(reactableOutput("dtable"))))
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
                  ),
            ),
            column(
              6,
              fluidRow(
                column(
                  6,
                  id='rescol2', align = 'right',
                  actionButton('remIssuesBtn', 'Exclude issues'),
                ),
                column(
                  6,
                  actionButton('to4btn', div(class="button-inner",
                    span('Calculate IBI score'), 
                    img(class='arrow-next', src='icons/buttonArrow.svg')
                    )),
                )
              )
            )
          ),
          fluidRow(
            column(6,
              htmlOutput('issuesSubTxt')
            ),
            column(3,
              div(id="issue_type_container", uiOutput('issue_type'))
            ),
            column(3,
              downloadButton(
                'downloadissues', class = "shinyjs-disabled dl-issues-btn",
                label=span(class="dl-issues-btn--label", "Download All Issues",
                img(class='dl-issues-btn--label__arrow', src='icons/smButtonArrow.svg')), icon=NULL
              )
            )
          ),
          br(),
          ## , fluidRow(verbatimTextOutput('logtxt')) # Comment-out this line if not testing
          div(style="margin-top: 2rem;", withspinner(reactableOutput("newTable")))
          ## , withspinner(DTOutput("newTable"))
        ),
        ## tabPanel('>'),
        tabPanel(img(src='icons/arrow.svg')),
        
        ## * 4. Calculate IBI score
        tabPanel(
          "4. Calculate IBI score",
          br(), br(),
          fluidRow(
            column(
              8,
              div(style="margin-bottom: 20px;",
                  h2(includeMarkdown('text/page-4-title.md')),
                  fluidRow(column(4, div(style="margin-top: 2.5rem;",
                      uiOutput('view_region_only'))),
                  column(7, offset=1, div(style="margin-top: 2.5rem;",
                      uiOutput('aggregate_site_visits'))))
                  )
            ),
            column(
              4, align='right',
              downloadButton('download', 
                  label=span(class="dl-button-label", "Download Results",
                  img(class='arrow-download', src='icons/buttonArrow.svg')), icon=NULL),
              )
          ),
          br(),
          hr(),
          ##br(),
          fluidRow(column(6
                        , fluidRow(column(9, div(class='subheader', textOutput('plot_title'))),
                                   column(3, style = "text-align: right;", mydownloadbutton('plotdl', 'Download plot')))
                        , div(class="key-line", withspinner(plotOutput("scoresPlot"))))
                 , column(6
                        , fluidRow(column(9, div(class = 'subheader', "Map of locations")),
                                   column(3, style = "text-align: right;", mydownloadbutton('mapdl', 'Download map')))
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
          div(h2(class="subheader", "Table of results")),
          fluidRow(withspinner(reactableOutput("ibiTable")))
        )
      ),
      
      span(
        br(), br(), br(),
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
      ),
    )
  )
)
