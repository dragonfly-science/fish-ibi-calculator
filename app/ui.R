library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyWidgets)


shinyUI(fluidPage(

    useShinydashboard(),
    
  # Header
  headerPanel(
    title=tags$a(tags$img(src='topimg.png'), target="_blank")),
    #tags$head(tags$script(HTML(js))),
  useShinyjs(),  # Set up shinyjs

    # Input widgets
  tabsetPanel(
    tabPanel("1. Upload your file",
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/welcome.rmd')),
               column(7, fluidRow( 
                      fileInput('target_upload', 'Upload CSV file',
                                   accept = '.csv')),
                      fluidRow(includeMarkdown('./text/needed.rmd')))),
             br(),
             hr(),
             br(),
             column(3,fluidRow(includeMarkdown('./text/about.rmd')),
                    br(),
                    fluidRow(tags$img(src = 'fishing.png'), target="_blank"),
                    br())),
    tabPanel("2. Match headers",
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/matching.rmd')),
               column(3,
                      strong("Mandatory fields:"),
                      uiOutput('mandfields')
                      )
             , column(6, box(width = 12, verbatimTextOutput('logtxt'))) # Comment-out this line if not testing
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
  tabPanel("3. Check input data"),
  tabPanel("4. Calculate IBI score")
    )
  )
)
