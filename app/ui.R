library(shiny)
library(shinydashboard)
library(DT)


shinyUI(fluidPage(

  # Header
  headerPanel(
    title=tags$a(tags$img(src='topimg.png'), target="_blank")),
    #tags$head(tags$script(HTML(js))),

    # Input widgets
  tabsetPanel(
    tabPanel("1. Upload your file",
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/welcome.rmd')),
               column(7, fileInput('target_upload', 'Upload CSV file',
                                   accept = '.csv'))),
             br(),
             hr(),
             br(),
             fluidRow(column(3, tags$img(src = 'square.png'), target= "_blank"),
                      column(7,includeMarkdown('./text/about.rmd'))),
             br(),
             hr(),
             br()),
    tabPanel("2. Match headers",
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/matching.rmd'))),
             br(),
             hr(),
             br(),
             fluidRow(
               column(12, strong("Mandatory fields:"))),
             fluidRow(
               column(6,
               uiOutput('info1'),
               uiOutput('info2'),
               uiOutput('info3')
               )),
             fluidRow(
               column(6,
               uiOutput('info4'),
               uiOutput('info5'),
               uiOutput('info6')
             )),
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
                 DT::DTOutput("table")
                 )
             ),
  tabPanel("3. Check input data"),
  tabPanel("4. Calculate IBI score")
    )
  )
)
