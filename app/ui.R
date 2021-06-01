#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(

  # Header
  headerPanel(
    title=tags$a(tags$img(src='topimg.png'), target="_blank")),


  # Input widgets
  tabsetPanel(
    tabPanel("1. Upload your file",
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/welcome.rmd')),
               column(7, fileInput('target_upload', 'Upload CSV file',
                                   accept = c(
                                     'text/csv',
                                     'text/comma-separated-values',
                                     '.csv'
                                   )),
                      radioButtons("separator","Separator: ",
                                   choices = c(";",",",":"),
                                   selected=",",inline=TRUE)
               )
             ),
             br(),
             hr(),
             br(),

             fluidRow(column(3, tags$img(src = 'square.png'), target= "_blank"),
                      column(7,includeMarkdown('./text/about.rmd'))),
             br(),
             hr(),
             br()

    ),
    tabPanel("2. Match headers",
             column(12,  DT::dataTableOutput("sample_table"))
    )
  )
)
)
