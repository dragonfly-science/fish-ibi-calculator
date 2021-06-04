#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

## this is not quite working re the data and field

js <- "
Shiny.addCustomMessageHandler('anim',
 function(x){

    var $icon = $('div.small-box i.fa');
    if(x == NULL && $icon.hasClass('fa-check-circle')){
      $icon.removeClass('fa-check-circle').addClass('fas fa-exclamation-triangle');
    }
    if(x != NULL && $icon.hasClass('fas fa-exclamation-triangle')){
      $icon.removeClass('fas fa-exclamation-triangle').addClass('fa-check-circle');
    }

    var $s = $('div.small-box div.inner h3');
    var o = {value: 0};
    $.Animation( o, {
        value: x
      }, {
        duration: 1500
        //easing: 'easeOutCubic'
      }).progress(function(e) {
          $s.text('$' + (e.tweens[0].now).toFixed(1));
    });

  }
);"

shinyUI(fluidPage(

  # Header
  headerPanel(
    title=tags$a(tags$img(src='topimg.png'), target="_blank")),
    tags$head(tags$script(HTML(js))),

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
             fluidRow(
               br(),
               column(3,includeMarkdown('./text/matching.rmd'))),
             br(),
             hr(),
             br(),
             fluidRow(
               column(12, strong("Mandatory fields:"))),
             fluidRow(
               valueBox("", subtitle = strong("Date (YYYY-MM-DD)"),
                        icon = icon("check-circle")
               )
             ),
             br(),
             #actionButton("btn", "Change value"),
               fluidRow(
               column(12,  DT::dataTableOutput("sample_table"))
)
             ),
  tabPanel("3. Check input data"),
  tabPanel("4. Calculate IBI score")

    )
  )
)
