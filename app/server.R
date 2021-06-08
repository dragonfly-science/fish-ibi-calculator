#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    df_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- fread(inFile$datapath, header = TRUE)
        return(df)
    })
    
    output$sample_table<- DT::renderDataTable({
        df <- df_upload()
        DT::datatable(df)
    })
    
    output$info <- renderUI({
        df <- df_upload()
        req(df)
        if ("date" %in% names(df)) {
            ico <- icon("check-circle")
            col <- 'green'
        } else {
            ico <- icon("exclamation-triangle")
            col <- 'red'
        }
        valueBox("",
                 subtitle = strong("Date (YYYY-MM-DD)"),
                 icon = ico,
                 color = col)
    })
})
