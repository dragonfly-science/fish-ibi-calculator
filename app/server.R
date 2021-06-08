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

cols_needed <- c('Date', 'Distance', 
                 'Species code', 'Site',
                 'Elevation', 'Count')
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed
icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(reqfields = req_fields)
    
    df_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- fread(inFile$datapath, header = TRUE)
        return(df)
    })
    
    # data table on page 2
    output[["table"]] <- renderDT({
        datatable(df_upload(), callback = JS(callback),
                  options = list(dom='t'))
    }, server = FALSE)  
    
    # value boxes for required columns
    observe({
        df <- df_upload()
        for (c in cols_needed) {
            if (c %in% names(df)) {
                rv$reqfields[c] <- 1L
            } else rv$reqfields[c] <- 0L
        }
    })
    output$info1 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[1]] + 1L]
        col <- cols[rv$reqfields[cols_needed[1]] + 1L]
        valueBox(tags$p(strong(cols_needed[1]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(YYYY-MM-DD)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info2 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[2]] + 1L]
        col <- cols[rv$reqfields[cols_needed[2]] + 1L]
        valueBox(tags$p(strong(cols_needed[2]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(km from coast)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info3 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[3]] + 1L]
        col <- cols[rv$reqfields[cols_needed[3]] + 1L]
        valueBox(tags$p(strong(cols_needed[3]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(from NZFFD)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info4 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[4]] + 1L]
        col <- cols[rv$reqfields[cols_needed[4]] + 1L]
        valueBox(tags$p(strong(cols_needed[4]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(must be unique)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info5 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[5]] + 1L]
        col <- cols[rv$reqfields[cols_needed[5]] + 1L]
        valueBox(tags$p(strong(cols_needed[5]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(meters a.s.l.)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info6 <- renderUI({
        ico <- icons[rv$reqfields[cols_needed[6]] + 1L]
        col <- cols[rv$reqfields[cols_needed[6]] + 1L]
        valueBox(tags$p(strong(cols_needed[6]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(observ. per fish species)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })

})
