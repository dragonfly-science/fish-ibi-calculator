library(shiny)
library(data.table)

cols_needed <- c('Date', 'Distance', 
                 'Species code', 'Site',
                 'Elevation', 'Count')
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed
icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

callback <- "$.contextMenu({
    selector: '#table th',
    trigger: 'right',
    autoHide: true,
    items: {
        // <input type=\"radio\">
        Date: {
            name: \"Date\", 
            type: 'radio', 
            radio: 'radio', 
            value: '1'
        },
        Distance: {
            name: \"Distance\", 
            type: 'radio', 
            radio: 'radio', 
            value: '2'
        },
        SpeciesCode: {
            name: \"SpeciesCode\", 
            type: 'radio', 
            radio: 'radio', 
            value: '3'
        },
        Site: {
            name: \"Site\", 
            type: 'radio', 
            radio: 'radio', 
            value: '4'
        },
        Elevation: {
            name: \"Elevation\", 
            type: 'radio', 
            radio: 'radio', 
            value: '5'
        },
        Count: {
            name: \"Count\", 
            type: 'radio', 
            radio: 'radio', 
            value: '6'
        },
        Easting: {
            name: \"Easting (opt)\", 
            type: 'radio', 
            radio: 'radio', 
            value: '7'
        },
        Northing: {
            name: \"Northing (opt)\", 
            type: 'radio', 
            radio: 'radio', 
            value: '8'
        }
    },
  events: {
    show: function(opt) {
      var $this = this;
      $.contextMenu.setInputValues(opt, $this.data());
    }, 
    hide: function(opts) {
      var $this = this;
      var data = $.contextMenu.getInputValues(opts, $this.data());
      var $th = opts.$trigger;
      $th.text = data.value;
    }
  }
});"

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
                  options = list(dom='t'),
                  rownames = FALSE)
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
