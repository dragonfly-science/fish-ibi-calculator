library(shiny)
library(data.table)

cols_needed <- c('Date', 'Distance', 
                 'Species code', 'Site',
                 'Elevation', 'Count')
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed
icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

rv <- NULL

callback <- "$(document).contextMenu({
    selector: '#table th',
    trigger: 'right',
    autoHide: true,
    items: {
        // <input type=\"radio\">
        Date: {
            name: \"Date\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Date',
        },
        Distance: {
            name: \"Distance\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Distance'
        },
        SpeciesCode: {
            name: \"SpeciesCode\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'SpeciesCode'
        },
        Site: {
            name: \"Site\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Site'
        },
        Elevation: {
            name: \"Elevation\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Elevation'
        },
        Count: {
            name: \"Count\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Count'
        },
        Easting: {
            name: \"Easting (opt)\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Easting'
        },
        Northing: {
            name: \"Northing (opt)\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Northing'
        }
    },
  events: {
    show: function(opt) {
      var $this = this;
      $.contextMenu.setInputValues(opt, $this.data());
    }, 
    hide: function(opt) {
      var $this = this;
      var data = $.contextMenu.getInputValues(opt, $this.data());
      var $th = opt.$trigger;
      Shiny.onInputChange(\"selfield\", data.radio + \"=\" + $th.text());
      /* $th.text(data.radio); */
    }
  }
});"

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.table(req = names(req_fields), ori = NA_character_, good = 0L, key='req'),
        intable = NULL)
    
    observe({
        req(rv)
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- fread(inFile$datapath, header = TRUE)
        fields <- rv[['selfields']]
        fields[req %in% names(df), ori := req] # automatically pick the right columns
        rv[['selfields']] <- fields
        rv[['intable']] <- df
    })

    observeEvent(input$selfield, {
        req(rv)
        if (!is.null(input$selfield)) {
            pair <- strsplit(input$selfield, '=')[[1]]
            d <- rv[['selfields']]
            d[pair[1], `:=`(ori = pair[2], good = 1L)]
            rv[['selfields']] <- d
            d <- rv[['intable']]
            setnames(d, pair[2], pair[1])
            rv[['intable']] <- d
        }
    })

    ## observeEvent(rv[['selfields']], {
    ##     tochange <- rv[['selfields']][!is.na(ori)]
    ##     d <- rv[['intable']]
    ##     for (i in seq_len(nrow(tochange))) {
    ##         setnames(d, tochange[i, ori], tochange[i, req])
    ##     }
    ##     rv[['intable']] <- d
    ## })

    # data table on page 2
    output$table <- renderDT({
        req(rv)
        d <- rv[['intable']]
        datatable(d, callback = JS(callback),
                  options = list(dom='t'),
                  rownames = FALSE)
    }, server = FALSE)  
    tableProx <- dataTableProxy('table')

    observe({
        req(rv)
        d <- rv[['intable']]
        replaceData(tableProx, d)
    })
    # value boxes for required columns
    ## observe({
    ##     df <- rv[['intable']]
    ##     for (c in cols_needed) {
    ##         if (c %in% names(df)) {
    ##             rv$reqfields[c] <- 1L
    ##         } else rv$reqfields[c] <- 0L
    ##     }
    ## })
    output$info1 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[1], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[1]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(YYYY-MM-DD)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info2 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[2], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[2]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(km from coast)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info3 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[3], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[3]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(from NZFFD)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info4 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[4], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[4]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(must be unique)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info5 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[5], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[5]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(meters a.s.l.)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info6 <- renderUI({
        req(rv)
        ok <- rv$selfields[cols_needed[6], good + 1L]
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[6]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(observ. per fish species)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })

    output$logtxt <- renderPrint({
        req(rv)
        cat('input$selfield=\n')
        print(input$selfield)
        cat('\n\nrv$selfields=\n')
        print(rv$selfields)
        cat('\n\nnames(rv$intable)=\n')
        print(names(rv[['intable']]))
        cat('\n\nok=\n')
        print(rv$selfields[cols_needed[1], good + 1L])
    })
    
})
