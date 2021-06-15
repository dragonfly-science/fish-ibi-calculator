library(shiny)
library(data.table)

cols_needed <- c('m', 'y', 'Distance', 
                 'spcode', 'Site',
                 'Elevation', 'Count')
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed
icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

rv <- NULL

callback <- "$(document).contextMenu({
    selector: '#dtable th',
    trigger: 'right',
    autoHide: true,
    items: {
        // <input type=\"radio\">
        Month: {
            name: \"Month\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'm',
        },
        Year: {
            name: \"Year\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'y',
        },
        Distance: {
            name: \"Distance\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Distance'
        },
        SpeciesCode: {
            name: \"Species code\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'spcode'
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
      $th.text(data.radio);
      Shiny.onInputChange(\"selfield\", data.radio + \"=\" + $th.text());
    }
  }
});"

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.frame(req = names(req_fields), ori = NA_character_, good = 0L),
        intable = NULL)
    
    observe({
        req(rv)
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- fread(inFile$datapath, header = TRUE)
        fields <- rv[['selfields']]
        fields[fields$req %in% names(df), 'ori'] <- fields[fields$req %in% names(df), 'req']
        rv[['selfields']] <- fields
        rv[['intable']] <- df
    })

    observeEvent(input$selfield, {
        req(rv)
        if (!is.null(input$selfield)) {
            pair <- strsplit(input$selfield, '=')[[1]]
            sf <- rv[['selfields']]
            sf[sf$req == pair[1], 'ori'] <- pair[2]
            sf[sf$req == pair[1], 'good'] <- 1L
            rv[['selfields']] <- sf
            d <- rv[['intable']]
            if (pair[2] %in% names(d)) {
                names(d)[names(d) == pair[2]] <- pair[1]
            }
            rv[['intable']] <- d
        }
    })

    # data table on page 2
    output$dtable <- renderDT({
        d <- rv[['intable']]
        DT::datatable(d, callback = JS(callback),
                  options = list(dom='t'),
                  rownames = FALSE)
    }) #, server = FALSE)  
    tableProx <- DT::dataTableProxy('dtable')

    
    output$testnum <- renderUI({
        numericInput('testnum', 'Sum(good)', value=sum(rv$selfields$good))
    })
    output$info1 <- renderValueBox({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[1], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong('Month'), style = "font-size: 70%;"),
                subtitle = "",
                 icon = icon(ico), color = col)
    })
    output$year <- renderValueBox({
      d <- rv[['selfields']]
      ok <- d[d$req==cols_needed[2], 'good'] + 1L
      ico <- icons[ok]
      col <- cols[ok]
      valueBox(tags$p(strong('Year'), style = "font-size: 70%;"),
                subtitle ="",
               icon = icon(ico), color = col)
    })
    output$info2 <- renderValueBox({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[3], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[3]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(km from coast)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info3 <- renderUI({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[4], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong("Species code"), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(from NZFFD)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info4 <- renderUI({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[5], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[5]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(must be unique)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info5 <- renderUI({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[6], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[6]), style = "font-size: 70%;"),
                 subtitle = strong(tags$p("(meters a.s.l.)", style = "font-size: 70%;")),
                 icon = icon(ico), color = col)
    })
    output$info6 <- renderUI({
        d <- rv[['selfields']]
        ok <- d[d$req==cols_needed[7], 'good'] + 1L
        ico <- icons[ok]
        col <- cols[ok]
        valueBox(tags$p(strong(cols_needed[7]), style = "font-size: 70%;"),
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
        ## str(rv[['intable']])
        cat('\n\nok=\n')
        d <- rv[['selfields']]
        print(d[d$req==cols_needed[1], 'good'] + 1L)
    })
    
})
