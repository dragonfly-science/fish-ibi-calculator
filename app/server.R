library(shiny)
library(shinyjs)
library(data.table)

cols_needed <- c('Month', 'Year', 'Distance', 
                 'SpeciesCode', 'Site',
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
            value: 'Month',
        },
        Year: {
            name: \"Year\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Year',
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
      $th.text(data.radio);
    }
  }
});"

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.frame(req = names(req_fields), ori = NA_character_, good = 0L),
        intable = NULL, tablefields = NULL, tablefields_ori = NULL
    )
    
    observe({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields[fields$req %in% names(df), 'ori'] <- fields[fields$req %in% names(df), 'req']
        rv[['selfields']] <- fields
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
    })

    output$mandfields <- renderUI({
        disabled(checkboxGroupInput('mandfields',  label = NULL, choices = cols_needed,
                                    selected = intersect(cols_needed, names(rv$intable))))
    })
    observe({
        updateCheckboxGroupInput(session, 'mandfields', selected = intersect(cols_needed, names(rv$intable)))
    })
    observeEvent(input$selfield, {
        req(rv)
        if (!is.null(input$selfield)) {
            pair <- strsplit(input$selfield, '=')[[1]]
            sf <- rv[['selfields']]
            ## if (sf[sf$req == pair[1], 'ori'] %in% rv$tablefields_ori) {
            sf[sf$req == pair[1], 'ori'] <- pair[2]
            ## }
            sf[sf$req == pair[1], 'good'] <- 1L
            rv[['selfields']] <- sf
            d <- rv[['intable']]
            print(pair)
            print(names(d))
            if (pair[2] %in% names(d)) {
                names(d)[names(d) == pair[2]] <- pair[1]
                rv[['tablefields']] <- names(d)
            }
            print(names(d))
            rv[['intable']] <- d
            ## reloadData(tableProx, clearSelection='all')
        }
    })

    # data table on page 2
    output$dtable <- renderDT({
        d <- rv[['intable']]
        nms <- names(d)
        
        DT::datatable(d, callback = JS(callback),
                      colnames = rv[['tablefields']],
                      options = list(dom='t', ordering=F),
                      rownames = FALSE)
    }) #, server = FALSE)  
    
    output$testnum <- renderUI({
        numericInput('testnum', 'Sum(good)', value=sum(rv$selfields$good))
    })

    output$logtxt <- renderPrint({
        req(rv)
        cat('input$selfield=\n')
        print(input$selfield)
        cat('\n\nrv$selfields=\n')
        print(rv$selfields)
        cat('\n\nnames(rv$intable)=\n')
        print(names(rv[['intable']]))
        cat('\n\ntablefields=\n')
        print(rv[['tablefields']])
        ## str(rv[['intable']])
        cat('\n\nok=\n')
        d <- rv[['selfields']]
        print(d[d$req==cols_needed[1], 'good'] + 1L)
    })
    
})
