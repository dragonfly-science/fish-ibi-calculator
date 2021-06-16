library(shiny)
library(shinyjs)
library(data.table)
library(DT)

cols_needed <- c('Stratum', 'Penetration', 
                 'Altitude', 'SpeciesCode' 
                 )
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed
icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

rv <- NULL

callback <- "$(document).contextMenu({
    selector: '#dtable th',
    trigger: 'right',
    items: {
        // <input type=\"radio\">
        Stratum: {
            name: \"Stratum\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Stratum',
        },
        Penetration: {
            name: \"Penetration\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Penetration'
        },
        SpeciesCode: {
            name: \"Species code\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'SpeciesCode'
        },
        Altitude: {
            name: \"Altitude\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Altitude'
        },
        sep1: \"---------\",
        // <optional>
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
        },
        Location: {
            name: \"Location (opt)\",
            type: 'radio',
            radio: 'radio',
            value: 'Location'
        },
        NZreach: {
            name: \"NZreach (opt)\",
            type: 'radio',
            radio: 'radio',
            value: 'NZreach'
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
      if (typeof data.radio !== \"undefined\") {
        Shiny.onInputChange(\"selfield\", data.radio + \"=\" + $th.text());
      }
    }
  }
});"

shinyServer(function(input, output, session) {
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.frame(req = names(req_fields), good = 0L),
        intable = NULL, tablefields = NULL, tablefields_ori = NULL,
        cleanTable = NULL
    )
    
    observe({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
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
        updateCheckboxGroupInput(session, 'mandfields',
                                 selected = intersect(cols_needed, names(rv$intable)))
    })
    
    observeEvent(input$selfield, {
        if (!is.null(input$selfield)) {
            pair <- strsplit(input$selfield, '=')[[1]]
            sf <- isolate(rv[['selfields']])
            sf[sf$req == pair[1], 'good'] <- 1L
            rv[['selfields']] <- sf
            d <- isolate(rv[['intable']])
            if (pair[1] %in% names(d)) {
                names(d)[names(d) %in% pair[1]] <- paste0(names(d)[names(d) %in% pair[1]], '_0')
            }
            if (pair[2] %in% names(d)) {
                names(d)[names(d) == pair[2]] <- pair[1]
                rv[['tablefields']] <- names(d)
            }
            sf$good <- ifelse(sf$req %in% names(d), 1, 0)
            rv[['selfields']] <- sf
            rv[['intable']] <- d
            ## reloadData(tableProx, clearSelection='all')
        }
    })

    # data table on page 2
    output$dtable <- renderDT({
        d <- rv[['intable']]
        nms <- names(d)
        dt <- DT::datatable(d, callback = JS(callback),
                            colnames = rv[['tablefields']],
                            options = list(dom='t', ordering=F),
                            rownames = FALSE)
        sf <- rv[['selfields']]
        goodfields <- sf[sf$good == 1, 'req']
        dt <- dt %>% formatStyle(columns = goodfields, backgroundColor = "#E5F5E0")
        dt
    }, server = FALSE)  
    
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
    
    # button to go from tab 2 to 3
    observe({
      if(all(rv$selfields$good == 1)) {
        enable('checkData')
      } else{
        disable('checkData')
      }
    })
    
    # check data button
    observeEvent(input$checkData, {
      updateTabsetPanel(session, "myFirst",
                        selected = "3. Check input data")
    })
    
    # data table on page 3
    output$newTable <- renderDT({
      d <- rv[['cleanTable']]
      dt <- DT::datatable(d,
                          options = list(dom='t', ordering=F),
                          rownames = FALSE)
      dt
    }, server = FALSE)  
    
    observe({
        d <- rv$intable[, intersect(names(rv$intable), cols_needed)]
        
        ## d$issues <- NA
        
        ## if ('Year' %in% names(d)) {
            
        ## }
        rv$cleanTable <- d
    })
    
})
