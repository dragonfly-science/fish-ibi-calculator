library(shiny)
library(shinyjs)
library(data.table)
library(DT)

cols_needed <- c('Stratum', 'Penetration', 
                 'Altitude', 'SpeciesCode' 
                 )
req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed

cols_opt <- c('Easting', 'Northing', 'Location', 'NZreach')

icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

rv <- NULL

## df <- read.csv('~/Downloads/goodtable.csv', stringsAsFactors=F)

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
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
    })
    observeEvent(input$exbtn, {
        df <- read.csv('data/demo-table.csv', header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
        rv[['selfields']] <- fields
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
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

    observeEvent(input$to4btn, {
      updateTabsetPanel(session, "myFirst",
                        selected = "4. Calculate IBI score")
    })
    
    
    
    ## * Table cleaned and showing errors
    observe({
        req(rv$intable)
        d <- rv$intable[, c(cols_needed[cols_needed %in% names(rv$intable)],
                            cols_needed[cols_opt %in% names(rv$intable)])]

        ## ** Penetration
        ## *** Check for non-numeric values
        c <- !grepl('^[0-9.]+$', d$Penetration)
        d[c, 'Penetration_issues'] <- 1L
        d[c, 'Penetration_txt']    <- 'Value should be numeric'
        ## *** Check for negative values
        c <- d$Penetration < 0
        d[c, 'Penetration_issues'] <- 1L
        d[c, 'Penetration_txt']    <- 'Value should be positive'
        ## *** Check for excessive values
        c <- grepl('^[0-9.]+$', d$Penetration) & d$Penetration > 2000
        d[c, 'Penetration_issues'] <- 1L
        d[c, 'Penetration_txt']    <- 'Value too high'

        ## ** SpeciesCode
        ## *** Check for existence
        c <- !(tolower(d$SpeciesCode) %in% fishr::fish_names[['NZFFD code']])
        d[c, 'SpeciesCode_issues'] <- 1L
        d[c, 'SpeciesCode_txt']    <- 'Species code not recognised'

        ## ** Altitude
        ## *** Check for non-numeric values
        c <- is.na(as.numeric(d$Altitude))
        d[c, 'Altitude_issues'] <- 1L
        d[c, 'Altitude_txt']    <- 'Value should be numeric'
        ## *** Check for negative values
        c <- d$Altitude < 0
        d[c, 'Altitude_issues'] <- 1L
        d[c, 'Altitude_txt']    <- 'Value should be positive'
        ## *** Check for excessive values
        c <- d$Altitude > 3600
        d[c, 'Altitude_issues'] <- 1L
        d[c, 'Altitude_txt']    <- 'Value too high'
        
        rv$table_js <- paste(c("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                               as.vector(unlist(sapply(setdiff(cols_needed[cols_needed %in% names(d)], 'Stratum'),
                                                function(v) {
                                   if (any(d[[paste0(v, '_issues')]] %in% 1L)) {
                                       c(sprintf("$('td:eq(%i)', nRow).attr('title', aData[%i]);",
                                                 which(names(d) == v)-1L,
                                                 which(names(d) == paste0(v, '_txt'))-1L),
                                         sprintf("if (aData[%i] == 1) $('td:eq(%i)', nRow).css(\"background-color\", \"#f00\").css(\"color\", \"#fff\").css(\"font-weight\", \"bold\")",
                                                 which(names(d) == paste0(v, '_issues'))-1L,
                                                 which(names(d) == v)-1L))
                                       }
          }))),
          "}"), collapse='\n')
        
        rv$cleanTable <- d
    })

    # data table on page 3
    output$newTable <- renderDT({
        req(rv$cleanTable)
        print(rv$cleanTable)
        d <- rv[['cleanTable']]
        tabjs <- rv[['table_js']]
        print(tabjs)
        dt <- DT::datatable(d, rownames = F, selection = 'none',
                            options = list(
                                paging = FALSE, searching = FALSE, ordering = FALSE
                              , rowCallback = JS(tabjs)
                              , columnDefs = list(list(visible=FALSE,
                                                       targets=grep('_issues$|_txt$', names(d))-1L))
            )
            )
        dt
    }, server = FALSE)
    
})
