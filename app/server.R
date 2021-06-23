library(shiny)
library(shinyjs)
library(data.table)
library(DT)
library(tidyverse)
library(quantreg)
library(ggplot2)
library(shinyWidgets)
library(leaflet)
library(sf)
## library(tmap)
library(leaflet.esri)
library(kableExtra)
library(mapview)

load('data/species_ibi_metrics.rda', v=T)
load('data/fish_names.rda', v=T)
source('fishr-functions.R')

cols_needed <- c('Date', 'SiteID', 'Penetration', 
                 'Altitude', 'SpeciesCode')

req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed

cols_opt <- c('Easting', 'Northing', 'Location', 'NZreach')

icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')



rv <- NULL
## reactlog::reactlog_enable()


## df <- read.csv('data/trial.csv', stringsAsFactors=F)
## df <- read.csv('data/demo-table.csv', stringsAsFactors=F)

callback <- "$(document).contextMenu({
    selector: '#dtable th',
    trigger: 'right',
    items: {
        // <input type=\"radio\">
        Date: {
            name: \"Date\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'Date',
        },
        SiteID: {
            name: \"SiteID\", 
            type: 'radio', 
            radio: 'radio', 
            value: 'SiteID',
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

    shinyjs::disable(selector = '#myFirst li a[data-value=">"]')
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.frame(req = names(req_fields), good = 0L),
        intable = NULL, tablefields = NULL, tablefields_ori = NULL,
        finalTable = NULL
    )

    observe({
        if (is.null(rv$intable)) {
            shinyjs::disable(selector = '#myFirst li a[data-value="2. Match headers"]')
            shinyjs::disable(selector = '#myFirst li a[data-value="3. Check input data"]')
            shinyjs::disable(selector = '#myFirst li a[data-value="4. Calculate IBI score"]')
        } else {
            shinyjs::enable(selector = '#myFirst li a[data-value="2. Match headers"]')
        }
        if (!is.null(rv$selfields) && all(rv$selfields$good == 1)) {
            shinyjs::enable(selector = '#myFirst li a[data-value="3. Check input data"]')
        } else {
            shinyjs::disable(selector = '#myFirst li a[data-value="3. Check input data"]')
        }
        if (!is.null(dataissues()) && !dataissues()) {
            shinyjs::enable(selector = '#myFirst li a[data-value="4. Calculate IBI score"]')
        } else {
            shinyjs::disable(selector = '#myFirst li a[data-value="4. Calculate IBI score"]')
        }
        
    })
    
    ## * Data loading
    
    observe({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        rv[['intable']] <- NULL
        rv[['ignoredrows']] <- NULL
        rv[['finalTable']] <- NULL
        df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
        rv[['selfields']] <- fields
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
    }, label='File upload')
    
    observeEvent(input$exbtn, {
        df <- NULL
        rv[['intable']] <- NULL
        rv[['ignoredrows']] <- NULL
        rv[['finalTable']] <- NULL
        df <- read.csv('data/trial.csv', header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
        rv[['selfields']] <- fields
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
    }, label = 'Demo loading')

    ## * List of required fields
    output$mandfields <- renderUI({
        ## checkboxGroupInput('mandfields',  label = 'Mandatory fields:', choices = cols_needed,
        ##                    selected = intersect(cols_needed, names(rv$intable)))
        ## awesomeCheckboxGroup('mandfields',  label = 'Mandatory fields:', choices = cols_needed,
        ##                      selected = intersect(cols_needed, names(rv$intable)))
        prettyCheckboxGroup(inputId='mandfields',  label = 'Mandatory fields:', choices = cols_needed,
                            shape = 'round', icon = icon('check'), animation = 'pulse',
                            selected = intersect(cols_needed, names(rv$intable)))
    })
    observe({
        ## updateCheckboxGroupInput(session, 'mandfields',
        ##                          selected = intersect(cols_needed, names(rv$intable)))
        ## updateAwesomeCheckboxGroup(session, 'mandfields',
        ##                            selected = intersect(cols_needed, names(rv$intable)))
        updatePrettyCheckboxGroup(session, inputId='mandfields', label = 'Mandatory fields:',
                                  choices = cols_needed,
                                  selected = intersect(cols_needed, names(rv$intable)),
                                  prettyOptions = list(shape = 'round', icon = icon('check'),
                                                       animation = 'pulse'))
        })

    ## * Renaming fields from context menu
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
    }, label = 'Fields renaming')

    ## * Raw input table
    # data table on page 2
    output$dtable <- renderDT({
        d <- rv[['intable']]
        nms <- names(d)
        dt <- DT::datatable(d
                          , callback = JS(callback)
                          , colnames = rv[['tablefields']], rownames = F,
                            selection = 'none', width = 600,
                            class = 'nowrap hover compact nostripe',
                            options = list(autoWidth = FALSE, scrollCollapse=TRUE, lengthChange = F
                                         , paging = nrow(d)>15, pageLength = 15
                                         , searching = FALSE, ordering = FALSE
                                         , columnDefs = list(list(className = 'dt-left', targets = '_all'))
                                           )
                            )
        sf <- rv[['selfields']]
        goodfields <- sf[sf$good == 1, 'req']
        dt <- dt %>% formatStyle(columns = goodfields, backgroundColor = "#00C7A811")
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
    }, label = 'Enable/disable button to next on page 2')
    
    # check data button
    observeEvent(input$checkData, {
      updateTabsetPanel(session, "myFirst",
                        selected = "3. Check input data")
    }, label = 'Go to page 3')

    # button to go from tab 2 to 3
    observe({
      if(dataissues()) {
        disable('to4btn')
        enable('remIssuesBtn')
      } else {
        enable('to4btn')
        disable('remIssuesBtn')
      }
    }, label = 'Enable/disable button to next on page 3')

    
    observeEvent(input$to4btn, {
      updateTabsetPanel(session, "myFirst",
                        selected = "4. Calculate IBI score")
    }, label = 'Go to page 4')
    
    
    ## * Table cleaned and with identified issues
    
    cleanTable <- reactive({
        req(rv$intable)
        d <- rv$intable[, c(cols_needed[cols_needed %in% names(rv$intable)],
                            cols_opt[cols_opt %in% names(rv$intable)])]

        ## ** Penetration
        if ('Penetration' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$Penetration)
            if (any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value is missing'
            }
            ## *** Check for non-numeric values
            c <- !grepl('^[0-9.]+$', d$Penetration)
            if (any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value should be numeric'
            }
            ## *** Check for negative values
            c <- !is.na(d$Penetration) & d$Penetration < 0
            if (any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value should be positive'
            }
            ## *** Check for excessive values
            c <- grepl('^[0-9.]+$', d$Penetration) & d$Penetration > 2000
            if (any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value too high'
            }
        }
        ## ** SpeciesCode
        if ('SpeciesCode' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$SpeciesCode)
            if (any(c)) {
                d[c, 'SpeciesCode_issues'] <- 1L
                d[c, 'SpeciesCode_txt']    <- 'Value is missing'
            }
            ## *** Check for existence
            c <- !(tolower(d$SpeciesCode) %in% fish_names[['NZFFD code']])
            if (any(c)) {
                d[c, 'SpeciesCode_issues'] <- 1L
                d[c, 'SpeciesCode_txt']    <- 'Species code not recognised'
            }
        }
        ## ** Altitude
        if ('Altitude' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$Altitude)
            if (any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value is missing'
            }
            ## *** Check for non-numeric values
            c <- is.na(as.numeric(d$Altitude))
            if (any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value should be numeric'
            }
            ## *** Check for negative values
            c <- !is.na(d$Altitude) & d$Altitude < 0
            if (any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value should be positive'
            }
            ## *** Check for excessive values
            c <- !is.na(d$Altitude) & d$Altitude > 3600
            if (any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value too high'
            }
        }

        d
        
    })


    table_js <- reactive({
        d <- cleanTable()
        req(d)
        
        ## ** Javascript bit for cell colour and tooltip
        if (any(c(cols_needed, cols_opt) %in% names(d))) {
                paste(c("function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                        as.vector(unlist(sapply(
                            setdiff(cols_needed[cols_needed %in% names(d)], 'Date'),
                            function(v) {
                                if (any(d[[paste0(v, '_issues')]] %in% 1L)) {
                                    c(sprintf("$('td:eq(%i)', nRow).attr('title', aData[%i]);",
                                              which(names(d) == v)-1L,
                                              which(names(d) == paste0(v, '_txt'))-1L),
                                      sprintf("if (aData[%i] == 1) $('td:eq(%i)', nRow).css(\"background-color\", \"#ff000044\").css(\"color\", \"#003547\").css(\"font-weight\", \"normal\")",
                                              which(names(d) == paste0(v, '_issues'))-1L,
                                              which(names(d) == v)-1L))
                                }
                            }))),
                        "}"), collapse='\n')
        }
        
    })

    
    dataissues <- reactive({
        d <- rv$finalTable
        req(d)
        if (any(grepl('_issues$', names(d))) &&
            sum(sapply(d[, grep('_issues$', names(d), val=T)], function(x) any(x %in% 1))) > 0) {
            return(1)
        } else {
            return(0)
        }
    }, label = 'Data with issues?')

    observeEvent(input$remIssuesBtn, {
        d <- cleanTable()
        req(d)
        if (any(grepl('_issues$', names(d)))) {
            rv$ignoredrows <- sum(rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T) > 0)
            d <- d[rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T) == 0,]
        }
        rv$finalTable <- d
    }, label = 'Ignore rows with issues')
    
    observe({
        d <- cleanTable()
        req(d)
        rv$finalTable <- d
    })
    
    ## * Cleaned table on page 3
    
    output$newTable <- renderDT({
        d <- rv$finalTable
        req(d)
        print(head(d))
        if (any(grepl('_issues$', names(d))) &&
            sum(sapply(d[, grep('_issues$', names(d), val=T)], function(x) any(x %in% 1))) > 0) {
            d <- d[rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T) > 0,]
            tabjs <- table_js()
        } else {
            tabjs <- NULL
        }
        dt <- DT::datatable(
            d, rownames = F, selection = 'none', width = 600,
            class = 'nowrap hover compact nostripe',
            options = list(autoWidth = FALSE, scrollCollapse=TRUE
                         , paging = nrow(d)>15, pageLength = 15, lengthChange = F
                         , searching = FALSE, ordering = FALSE
                         , rowCallback = JS(tabjs)
                         , columnDefs = list(list(className = 'dt-left', targets = '_all'),
                                             list(visible=FALSE,
                                                  targets=grep('_issues$|_txt$', names(d))-1L))
                           )
        )
        dt
    }, server = FALSE)



    ## * Feedback on issues
    
    output$issuesTxt <- renderText({
        d <- rv$finalTable
        req(d)
        n.rows.noissues <- sum(rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T) == 0)
        n.issues <- sum(rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T))
        n.ignoredrows <- ifelse(is.null(rv$ignoredrows), 0, rv$ignoredrows)

        sprintf('%s<br>%i valid rows%s',
                ifelse(n.issues == 0, 'No issues found',
                       sprintf('%i data issues were found', n.issues)),
                n.rows.noissues,
                ifelse(n.ignoredrows == 0, '',
                       sprintf('<br>(%s rows with issues were excluded)', n.ignoredrows)))
    })
    output$issuesIcon <- renderText({
        if (dataissues()) {
            as.character(icon('exclamation-triangle', class='dangerico'))
        } else {
            as.character(icon('check-square', class='nodangerico'))
        }
    })
    
    output$issueImg <- renderImage({
        d <- rv$finalTable
        req(d)
        n.issues <- sum(rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T))
        if (n.issues > 0) {
            list(src = 'data/warning.png',
                 width = '100px', height = '100px',
                 alt = 'Warning')
        } else {
            list(src = 'data/check.png',
                 width = '100px', height = '100px',
                 alt = 'All good')
        }
    }, deleteFile=F)
    
    output$issuesSubTxt <- renderText({
        d <- rv$finalTable
        req(d)
        n.issues <- sum(rowSums(d[, grep('_issues$', names(d), val=T)], na.rm=T))
        if (n.issues > 0) {
            return('Please correct the following issues and re-upload the file')
        } else {
            return('The table below will be used as it is to calculate the IBI scores')
        }
    })

    observeEvent(input$testbtn, {
        print(cleanTable())
        print(rv$finalTable)
        
    })


    ## * IBI scores
    
    ibiData <- reactive({
        d <- rv$finalTable
        req(d)

        d$Altitude <- as.numeric(d$Altitude)
        d$Penetration <- as.numeric(d$Penetration)
        
        site_metrics_all <- d %>%
            prep.site.metrics(species.ibi.metrics = species_ibi_metrics)
        
        qr.1.elev <- qr.construct("metric1", "altitude", data = site_metrics_all)
        qr.2.elev <- qr.construct("metric2", "altitude", data = site_metrics_all)
        qr.3.elev <- qr.construct("metric3", "altitude", data = site_metrics_all)
        qr.4.elev <- qr.construct("metric4", "altitude", data = site_metrics_all)
        qr.5.elev <- qr.construct("metric5", "altitude", data = site_metrics_all)
        
        qr.1.penet <- qr.construct("metric1", "penet", data = site_metrics_all)
        qr.2.penet <- qr.construct("metric2", "penet", data = site_metrics_all)
        qr.3.penet <- qr.construct("metric3", "penet", data = site_metrics_all)
        qr.4.penet <- qr.construct("metric4", "penet", data = site_metrics_all)
        qr.5.penet <- qr.construct("metric5", "penet", data = site_metrics_all)
        
        ibi_scores <- site_metrics_all %>% 
            add.fish.metrics(q1e=qr.1.elev, q2e=qr.2.elev, q3e=qr.3.elev,
                             q4e=qr.4.elev, q5e=qr.5.elev,
                             q1p=qr.1.penet, q2p=qr.2.penet, q3p=qr.3.penet,
                             q4p=qr.4.penet, q5p=qr.5.penet
                             ) %>% 
            add.fish.metric6() %>% 
            add.fish.ibi() %>% 
            cut.fish.ibi() %>% 
            nps()

        ibi_scores
    })

    
    ## * Table of IBI scores
    output$ibiTable <- renderDT({
        ibi_scores <- ibiData()
        req(ibi_scores)
        ibi_scores <- ibi_scores %>% select("date", 'siteID', "ibi_score",
                                            "ibi_score_cut", "nps_score", "Stratum")
        
        dt <- DT::datatable(
            ibi_scores, rownames = F, selection = 'none', width = 600,
            class = 'nowrap hover compact stripe',
            options = list(autoWidth = TRUE, scrollCollapse=TRUE, scrollX = ncol(ibi_scores)>15,
                         paging = nrow(ibi_scores)>15, pageLength = 15,
                         searching = FALSE, ordering = FALSE,
                         columnDefs = list(list(
                           className = 'dt-left', targets = 1:5
                         ))
                           )
        )        
        dt
    }, server = F)

    
    ## * MAP
    output$map <- renderLeaflet({

        ibi_scores <- copy(ibiData())
        ibi <- as.data.table(ibi_scores)
        req(ibi)
        dt <- as.data.table(rv$finalTable)

        print(ibi[1])
        print(dt[1])
        if (all(c('Easting', 'Northing') %in% names(dt))) {
            
            coords <- dt[, .(x = mean(Easting), y = mean(Northing)), .(Date, SiteID)]
            ibi <- merge(ibi, coords, by.x = c('date', 'siteID'), by.y = c('Date', 'SiteID'), all = T)
            ibi <- st_as_sf(ibi, coords = c('x', 'y'), crs = 27200)
            ibi <- st_transform(ibi, crs = 4326)
            ibi <- cbind(ibi, st_coordinates(ibi))
            ibi <- as.data.table(ibi)
            ibi <- ibi[, .(date, siteID, ibi_score, ibi_score_cut, nps_score,
                           total_sp_richness, number_non_native, X, Y)]

            numcols <- colorNumeric(c('#BF2F37', '#004A6D', '#2C9986'), domain = NULL)
            factcols <- colorFactor(rev(c('#BF2F37', '#004A6D', '#2C9986', '#00C7A8')), domain = NULL)
            ibi[, nps_score := factor(as.character(nps_score), levels = c('A', 'B', 'C', 'D'))]
            
            ibi[, labels := paste0(
                      sprintf("<strong> Site ID: %s - Date: %s: </strong><br/> ", siteID, date),
                      kable_styling(knitr::kable(data.table(`IBI score`         = ibi_score,
                                                            `IBI category`      = ibi_score_cut,
                                                            `NPS category`      = nps_score,
                                                            `Total sp richness` = total_sp_richness,
                                                            `Non-native spp`   = number_non_native),
                                                 format='html', escape = F)))
              , by = 1:nrow(ibi)]

            ## Create map
            rv$map <- leaflet() %>%
                addTiles() %>%
                setView(173.6, -41, zoom = 5) %>% 
                addEsriBasemapLayer(esriBasemapLayers$Topographic, autoLabels = TRUE
                                    ## , options = providerTileOptions(minZoom = 5, maxZoom = 12)
                                    ) %>%
                addCircleMarkers(data = ibi, lng = ~X, lat = ~Y,
                                 ## fillColor = ~numcols(ibi_score), color = ~numcols(ibi_score),
                                 fillColor = ~factcols(nps_score), color = ~factcols(nps_score),
                                 popup = ~labels %>% lapply(htmltools::HTML),
                                 popupOptions = labelOptions(
                                     style = list("font-weight" = "normal",
                                                  padding = "3px 8px", "color" = 'grey80'),
                                     textsize = "17px", direction = "auto", sticky = F,
                                     maxWidth = 700, closeOnClick = T),
                                 ## radius = ~radius,
                                 fillOpacity = 0.6,
                                 radius = 4,
                                 opacity = 0.8,
                                 weight = 1
                                 ) %>%
                addLegend(data = ibi, "bottomright", pal = factcols, values = ~nps_score,
                          title = 'NPS category')
                ## addLegend(data = ibi, "bottomright", pal = numcols, values = ~ibi_score,
                ##           title = 'IBI score')

            rv$map
        }

    })


    output$mapdl <- downloadHandler(
        filename = 'IBI_map.png',
        ## filename = 'IBI_map.html',
        content = function(file) {
            mapshot(rv$map, file = file)
            ## saveWidget(rv$map, 'map.html', selfcontained=F)
            ## webshot::webshot('map.html', file = file)
        }
    )
    
    
    output$npsGraph <- renderPlot({
      ibi_scores <- ibiData()
      req(ibi_scores)
      
      group.colors <- c('A'  = "#00C7A8",
                        'B'  = "#2C9986", 
                        'C'  = "#004A6D", 
                        'D'  = "#BF2F37",
                        'NA' = '#d4dde1')
      
      g <- ggplot(ibi_scores, aes(x = nps_score)) + 
        geom_histogram(stat = "count", fill = group.colors, alpha = 0.9) + 
        xlab("NPS-FM category") + 
        ylab("Number of sites") + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_line(size=.1, color="black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(),
              axis.line.y = element_blank(),
              axis.text.x=element_text(size = 12, face = 'bold', margin = margin(t = 15)),
              axis.ticks = element_blank()) +
        theme(plot.margin=unit(c(1, 0.5, 0.5, 0.5),"cm"))+
        theme(axis.title.x = element_text(size = 12, margin = margin(t = 20)))+
        theme(axis.title.y = element_text(size = 12, margin = margin(r = 14)))
      
      g
    })

    output$download <- downloadHandler(
      filename = function(){"ibiScore.csv"},
      content = function(fname){
        write.csv(ibiData(), fname)
      })
    
})

