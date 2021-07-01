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
library(leaflet.extras)
library(kableExtra)
library(mapview)
library(reactable)
library(tippy)

load('data/species_ibi_metrics.rda', v=T)
load('data/fish_names.rda', v=T)
source('fishr-functions.R')

cols_needed <- c('SiteID', 'Date', 'Penetration', 
                 'Altitude', 'SpeciesCode')

req_fields <- rep(0, length(cols_needed))
names(req_fields) <- cols_needed

cols_opt <- c('Easting', 'Northing', 'Location', 'NZreach')

icons <- c("exclamation-triangle", "check-circle")
cols <- c('red', 'green')

ph <- webshot:::find_phantom()
if (is.null(ph) || ph == '') {
    webshot::install_phantomjs()
}

rv <- NULL
## reactlog::reactlog_enable()


## rv <- list(
##     reqfields = req_fields,
##     selfields = data.frame(req = names(req_fields), good = 0L),
##     intable = NULL, tablefields = NULL, tablefields_ori = NULL,
##     finalTable = NULL
## )

shinyServer(function(input, output, session) {

    shinyjs::disable(selector = '#myFirst li a[data-value=">"]')
    
    rv <- reactiveValues(
        reqfields = req_fields,
        selfields = data.frame(req = names(req_fields), good = 0L),
        intable = NULL, tablefields = NULL, tablefields_ori = NULL,
        finalTable = NULL, test = NULL
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
        rv[['ignoredrows']] <- 0L
        rv[['finalTable']] <- NULL
        df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
        rv[['selfields']] <- fields
        fields <- data.table(ori = names(df), good = names(df) %in% c(cols_needed, cols_opt))
        fields[, match := fifelse(ori %in% c(cols_needed, cols_opt), ori, '')]
        rv[['fieldsmatching']] <- as.data.frame(fields)
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
    }, label='File upload')
    
    observeEvent(input$exbtn, {
        df <- NULL
        rv[['intable']] <- NULL
        rv[['ignoredrows']] <- 0L
        rv[['finalTable']] <- NULL
        df <- read.csv('data/example-data.csv', header = TRUE, stringsAsFactors = F)
        fields <- isolate(rv[['selfields']])
        fields$good <- ifelse(fields$req %in% names(df), 1, 0)
        rv[['selfields']] <- fields
        fields <- data.table(ori = names(df), good = names(df) %in% c(cols_needed, cols_opt))
        fields[, match := fifelse(ori %in% c(cols_needed, cols_opt), ori, '')]
        rv[['fieldsmatching']] <- as.data.frame(fields)
        rv[['intable']] <- df
        rv[['tablefields']] <- names(df)
        rv[['tablefields_ori']] <- names(df)
        updateTabsetPanel(session, "myFirst",
                          selected = "2. Match headers")
    }, label = 'Demo loading')

    ## * List of required fields
    output$mandfields <- renderUI({
        fm <- rv$fieldsmatching
        prettyCheckboxGroup(inputId='mandfields',  label = 'Mandatory fields:', choices = cols_needed,
                            shape = 'round', icon = icon('check'), animation = 'pulse',
                            selected = intersect(cols_needed, fm$match[fm$good==T]))
    })
    observe({
        fm <- rv$fieldsmatching
        updatePrettyCheckboxGroup(session, inputId='mandfields', label = 'Mandatory fields:',
                                  choices = cols_needed,
                                  selected = intersect(cols_needed, fm$match[fm$good==T]),
                                  prettyOptions = list(shape = 'round', icon = icon('check'),
                                                       animation = 'pulse'))
        })

    ## * Assigment of field roles from context menu
    
    observeEvent(input$selfield, {
        if (!is.null(input$selfield)) {
            pair <- strsplit(input$selfield, '=')[[1]]
            if (pair[1] == 'None') {
                pair[1] <- ''
            }
            ## pair <- c('SiteID', 'card')
            ## pair <- c('Northing', 'Easting')
            sf <- isolate(rv[['selfields']])
            fm <- as.data.table(isolate(rv[['fieldsmatching']]))
            if (pair[1] %in% fm$match) {
                fm[match == pair[1], `:=`(good = F, match = '')]
                sf[sf$req == pair[1], 'good'] <- 0L
            }
            fm[ori == pair[2], `:=`(good = T, match = pair[1])]
            rv[['fieldsmatching']] <- as.data.frame(fm)
            
            sf[sf$req == pair[1], 'good'] <- 1L
            rv[['selfields']] <- sf
            ## d <- isolate(rv[['intable']])
            ## if (pair[1] != pair[2] & pair[1] %in% names(d)) {
            ##     names(d)[names(d) %in% pair[1]] <- paste0(names(d)[names(d) %in% pair[1]], '_0')
            ## }
            ## if (pair[1] != pair[2] & pair[2] %in% names(d)) {
            ##     names(d)[names(d) == pair[2]] <- pair[1]
            ##     rv[['tablefields']] <- names(d)
            ## }
            ## sf$good <- ifelse(sf$req %in% names(d), 1, 0)
            ## rv[['selfields']] <- sf
            ## rv[['intable']] <- d
        }
    }, label = 'Fields renaming')


    ## * Raw input table

    output$dtable <- renderReactable({
        d <- rv[['intable']]
        fm <- rv[['fieldsmatching']]
        cols <- sapply(fm$ori, function(m) {
            colDef(header = function(value) {
                sub <- div(style = "color: #2C9986; font-size: 1em", fm$match[fm$ori == m])
                div(div(class='field', value), sub)
            })
        }, simplify = F)
        reactable(d, highlight=T, compact=T, wrap=F,
                  columns = cols,
                  defaultColDef = colDef(align = 'left'))
    })

    output$logtxt <- renderPrint({
        cat('input$selfield=\n')
        print(input$selfield)
        cat('\n\nrv$selfields=\n')
        print(rv$selfields)
        cat('\n\nnames(rv$intable)=\n')
        print(names(rv[['intable']]))
        print(rv$fieldsmatching)
        print(head(rv$finalTable))
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

        fm <- rv$fieldsmatching
        d <- rv$intable
        cols <- fm$ori[fm$good == T]
        goodcols <- intersect(c(cols_needed, cols_opt), fm$match[fm$good==T])
        d <- rv$intable[, fm$ori[match(goodcols, fm$match)]]
        names(d) <- goodcols
        
        ## ** Penetration
        if ('Penetration' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$Penetration)
            if (any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value is missing'
            }
            ## *** Check for non-numeric values
            suppressWarnings({c <- !is.na(d$Penetration) & is.na(as.numeric(d$Penetration))})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value should be numeric'
            }
            ## *** Check for negative values
            suppressWarnings({c <- !is.na(d$Penetration) & as.numeric(d$Penetration) < 0})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value should be positive'
            }
            ## *** Check for excessive values
            suppressWarnings({c <- grepl('^[0-9.]+$', d$Penetration) & as.numeric(d$Penetration) > 2000})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Penetration_issues'] <- 1L
                d[c, 'Penetration_txt']    <- 'Value too high'
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
            suppressWarnings({c <- !is.na(d$Altitude) & is.na(as.numeric(d$Altitude))})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value should be numeric'
            }
            ## *** Check for negative values
            suppressWarnings({c <- !is.na(d$Altitude) & as.numeric(d$Altitude) < 0})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value should be positive'
            }
            ## *** Check for excessive values
            suppressWarnings({c <- !is.na(d$Altitude) & as.numeric(d$Altitude) > 3600})
            if (!all(is.na(c)) && any(c)) {
                d[c, 'Altitude_issues'] <- 1L
                d[c, 'Altitude_txt']    <- 'Value too high'
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
            ## *** Check existence in species_ibi_metrics
            c <- !(tolower(d$SpeciesCode) %in% species_ibi_metrics$spcode)
            if (any(c)) {
                d[c, 'SpeciesCode_warnings'] <- 1L
                d[c, 'SpeciesCode_wtxt']    <- 'Non-fish code or species without IBI metrics'
            }
        }        
        ## ** SiteID
        if ('SiteID' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$SiteID)
            if (any(c)) {
                d[c, 'SiteID_issues'] <- 1L
                d[c, 'SiteID_txt']    <- 'Value is missing'
            }
        }
        ## ** Date
        if ('Date' %in% names(d)) {
            ## *** Check for missing values
            c <- is.na(d$Date)
            if (any(c)) {
                d[c, 'Date_issues'] <- 1L
                d[c, 'Date_txt']    <- 'Value is missing'
            }
        }

        d
        
    })


    dataissues <- reactive({
        d <- rv$finalTable
        req(d)
        if ((any(grepl('_issues$', names(d))) &&
            sum(sapply(d[, grep('_issues$', names(d), val=T)], function(x) any(x %in% 1)))) | 
            (any(grepl('_warnings$', names(d))) &&
             sum(sapply(d[, grep('_warnings$', names(d), val=T)], function(x) any(x %in% 1)))) > 0) {
            return(1)
        } else {
            return(0)
        }
    }, label = 'Data with issues?')

    observeEvent(input$remIssuesBtn, {
        d <- as.data.table(cleanTable())
        req(d)
        if (any(grepl('_issues$', names(d)))) {
            d[, with_issue := rowSums(.SD, na.rm=T) > 0,
              .SDcols = patterns('_issues$')]
            d[, site_issue := any(with_issue %in% T), .(SiteID, Date)]
            rv$ignoredrows <- sum(d$site_issue == T)
            d <- d[site_issue == F, -c('with_issue', 'site_issue'), with = F]
        }
        if (any(grepl('_warnings$', names(d)))) {
            d[, with_warnings := rowSums(.SD, na.rm=T) > 0,
              .SDcols = patterns('_warnings$')]
            rv$ignoredrows <- rv$ignoredrows + sum(d$with_warnings == T)
            d <- d[with_warnings == F, -c('with_warnings'), with=F]
        }
        rv$finalTable <- as.data.frame(d)
    }, label = 'Ignore visits with issues')
    
    observe({
        d <- cleanTable()
        req(d)
        rv$finalTable <- d
    })
    
    ## * Cleaned table on page 3
    
    output$newTable <- renderReactable({
        d <- rv$finalTable
        req(d)
        if (any(grepl('_issues$', names(d))) &&
            sum(sapply(d[, grep('_issues$|_warnings$', names(d), val=T)], function(x) any(x %in% 1))) > 0) {
            d <- d[rowSums(d[, grep('_issues$|_warnings$', names(d), val=T)], na.rm=T) > 0,]
        }
        print(d[1,])

        cols2hide <- sapply(grep('_issues$|_warnings$|_txt$|_wtxt$', names(d), val=T), function(x) {
            colDef(show = FALSE)
        }, simplify = F)
        
        dt <- reactable(
            d, highlight=T, compact=T, wrap=F,
            defaultColDef = colDef(
                align = 'left', html = T,
                class = function(value, index, name) {
                    if ((paste0(name,'_warnings') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_warnings')]) &&
                        d[index, paste0(name,'_warnings')] > 0) |
                        (paste0(name,'_issues') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_issues')]) &&
                        d[index, paste0(name,'_issues')] > 0)) 
                        return('CellWithComment')
                },
                style = function(value, index, name) {
                    if (paste0(name,'_warnings') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_warnings')]) &&
                        d[index, paste0(name,'_warnings')] > 0) {
                        return(list(background='#1F3B7133', color='#003547', fontWeight='normal'))
                    }
                    if (paste0(name,'_issues') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_issues')]) &&
                        d[index, paste0(name,'_issues')] > 0) {
                        return(list(background='#BF2F3733', color='#003547', fontWeight='normal'))
                    }
                },
                cell = function(value, index, name) {
                    if (is.na(value))
                        value <- ''
                    ##     v <- '<tr><td>&nbsp;</td></tr>'
                    ## } else v <- value
                    v <- value
                    if (paste0(name,'_warnings') %in% names(d) &&
                        paste0(name,'_wtxt') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_warnings')]) &&
                        d[index, paste0(name,'_warnings')] > 0) {
                        v <- sprintf('%s <span class="CellComment">%s</span>',
                                     value, d[index, paste0(name,'_wtxt')])
                    }
                    if (paste0(name,'_issues') %in% names(d) &&
                        paste0(name,'_txt') %in% names(d) &&
                        !is.na(d[index, paste0(name,'_issues')]) &&
                        d[index, paste0(name,'_issues')] > 0) {
                        v <- sprintf('%s <span class="CellComment">%s</span>',
                                     value, d[index, paste0(name,'_txt')])
                    }
                    v
                }),
            columns = cols2hide
        )
        dt
    })


    ## * Feedback on issues
    
    output$issuesTxt <- renderText({
        d <- rv$finalTable
        req(d)
        n.rows.noissues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T)], na.rm=T) == 0)
        n.issues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T)], na.rm=T))
        n.ignoredrows <- ifelse(is.null(rv$ignoredrows), 0, rv$ignoredrows)

        sprintf('%s<br>%i valid rows%s',
                ifelse(n.issues == 0, 'No issues found',
                       sprintf('%i data issues were found', n.issues)),
                n.rows.noissues,
                ifelse(n.ignoredrows == 0, '',
                       sprintf('<br>(%s rows were excluded)', n.ignoredrows)))
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
        n.issues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T)], na.rm=T))
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
        n.issues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T)], na.rm=T))
        if (n.issues > 0) {
            return('Please correct the following issues and re-upload the file, or exclude the issues, which would remove the individual records for issues of non-fish codes, or whole visits for other issues.')
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
        
        qr.1.elev <- qr.construct("metric1", "Altitude", data = site_metrics_all)
        qr.2.elev <- qr.construct("metric2", "Altitude", data = site_metrics_all)
        qr.3.elev <- qr.construct("metric3", "Altitude", data = site_metrics_all)
        qr.4.elev <- qr.construct("metric4", "Altitude", data = site_metrics_all)
        qr.5.elev <- qr.construct("metric5", "Altitude", data = site_metrics_all)
        
        qr.1.penet <- qr.construct("metric1", "Penet", data = site_metrics_all)
        qr.2.penet <- qr.construct("metric2", "Penet", data = site_metrics_all)
        qr.3.penet <- qr.construct("metric3", "Penet", data = site_metrics_all)
        qr.4.penet <- qr.construct("metric4", "Penet", data = site_metrics_all)
        qr.5.penet <- qr.construct("metric5", "Penet", data = site_metrics_all)
        
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

        ibi_scores <- as.data.table(ibi_scores)[unique(as.data.table(d)[, .(SiteID, Date)]), on = c('Date', 'SiteID')]
        
        return(as.data.frame(ibi_scores))
    })

    
    ## * Table of IBI scores
    output$ibiTable <- renderReactable({
        ibi_scores <- ibiData()
        req(ibi_scores)
        ibi_scores <- ibi_scores %>% select('SiteID', "Date", "IBIscore",
                                            "IBIscoreCut", "NPSscore")

        dt <- reactable(ibi_scores, highlight=T, compact=T, wrap=F, defaultColDef = colDef(align = 'left'))
        
        ## dt <- DT::datatable(
        ##     ibi_scores, rownames = F, selection = 'none', width = 600,
        ##     class = 'nowrap hover compact stripe',
        ##     options = list(autoWidth = TRUE, scrollCollapse=TRUE, scrollX = FALSE,
        ##                  paging = nrow(ibi_scores)>15, pageLength = 15, lengthChange = F,
        ##                  searching = FALSE, ordering = FALSE,
        ##                  columnDefs = list(list(
        ##                    className = 'dt-left', targets = 0:4
        ##                  ))
        ##                    )
        ## )        
        dt
    })



    ## * Scores plot
    
    output$scoresPlot <- renderPlot({
      ibi_scores <- ibiData()
      req(ibi_scores)

      ibi_scores$NPSscore[is.na(ibi_scores$NPSscore)] <- 'Unknown'
      ibi_scores$NPSscore <- factor(as.character(ibi_scores$NPSscore),
                                    levels = c('A', 'B', 'C', 'D', 'Unknown'))

      ibi_scores$IBIscoreCut <- as.character(ibi_scores$IBIscoreCut)
      ibi_scores$IBIscoreCut[is.na(ibi_scores$IBIscoreCut)] <- 'Unknown'
      ibi_scores$IBIscoreCut <- factor(ibi_scores$IBIscoreCut,
                                       levels = c('Low quality', 'Medium quality', 'High quality', 'Unknown'))
      
      if (input$sel_score == 'nps_score') {
          group.colors <- c('A'       = "#00C7A8",
                            'B'       = "#2C9986", 
                            'C'       = "#004A6D", 
                            'D'       = "#BF2F37",
                            'Unknown' = '#d4dde1')
          group.colors <- group.colors[names(group.colors) %in% ibi_scores$NPSscore]
          g <- ggplot(ibi_scores, aes(x = NPSscore, fill = NPSscore)) +
              xlab("NPS-FM category")
      } else if (input$sel_score == 'ibi_score') {
          group.colors <- c('Low quality'    = "#BF2F37",
                            'Medium quality' = "#004A6D", 
                            'High quality'   = "#00C7A8",
                            'Unknown'        = '#d4dde1')
          group.colors <- group.colors[names(group.colors) %in% ibi_scores$IBIscoreCut]
          g <- ggplot(ibi_scores, aes(x = IBIscoreCut, fill = IBIscoreCut)) + 
              xlab("IBI category")
      }
      g <- g + 
          geom_histogram(stat = "count", alpha = 0.9) + 
          ylab("Number of sites") + 
          scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
          scale_fill_manual(values = group.colors, na.value = '#d4dde1') +
          theme_bw() +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_line(size=.1, color="black"),
                panel.grid.major   = element_blank(),
                panel.grid.minor   = element_blank(),
                panel.border       = element_blank(),
                axis.line          = element_line(),
                axis.line.y        = element_blank(),
                axis.text.x        = element_text(size = 12, face = 'bold', margin = margin(t = 15)),
                axis.ticks         = element_blank(),
                legend.position    = 'none') +
          theme(plot.margin = unit(c(2, 0.5, 0.5, 0.5),"cm"))+
          theme(axis.title.x = element_text(size = 12, margin = margin(t = 20)))+
          theme(axis.title.y = element_text(size = 12, margin = margin(r = 14)))

      rv$scoredistplot <- g
      g
    })

    output$plotdl <- downloadHandler(
        filename = function() fifelse(input$sel_score == 'nps_score',
                                      'NPS_scores_distribution.png',
                                      'IBI_scores_distribution.png'),
        content = function(file) {
            req(rv$scoredistplot)
            ggsave(file, plot = rv$scoredistplot, device = "png", width = 7.5, height = 5)
        }
    )

    
    ## * MAP
    
    output$map <- renderLeaflet({

        ibi_scores <- copy(ibiData())
        ibi <- as.data.table(ibi_scores)
        req(ibi)
        dt <- as.data.table(rv$finalTable)

        ## print(ibi[1])
        ## print(dt[1])
        if (all(c('Easting', 'Northing') %in% names(dt))) {
            
            coords <- dt[, .(x = mean(Easting, na.rm=T), y = mean(Northing, na.rm=T)), .(Date, SiteID)]
            ibi <- merge(ibi, coords, by.x = c('Date', 'SiteID'), by.y = c('Date', 'SiteID'), all = T)
            ibi <- st_as_sf(ibi, coords = c('x', 'y'), crs = 27200)
            ibi <- st_transform(ibi, crs = 4326)
            ibi <- cbind(ibi, st_coordinates(ibi))
            ibi <- as.data.table(ibi)
            ibi <- ibi[, .(Date, SiteID, IBIscore, IBIscoreCut, NPSscore,
                           total_sp_richness, number_non_native, X, Y)]
            
            ibi[is.na(NPSscore), NPSscore := 'Unknown']
            ibi[, NPSscore := factor(as.character(NPSscore), levels = c('A', 'B', 'C', 'D', 'Unknown'))]
            ibi[, IBIscoreCut := as.character(IBIscoreCut)]
            ibi[is.na(IBIscoreCut), IBIscoreCut := 'Unknown']
            ibi[, IBIscoreCut := factor(IBIscoreCut, levels = c('Low quality',
                                                                 'Medium quality',
                                                                 'High quality',
                                                                 'Unknown'))]
            ibi[, labels := paste0(
              sprintf("<strong> Site ID: %s - Date: %s: </strong><br/> ", SiteID, Date),
              kable_styling(knitr::kable(data.table(`IBI score`         = IBIscore,
                                                    `IBI category`      = IBIscoreCut,
                                                    `NPS category`      = NPSscore,
                                                    `Total sp richness` = total_sp_richness,
                                                    `Non-native spp`   = number_non_native),
                                         format='html', escape = F)))
              , by = 1:nrow(ibi)]
            
            if (input$sel_score == 'nps_score') {
                
                factcols <- colorFactor(c('#00C7A8', '#2C9986', '#004A6D', '#BF2F37', '#808080'), domain = NULL)
                fc = ~factcols(NPSscore) 
                c = ~factcols(NPSscore)

                npss <- data.table(label = c('A', 'B', 'C', 'D', 'Unknown'),
                                   color = c('#00C7A8', '#2C9986', '#004A6D', '#BF2F37', '#808080'))
                npss <- npss[as.character(label) %in% ibi$NPSscore]
                
                rv$map <- leaflet() %>%
                    setView(173.6, -41, zoom = 5) %>%
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    addCircleMarkers(data = ibi, lng = ~X, lat = ~Y,
                                     fillColor = fc, color = c,
                                     popup = ~labels %>% lapply(htmltools::HTML),
                                     popupOptions = labelOptions(
                                         style = list("font-weight" = "normal",
                                                      padding = "3px 8px", "color" = 'grey80'),
                                         textsize = "17px", direction = "auto", sticky = F,
                                         maxWidth = 700, closeOnClick = T),
                                     ## radius = ~radius,
                                     fillOpacity = 0.7,
                                     radius = 4,
                                     opacity = 1,
                                     weight = 1
                                     ) %>%
                    addLegend(data = ibi, "bottomright",
                              colors = paste0(npss$color, "; opacity: 0.5; width: 10px; height: 10px; border-radius: 50%"),
                              labels = paste0("<div style='display: inline-block; height: 10px; margin-top: 4px; line-height: 10px;'>", npss$label, "</div>"),
                              title = 'NPS category', opacity = 1) %>%
                    addFullscreenControl()
                
                rv$map
                
            } else if (input$sel_score == 'ibi_score') {

                factcols <- colorFactor(c('#BF2F37', '#004A6D', '#2C9986', '#808080'), domain = NULL)
                fc <- ~factcols(IBIscoreCut) 
                c <- ~factcols(IBIscoreCut)
                ibis <- data.table(label = c(levels(ibi_scores$IBIscoreCut), 'Unknown'),
                                   color = c('#BF2F37', '#004A6D', '#2C9986', '#808080'))
                ibis <- ibis[as.character(label) %in% ibi$IBIscoreCut]
                
                rv$map <- leaflet() %>%
                                        # addTiles() %>%
                    setView(173.6, -41, zoom = 5) %>% 
                    addProviderTiles(providers$CartoDB.Positron) %>%
                    addCircleMarkers(data = ibi, lng = ~X, lat = ~Y,
                                     fillColor = fc, color = c,
                                     popup = ~labels %>% lapply(htmltools::HTML),
                                     popupOptions = labelOptions(
                                         style = list("font-weight" = "normal",
                                                      padding = "3px 8px", "color" = 'grey80'),
                                         textsize = "17px", direction = "auto", sticky = F,
                                         maxWidth = 700, closeOnClick = T),
                                     ## radius = ~radius,
                                     fillOpacity = 1,
                                     radius = 4,
                                     opacity = 1,
                                     weight = 1
                                     ) %>%
                    addLegend(data = ibi, "bottomright",
                              colors = paste0(ibis$color,
                                              "; width: 10px; height: 10px; border-radius: 50%"),
                              labels = paste0("<div style='display: inline-block; height: 10px; margin-top: 4px; line-height: 10px;'>", ibis$label, "</div>"),
                              title = 'IBI category', opacity = 1) %>%
                    addFullscreenControl()
                
                rv$map
            }
   
        }

    })

    mapdown <- reactive({
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        m <- rv$map %>%
            setView(lng = (lngRng[1]+lngRng[2])/2, lat = (latRng[1]+latRng[2])/2, zoom = input$map_zoom)
        m$x$options$fullscreenControl <- NULL # remove fullscreen control (others are removed automatically)
        m
    })

    output$mapdl <- downloadHandler(
        filename = function() fifelse(input$sel_score == 'nps_score',
                                      'NPS_scores_map.png',
                                      'IBI_scores_map.png'),
        content = function(file) {
            mapshot(mapdown(), file = file,
                    remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                                        "drawToolbar", "easyButton"))
        }
    )

    
    ## * Table of categories
    
    output$text <- renderUI({
      if(input$sel_score == 'nps_score'){
        splitLayout(cellWidths = rep("25%", 4),
                    cellArgs = list(style='white-space: normal;'),
                    column(width= 12, 
                           h5(strong("A")),
                           h5("≥ 34"),
                           hr(),
                           h6(includeMarkdown('text/nps-a.md'))),
                    column(width= 12, 
                           h5(strong("B")),
                           h5("< 34 and ≥ 28"),
                           hr(),
                           h6(includeMarkdown('text/nps-b.md'))),
                    column(width= 12, 
                           h5(strong("C")),
                           h5("< 28 and ≥ 18"),
                           hr(),
                           h6(includeMarkdown('text/nps-c.md'))),
                    column(width= 12, 
                           h5(strong("D")),
                           h5("< 18"),
                           hr(),
                           h6(includeMarkdown('text/nps-d.md')))
        )
      } else if(input$sel_score == 'ibi_score'){
        splitLayout(cellWidths = rep("33%", 3),
                    cellArgs = list(style='white-space: normal;'),
                    column(width= 12, 
                           h5(strong("High quality")),
                           h5("> 67%"),
                           hr(),
                           # h6(includeMarkdown('text/nps-a.md'))
                           ),
                    column(width= 12, 
                           h5(strong("Medium quality")),
                           h5("≤ 67 and ≥ 33"),
                           hr(),
                           # h6(includeMarkdown('text/nps-b.md'))
                           ),
                    column(width= 12, 
                           h5(strong("Low quality")),
                           h5("< 33"),
                           hr(),
                           # h6(includeMarkdown('text/nps-c.md'))
                           )
        )
      }
        
    })

    

    output$download <- downloadHandler(
        filename = "IBI_scores.csv",
        content = function(fname) {
            fwrite(ibiData(), fname)
        })
    
})

