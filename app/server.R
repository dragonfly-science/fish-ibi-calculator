library(shiny)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(leaflet)
library(sf)
library(reactable)
library(ggplot2)
library(qs)

## load('data/species_ibi_metrics.rda', v=T)
## load('data/fish_names.rda', v=T)
## load('data/nz-fitted-quantiles.rdata', v=T)
## regions <- readRDS('data/regions.rds')

source('fishr-functions.R')
source('app-functions.r')

qload('data/app-data.qs')

options(debug = T)

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

input <- list(region = 'Wellington', nz_region = 'Wellington', target_upload = list(datapath='~/Downloads/test-data (copy).csv'))
input <- list(region = 'Wellington', nz_region = 'No Region', target_upload = list(datapath='~/Downloads/empty-test-data.csv'))
rv <- list(
    reqfields = req_fields,
    selfields = data.frame(req = names(req_fields), good = 0L),
    intable = NULL, tablefields = NULL, tablefields_ori = NULL,
    finalTable = NULL, region = 'Wellington'
)


shinyServer(function(input, output, session) {

  shinyjs::disable(selector = '#myFirst li a[data-value=">"]')

  rv <- reactiveValues(
    reqfields = req_fields,
    selfields = data.frame(req = names(req_fields), good = 0L),
    intable = NULL, tablefields = NULL, tablefields_ori = NULL,
    finalTable = NULL, test = NULL
  )


  observe({
    debuginfo('In observe shinyjs switches')
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

  output$region <- renderUI({ # Region selector widget
    debuginfo('Rendering UI region')
    labs <- c('No Region', regions$region)
    selectInput(inputId  = "region",
                multiple = FALSE,
                label    = div(class = 'input-label', "Select your region (optional):"),
                choices  = labs)
  })

  output$view_region_only <- renderUI({
    debuginfo("Rendering UI region_only")
    if (input$region != "No Region") {
      span(id = "toggle-switch-container",
          tags$label('for' = "view_region_only", class = 'toggle-label-l','National'),
          prettySwitch(
            inputId = "view_region_only",
            label = NULL,
            value = FALSE,
            inline = TRUE,
            fill = TRUE
            # status = 'primary'
          ),
          tags$label('for' = "view_region_only", class = 'toggle-label-r', input$region)
      )
    }
  })

  observeEvent(input$view_region_only, {
    if(input$view_region_only) {
      addClass(selector = "#toggle-switch-container label.toggle-label-r", class = "bold")
      removeClass(selector = "#toggle-switch-container label.toggle-label-l", class = "bold")
    } else {
      addClass(selector = "#toggle-switch-container label.toggle-label-l", class = "bold")
      removeClass(selector = "#toggle-switch-container label.toggle-label-r", class = "bold")
    }
  })
  
  observe({
    debuginfo('In observe file upload')

    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    rv[['intable']] <- NULL
    rv[['ignoredrows']] <- 0L
    rv[['finalTable']] <- NULL
    df <- read.csv(inFile$datapath, header = TRUE, stringsAsFactors = F)

    debuginfo(head(df))
    
    fields <- isolate(rv[['selfields']])
    fields$good <- ifelse(fields$req %in% names(df), 1, 0)
    rv[['selfields']] <- fields
    fields <- data.table(ori = names(df), good = names(df) %in% c(cols_needed, cols_opt))
    fields[, match := fifelse(ori %in% c(cols_needed, cols_opt), ori, '')]
    rv[['fieldsmatching']] <- as.data.frame(fields)
    rv[['intable']] <- df
    rv[['tablefields']] <- names(df)
    rv[['tablefields_ori']] <- names(df)

    disable('downloadissues')
    shinyjs::disable(selector = '#myFirst li a[data-value="2. Match headers"]')
    shinyjs::disable(selector = '#myFirst li a[data-value="3. Check input data"]')
    shinyjs::disable(selector = '#myFirst li a[data-value="4. Calculate IBI score"]')
    updateTabsetPanel(session, "myFirst",
                      selected = "2. Match headers")
  }, label='File upload')


  observeEvent(input$exbtn, {

    debuginfo('In observeEvent example load')
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

    disable('downloadissues')
    shinyjs::disable(selector = '#myFirst li a[data-value="2. Match headers"]')
    shinyjs::disable(selector = '#myFirst li a[data-value="3. Check input data"]')
    shinyjs::disable(selector = '#myFirst li a[data-value="4. Calculate IBI score"]')
    updateTabsetPanel(session, "myFirst",
                      selected = "2. Match headers")
  }, label = 'Demo loading')

  ## * List of required fields
  output$mandfields <- renderUI({
    debuginfo('Rendering UI mandfields')

    fm <- rv$fieldsmatching
    prettyCheckboxGroup(inputId='mandfields',  label = 'Mandatory fields:', choices = cols_needed,
                        shape = 'round', icon = icon('check'), animation = 'pulse',
                        selected = intersect(cols_needed, fm$match[fm$good==T]))
  })
  observe({
    debuginfo('Updating UI mandfields')
    fm <- rv$fieldsmatching
    updatePrettyCheckboxGroup(session, inputId='mandfields', label = 'Mandatory fields:',
                              choices = cols_needed,
                              selected = intersect(cols_needed, fm$match[fm$good==T]),
                              prettyOptions = list(shape = 'round', icon = icon('check'),
                                                   animation = 'pulse'))
  })

  ## * Assigment of field roles from context menu

  observeEvent(input$selfield, {
    debuginfo('In observedEvent Fields renaming')
    if (!is.null(input$selfield)) {
      pair <- strsplit(input$selfield, '=')[[1]]
      if (pair[1] == 'None') {
        pair[1] <- ''
      }
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
      debuginfo(rv[['fieldsmatching']])
      debuginfo(rv[['selfields']])
    }
  }, label = 'Fields renaming')


  ## * Raw input table

  output$dtable <- renderReactable({
    debuginfo('Rendering raw input table')
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
    debuginfo('In Enable/disable button to next on page 2')
    if(all(rv$selfields$good == 1)) {
      enable('checkData')
      rv$intable <- copy(rv$intable)
    } else{
      disable('checkData')
    }
  }, label = 'Enable/disable button to next on page 2')

  # check data button
  observeEvent(input$checkData, {
    debuginfo('In Go to page 3')
    updateTabsetPanel(session, "myFirst",
                      selected = "3. Check input data")
  }, label = 'Go to page 3')

  # button to go from tab 2 to 3
  observe({
    debuginfo('In Enable/disable button to next on page 3')
    if(dataissues()) {
      disable('to4btn')
      enable('remIssuesBtn')
      enable('downloadissues')
      enable('issue_type')
    } else {
      enable('to4btn')
      disable('remIssuesBtn')
      disable('issue_type')
    }
  }, label = 'Enable/disable button to next on page 3')

  renamedTable <- reactive({
    req(rv$intable)
    tab <- copy(rv$intable)
    setDT(tab)
    req(all(rv$selfields$good == 1))
    debuginfo(sum(rv$selfields$good))
    setnames(tab, rv$fieldsmatching$ori, rv$fieldsmatching$match)
    debuginfo(tab)
    return(copy(tab))
  })

  
  ## * Processed table and issues =============================================================

  ## ** Table cleaned and with identified issues

  cleanTable <- reactive({
    debuginfo('In cleanTable()')
    tab <- renamedTable()
    req(tab)
    ## req(all(c('SpeciesCode', 'Date', 'SiteID', 'Penetration', 'Altitude') %in% names(rv$intable)))
    ## debuginfo(head(rv$intable))
    debuginfo(tab)
    fm <- rv$fieldsmatching
    debuginfo(rv$fieldsmatching)
    d <- copy(rv$intable)
    cols <- fm$ori[fm$good == T]
    goodcols <- intersect(c(cols_needed, cols_opt), fm$match[fm$good==T])
    d <- rv$intable[, fm$ori[match(goodcols, fm$match)], drop=F]
    names(d) <- goodcols
    setDT(d)
    d[, Stratum := paste0(Date, '_', SiteID, '_', Penetration, '_', Altitude)]
    d[, SpeciesCode := tolower(SpeciesCode)]
    
    ## *** Standardise species codes
    setnames(d, 'SpeciesCode', 'SpeciesCode.ori')
    d[species_codes, SpeciesCode := i.spp_code_2004, on = c('SpeciesCode.ori' = 'nzffd_spp_code')]
    d[is.na(SpeciesCode) & SpeciesCode.ori == 'nospec', SpeciesCode := 'nospec']

    if (input$region != 'No Region')
      d[, ReportingRegion := input$region]

    debuginfo(d)
    
    ## *** Penetration
    if ('Penetration' %in% names(d)) {
      ## **** Check for missing values
      c <- is.na(d$Penetration)
      if (any(c)) {
        d[c, `:=`(Penetration_issues = 1L,
                  Penetration_txt    = 'Penetration is missing')]
      }
      ## **** Check for non-numeric values
      suppressWarnings({c <- !is.na(d$Penetration) & is.na(as.numeric(d$Penetration))})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Penetration_issues = 1L,
                  Penetration_txt    = 'Penetration should be numeric')]
      }
      ## **** Check for negative values
      suppressWarnings({c <- !is.na(d$Penetration) & as.numeric(d$Penetration) < 0})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Penetration_issues = 1L,
                  Penetration_txt    = 'Penetration should be positive')]
      }
      ## **** Check for excessive values
      suppressWarnings({c <- grepl('^[0-9.]+$', d$Penetration) & as.numeric(d$Penetration) > 2000})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Penetration_issues = 1L,
                  Penetration_txt    = 'Penetration too high')]
      }
    }
    ## *** Altitude
    if ('Altitude' %in% names(d)) {
      ## **** Check for missing values
      c <- is.na(d$Altitude)
      if (any(c)) {
        d[c, `:=`(Altitude_issues = 1L,
                  Altitude_txt    = 'Altitude is missing')]
      }
      ## **** Check for non-numeric values
      suppressWarnings({c <- !is.na(d$Altitude) & is.na(as.numeric(d$Altitude))})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Altitude_issues = 1L,
                  Altitude_txt    = 'Altitude should be numeric')]
      }
      ## **** Check for negative values
      suppressWarnings({c <- !is.na(d$Altitude) & as.numeric(d$Altitude) < 0})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Altitude_issues = 1L,
                  Altitude_txt    = 'Altitude should be positive')]
      }
      ## **** Check for excessive values
      suppressWarnings({c <- !is.na(d$Altitude) & as.numeric(d$Altitude) > 3600})
      if (!all(is.na(c)) && any(c)) {
        d[c, `:=`(Altitude_issues = 1L,
                  Altitude_txt    = 'Altitude too high')]
      }
    }
    ## *** SpeciesCode
    if ('SpeciesCode' %in% names(d)) {
      d$SpeciesCode <- tolower(d$SpeciesCode)
      ## **** Check for missing values
      c <- is.na(d$SpeciesCode)
      if (any(c)) {
        d[c, `:=`(SpeciesCode_issues = 1L,
                  SpeciesCode_txt    = 'Species is missing')]
      }
      ## **** Check for existence
      c <- !(d$SpeciesCode %in% c(species_codes$spp_code_2004, 'nospec'))
      if (any(c)) {
        d[c, `:=`(SpeciesCode_issues = 1L,
                  SpeciesCode_txt    = 'Species code not recognised')]
      }
      ## **** Check existence in species_ibi_metrics
      c <- !(d$SpeciesCode %in% c(species_ibi_metrics$spcode, 'nospec'))
      if (any(c)) {
        d[c, `:=`(SpeciesCode_warnings = 1L,
                  SpeciesCode_wtxt     = 'Non-fish code or species without IBI metrics')]
      }
      ## **** Check for nospec alongside other species
      d[, `:=`(n_spp = uniqueN(setdiff(SpeciesCode, 'nospec')),
               n_nosp = sum(SpeciesCode %in% 'nospec'))
      , Stratum]
      c <- d[, SpeciesCode %in% 'nospec' & n_spp > 0]
      if (any(c)) {
        d[c, `:=`(SpeciesCode_warnings = 1L,
                  SpeciesCode_wtxt     = '"nospec" alongside some records of species')]
      }
    }
    ## *** SiteID
    if ('SiteID' %in% names(d)) {
      ## **** Check for missing values
      c <- is.na(d$SiteID)
      if (any(c)) {
        d[c, `:=`(SiteID_issues = 1L,
                  SiteID_txt    = 'Site ID is missing')]
      }
    }
    ## *** Date
    if ('Date' %in% names(d)) {
      ## **** Check for missing values
      c <- is.na(d$Date)
      if (any(c)) {
        d[c, `:=`(Date_issues = 1L,
                  Date_txt    = 'Date is missing')]
      }
    }
    ## *** Region
    if (all(c('Easting', 'Northing') %in% names(d)) & input$region != 'No Region') {
      coords <- unique(d[!is.na(Easting) & !is.na(Northing), .(Easting, Northing)])
      pts <- st_as_sf(coords, coords = c('Easting', 'Northing'), crs = 27200, remove = F)
      ptsreg <- st_join(pts, regions)
      setDT(ptsreg)
      d[ptsreg, PointRegion := i.region, on = c('Easting', 'Northing')]
      c <- d[, !is.na(Easting) & !is.na(Northing) & PointRegion != input$region]
      if (any(c)) {
        d[c, `:=`(PointRegion_warnings  = 1L,
                  PointRegion_wtxt  = 'Point region different from the one specified')]
      }
    }
    d$OriginalRow <- seq_len(nrow(d))
    setcolorder(d, 'OriginalRow')
    debuginfo(head(d))

    as.data.frame(d)

  })

  observe({
    debuginfo('In observe rv$finalTable <- ft')

    d <- cleanTable()
    req(d)

    ft <- copy(d)
    cat('\n* cleanTable -> finalTable\n')
    rv$finalTable <- ft

  })

  issues_long <- reactive({ # one row per issue and record
    debuginfo('In issues_long()')
    d <- cleanTable()

    d <- as.data.table(copy(d))
    req(d)
    if (any(grepl('_txt$', names(d)))) {
      e_long <- melt(d, id.var = 'OriginalRow', measure.vars = patterns('_txt$'), value.name = 'issue')[
      , type := 'error']
    } else e_long <- NULL
    if (any(grepl('_wtxt$', names(d)))) {
      w_long <- melt(d, id.var = 'OriginalRow', measure.vars = patterns('_wtxt$'), value.name = 'issue')[
      , type := 'warning']
    } else w_long <- NULL
    if (!is.null(e_long) | !is.null(w_long)) {
      long0 <- rbind(e_long, w_long)
    } else long0 <- NULL
    long0

  })

  issues_summary <- reactive({ # one row per issue
    debuginfo('In issues_summary()')
    long0 <- issues_long()
    debuginfo(long0)
    req(long0)

    if (!is.null(long0)) {
      long <- long0[!is.na(issue)]
      l <- long[, .N, .(type, issue)]
    } else l <- NULL
    debuginfo(l)
    l

  })

  output$issue_type <- renderUI({ # issue selector widget
    debuginfo('Rendering UI issue_type')
    l <- issues_summary()
    req(l)
    
    labs <- l[, setNames(c('All issues', issue),
                         c('All issues', sprintf('%s: %s (%s)', upper1st(type), issue, N)))]
    debuginfo(labs)
    selectInput(inputId  = "issue_type",
                multiple = FALSE,
                label    = div(class = 'input-label', "Show issues:"),
                choices  = labs)
  })

  dataissues <- reactive({ # boolean: is there any issue? yes:1 no:0
    debuginfo('In dataissues()')
    ft <- rv$finalTable
    debuginfo(rv$finalTable)
    req(ft)
    withissue   <- apply(ft[, grep('_issues$',   names(ft), val=T), drop=F], 1, function(x) any(x %in% 1))
    withwarning <- apply(ft[, grep('_warnings$', names(ft), val=T), drop=F], 1, function(x) any(x %in% 1))
    debuginfo(sum(withissue))
    debuginfo(sum(withwarning))
    if (any(withissue | withwarning)) {
      return(1)
    } else {
      return(0)
    }
  }, label = 'Data with issues?')


  ## ** Download issues

  output$downloadissues <- downloadHandler(
    filename = "IBI_data_issues.csv",
    content = function(fname) {
      d <- cleanTable()
      withissues <- apply(d[, grep('_issues$|_warnings$', names(d), val = T), drop=F], 1, function(x) any(x %in% 1))
      di <- d[withissues, ]
      di$issues <- apply(di[, grep('_txt$', names(di), val = T), drop=F], 1,
                         function(x) paste(na.omit(x), collapse = '; '))
      di$warnings <- apply(di[, grep('_wtxt$', names(di), val = T), drop=F], 1,
                           function(x) paste(na.omit(x), collapse = '; '))
      di <- di[, -grep('_issues$|_txt$|_warnings$|_wtxt$', names(d))]
      setcolorder(di, c('OriginalRow'))
      di$issues[di$issues == 'NA'] <- NA
      di$warnings[di$warnings == 'NA'] <- NA
      setnames(di, c('issues', 'warnings'), c('Issues', 'Warnings'), skip_absent = T)
      fwrite(di, fname, na = '')
    })


  ## ** Filtered issues

  selectedissues <- reactive({
    debuginfo('In selectedissues()')

    ft <- copy(rv$finalTable)
    setDT(ft)
    req(ft)
    long <- copy(issues_long())
    if (dataissues() & !is.null(input$issue_type)) {
      if (input$issue_type != 'All issues') {
        iids <- ft[long[issue %in% input$issue_type], on = 'OriginalRow'][, sort(unique(OriginalRow))]
      } else {
        iids <- ft[long[!is.na(issue)], on = 'OriginalRow'][, sort(unique(OriginalRow))]
      }
      dsel <- ft[match(iids, OriginalRow)]
    } else {
      dsel <- copy(rv$finalTable)
    }
    debuginfo('Done with selectedissues()')

    as.data.frame(copy(dsel))
  })

  ## ** Cleaned table on page 3

  output$newTable <- renderReactable({
    debuginfo('Rendering UI newTable')

    dsel <- selectedissues() #rv$finalTable
    req(dsel)

    cols2hide <- sapply(grep('n_spp$|n_nosp$|_issues$|_warnings$|_txt$|_wtxt$', names(dsel), val=T),
                        function(x) colDef(show = FALSE), simplify = F)
    debuginfo('Rendering UI newTable - done step 1')

    dt <- reactable(
      dsel, highlight=T, compact=T, wrap=F, resizable = T,
      defaultColDef = colDef(
        align = 'left', html = T,
        class = function(value, index, name) {
          d <- dsel[index,]
          if ((paste0(name,'_warnings') %in% names(d) &&
                 !is.na(d[[paste0(name,'_warnings')]]) &&
                 d[[paste0(name,'_warnings')]] > 0) |
                (paste0(name,'_issues') %in% names(d) &&
                   !is.na(d[[paste0(name,'_issues')]]) &&
                   d[[paste0(name,'_issues')]] > 0))
            return('CellWithComment')
        },
        style = function(value, index, name) {
          d <- dsel[index,]
          if (paste0(name,'_warnings') %in% names(d) &&
                !is.na(d[[paste0(name,'_warnings')]]) &&
                d[[paste0(name,'_warnings')]] > 0) {
            return(list(background='#1F3B7133', color='#003547', fontWeight='normal'))
          }
          if (paste0(name,'_issues') %in% names(d) &&
                !is.na(d[[paste0(name,'_issues')]]) &&
                d[[paste0(name,'_issues')]] > 0) {
            return(list(background='#BF2F3733', color='#003547', fontWeight='normal'))
          }
        },
        cell = function(value, index, name) {
          d <- dsel[index,]
          if (is.na(value))
            value <- ''
          v <- value
          if (paste0(name,'_warnings') %in% names(d) &&
                paste0(name,'_wtxt') %in% names(d) &&
                !is.na(d[[paste0(name,'_warnings')]]) &&
                d[[paste0(name,'_warnings')]] > 0) {
            v <- sprintf('%s <span class="cellcomment cellcomment--warn"><b>Warning:</b> %s</span>',
                         value, d[[paste0(name,'_wtxt')]])
          }
          if (paste0(name,'_issues') %in% names(d) &&
                paste0(name,'_txt') %in% names(d) &&
                !is.na(d[[paste0(name,'_issues')]]) &&
                d[[paste0(name,'_issues')]] > 0) {
            v <- sprintf('%s <span class="cellcomment cellcomment--error"><b>Error:</b> %s</span>',
                         value, d[[paste0(name,'_txt')]])
          }
          v
        }),
      columns = cols2hide
    )
    debuginfo('Done rendering UI newTable')
    
    dt
  })


  ## ** Exclude issues

  observeEvent(input$remIssuesBtn, {
    debuginfo('In observeEvent "Ignore visits with issues"')

    d <- cleanTable()
    d <- as.data.table(d)
    req(d)

    d0 <- copy(d)

    ## *** Collapse rows to "nospec" if no IBI species were present within the stratum
    d0[, nospec := all(!(SpeciesCode %in% species_ibi_metrics$spcode | SpeciesCode %in% 'nospec'))
    , Stratum]
    nospec1 <- d0[nospec == T, .SD[1], Stratum][, SpeciesCode := 'nospec']
    d1 <- rbind(d0[nospec == F], nospec1)
    rv$ignoredrows <- nrow(d0) - nrow(d1)
    d0 <- d1
    d0[, nospec := NULL]
    ## *** Remove complete strata with some issues
    if (any(grepl('_issues$', names(d0)))) {
      d0[, with_issue := rowSums(.SD, na.rm=T) > 0,
        .SDcols = patterns('_issues$')]
      d0[, site_issue := any(with_issue %in% T), .(SiteID, Date)]
      rv$ignoredrows <- rv$ignoredrows + sum(d0$site_issue == T)
      d0 <- d0[site_issue == F, -c('with_issue', 'site_issue'), with = F]
    }
    ## *** Remove individual records with some warnings
    if (any(grepl('_warnings$', names(d0)))) {
      d0[, with_warnings := rowSums(.SD, na.rm=T) > 0,
        .SDcols = patterns('_warnings$')]
      rv$ignoredrows <- rv$ignoredrows + sum(d0$with_warnings == T)
      d0 <- d0[with_warnings == F, -c('with_warnings'), with=F]
    }
    rv$finalTable <- as.data.frame(d0)

  }, label = 'Ignore visits with issues')


  ## ** Feedback on issues

  output$issuesTxt <- renderText({
    debuginfo('Rendering text issuesTxt')

    d <- rv$finalTable
    req(d)
    setDF(d)
    withissues <- rowSums(d[, grep('_issues$|_warnings$', names(d), val=T), drop=F], na.rm = T)
    n.issues        <- sum(withissues)
    n.ignoredrows   <- ifelse(is.null(rv$ignoredrows), 0, rv$ignoredrows)
    strata.with.issues <- unique(d[withissues > 0, 'Stratum'])
    strata.without.issues <- setdiff(unique(d$Stratum), strata.with.issues)
    n.rows.noissues <- nrow(d[d$Stratum %in% strata.without.issues & withissues == 0,])
    sprintf('%s<br>%i valid rows%s',
            ifelse(n.issues == 0,
                   sprintf('No issues found in %i strata', length(strata.without.issues)),
                   sprintf('%i data issues were found in %i (out of %i) strata', n.issues, length(strata.with.issues),
                           uniqueN(d$Stratum))),
            n.rows.noissues,
            ifelse(n.ignoredrows == 0, '',
                   sprintf('<br>(%s rows were excluded)', n.ignoredrows)))
  })


  output$issuesIcon <- renderText({
    debuginfo('Rendering text issuesIcon')
    if (dataissues()) {
      as.character(icon('exclamation-triangle', class='dangerico'))
    } else {
      as.character(icon('check-square', class='nodangerico'))
    }
  })


  output$issueImg <- renderImage({
    debuginfo('Rendering image issueImg')
    d <- rv$finalTable
    req(d)
    n.issues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T), drop=F], na.rm=T))
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
    debuginfo('Rendering text issuesSubTxt')

    d <- rv$finalTable
    req(d)
    n.issues <- sum(rowSums(d[, grep('_issues$|_warnings$', names(d), val=T), drop=F], na.rm=T))
    if (n.issues > 0) {
      return('<p>Please correct the following issues and re-upload the file, or exclude the issues. Excluding issues removes the individual records for non-fish code issues. For all other issues, all records associated with the particular site visit are excluded from the analysis.</p>
             <p>In the table below, cells are coloured red if they have errors, or blue/grey if they have warnings. Hover your mouse over cells to find more information about the issues, or filter the table using the button to the right.</p>
             <p>You can download a comma-separated table of issues using the button on the far right.</p>')
    } else {
      return('The table below will be used as it is to calculate the IBI scores')
    }
  })

  observeEvent(input$testbtn, {
    print(cleanTable())
    print(rv$finalTable)

  })

  observeEvent(input$to4btn, {
    debuginfo('In observeEvent "Go to page 4"')
    updateTabsetPanel(session, "myFirst",
                      selected = "4. Calculate IBI score")
  }, label = 'Go to page 4')


  ## * IBI scores =========================================================================

  ibiData <- reactive({
    debuginfo('Rendering text issuesSubTxt')

    ft <- rv$finalTable
    req(ft)

    ft$Altitude <- as.numeric(ft$Altitude)
    ft$Penetration <- as.numeric(ft$Penetration)

    ft <- ft[order(ft$SiteID, ft$Date, ft$SpeciesCode),]

    site_metrics_all <- ft |>
      prep.site.metrics(species.ibi.metrics = species_ibi_metrics, stratum.incl.alt.penet = T)
    setorder(site_metrics_all, Stratum)

    ## ibi_scores <- site_metrics_all |>
    ##   add.fish.metrics(q1e=qr.1.elev, q2e=qr.2.elev, q3e=qr.3.elev, q4e=qr.4.elev, q5e=qr.5.elev,
    ##                    q1p=qr.1.penet, q2p=qr.2.penet, q3p=qr.3.penet, q4p=qr.4.penet, q5p=qr.5.penet) |>
    ##   add.fish.metric6() |>
    ##   add.fish.ibi() |>
    ##   nps()

    nz_thresh <- ibi_thresh['No Region']

    ibi_scores <- site_metrics_all |>
      add.fish.metrics(cal = calibrations) |>
      add.fish.metric6() |>
      add.fish.ibi() |>
      nps(lq = nz_thresh$q1, med = nz_thresh$median, uq = nz_thresh$q3)

    ## ** Add region NPS category if region is specified
    reg_thresh <- ibi_thresh[input$region] 
    if (input$region != 'No Region') {
      ibi_scores <- nps(ibi_scores, lq = reg_thresh$q1, med = reg_thresh$median, uq = reg_thresh$q3,
                        colname = paste0('Regional_IBI_category_', gsub('\'', '', gsub(' ', '_', input$region))))
    }

    cols2rem <- c(grep('metric', names(ibi_scores), val = T))
    ibi_scores[, (cols2rem) := NULL]

    setnames(ibi_scores, c('total_sp_richness', 'number_non_native'),
             c('Species_richness', 'Species_non_native'))
    
    ibi_scores
  })

  
  ## * Table of IBI scores
  output$ibiTable <- renderReactable({
    debuginfo('Rendering ibiTable')
    ibi_scores <- ibiData()
    
    req(setDT(ibi_scores))
    ibi_scores_table <- copy(ibi_scores)

    if (any(grepl('Regional_IBI_category_', names(ibi_scores_table)))) {
      setnames(ibi_scores_table, grep('Regional_IBI_category_', names(ibi_scores_table), val = T),
               paste0('Regional IBI category ', input$region))
    }
    setnames(ibi_scores_table, gsub('_', ' ', names(ibi_scores_table)))

    dt <- reactable(ibi_scores_table, highlight=T, compact=T, wrap=F, defaultColDef = colDef(align = 'left'),
                    resizable = T)

    dt
  })


  ## * Scores plot

  output$scoresPlot <- renderPlot({
    debuginfo('Rendering scoresPlot')

    ibi_scores <- ibiData()

    req(ibi_scores)
    req(input$region)
    if (!is.null(input$view_region_only) && input$view_region_only != FALSE) {
      ibi_scores$NPScategory <- ibi_scores[, get(grep('Regional_IBI_category_', names(ibi_scores), val = T))]
      xlab <- sprintf('%s region IBI category', input$region)
    } else {
      ibi_scores$NPScategory <- ibi_scores$NPS_category
      xlab <- "NPS-FM category"
    }
    ibi_scores$NPScategory[is.na(ibi_scores$NPScategory)] <- 'Unknown'
    ibi_scores$NPScategory <- factor(as.character(ibi_scores$NPScategory),
                                      levels = c('A', 'B', 'C', 'D', 'Unknown', 'No species'))
    
    group.colors <- c('No species' = "#565659",
                      'A'       = "#00C7A8",
                      'B'       = "#2C9986",
                      'C'       = "#004A6D",
                      'D'       = "#BF2F37",
                      'Unknown' = '#d4dde1')
    group.colors <- group.colors[names(group.colors) %in% ibi_scores$NPScategory]

    g <- ggplot(ibi_scores, aes(x = NPScategory, fill = NPScategory)) +
      xlab(xlab)

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
    ## filename = rv$plot_filename,
    filename = function() {
      if (!is.null(input$view_region_only) && input$view_region_only != FALSE) {
        fn <- sprintf('IBI-category-distribution_%s-region.png', gsub('\'', '', gsub(' ', '-', input$region)))
      } else {
        fn <- 'IBI-category-distribution_national.png'
      }
      fn 
    },
    content = function(file) {
      req(rv$scoredistplot)
      ggsave(file, plot = rv$scoredistplot, device = "png", width = 7.5, height = 5)
    }
  )


  ## * MAP

  output$map <- renderLeaflet({
    debuginfo('Rendering map')

    ibi_scores <- copy(ibiData())

    if (!is.null(input$view_region_only) && input$view_region_only != FALSE) {
      ibi_scores$IBIcategory <- ibi_scores[, get(grep('Regional_IBI_category_', names(ibi_scores), val = T))]
      leg.title <- sprintf('%s region<br>IBI category', input$region)
    } else {
      ibi_scores$IBIcategory <- ibi_scores$NPS_category
      leg.title <- "NPS-FM category"
    }

    ibi <- as.data.table(ibi_scores)
    req(ibi)
    dt <- as.data.table(rv$finalTable)

    req(input$region)
    if (!(input$region %in% 'No Region'))
      req(!is.null(input$view_region_only))
    
    if (all(c('Easting', 'Northing') %in% names(dt))) {

      coords <- dt[, .(x = mean(Easting, na.rm=T), y = mean(Northing, na.rm=T)), .(Stratum)]
      ibi <- merge(ibi, coords, by = 'Stratum', all = T)
      ibi <- st_as_sf(ibi, coords = c('x', 'y'), crs = 27200)
      ibi <- st_transform(ibi, crs = 4326)
      ibi <- cbind(ibi, st_coordinates(ibi))
      ibi <- as.data.table(ibi)
      ibi <- ibi[, .(Date, SiteID, Penetration, Altitude, IBI_score, IBIcategory,
                     Species_richness, Species_non_native, X, Y)]

      ibi[is.na(IBIcategory), IBIcategory := 'Unknown']
      ibi[, IBIcategory := factor(as.character(IBIcategory), levels = c('A', 'B', 'C', 'D', 'Unknown', 'No species'))]
      factcols <- colorFactor(c('#00C7A8', '#2C9986', '#004A6D', '#BF2F37', '#808080', "#565659"),
                              domain = NULL)
      ibi[, Colour := factcols(IBIcategory)]

      ## Raw HTML for the map tooltip/label
      ibi[, labels := paste0(
        sprintf(
          '<div class="maptip">
            <div class="maptip--header">
              <div>
                Site ID:<br/>
                <span class="maptip--header__siteID">%s</span>
              </div>
              <div class="maptip--header__swatch" style="background-color: %s;"></div>
            </div>
            <div class="maptip--main">
              <div class="maptip--row">
                <span class="maptip--row__left">Date:</span><span class="maptip--row__right">%s</span>
              </div>
              <div class="maptip--row">
                <span class="maptip--row__left">IBI score:</span><span class="maptip--row__right">%s</span>
              </div>
              <div class="maptip--row">
                <span class="maptip--row__left">%s:</span><span class="maptip--row__right">%s</span>
              </div>
              <div class="maptip--row">
                <span class="maptip--row__left">Total sp richness:</span><span class="maptip--row__right">%s</span>
              </div>
              <div class="maptip--row">
                <span class="maptip--row__left">Non-native ssp:</span><span class="maptip--row__right">%s</span>
              </div>
            </div>
          </div>',
          SiteID, Colour, Date, IBI_score, leg.title, IBIcategory, Species_richness, Species_non_native
        )
      )
    , by = 1L:nrow(ibi)]


      factcols <- colorFactor(c('#00C7A8', '#2C9986', '#004A6D', '#BF2F37', '#808080', "#565659"),
                              domain = NULL)
      fc = ~factcols(IBIcategory)
      c = ~factcols(IBIcategory)

      npss <- data.table(label = c('A', 'B', 'C', 'D', 'Unknown', 'No species'),
                         color = c('#00C7A8', '#2C9986', '#004A6D', '#BF2F37', '#808080', "#565659"))
      npss <- npss[as.character(label) %in% ibi$IBIcategory]
      
      if (!(input$region %in% 'No Region') && !(input$view_region_only %in% FALSE)) {
        leg.title <- sprintf('%s region<br>IBI category', input$region)
      } else leg.title <- "NPS-FM category"

      rv$map <- leaflet() |>
        setView(173.6, -41, zoom = 5) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        addCircleMarkers(data = ibi, lng = ~X, lat = ~Y,
                         fillColor = fc, color = c,
                         popup = ~labels |> lapply(htmltools::HTML),
                         popupOptions = labelOptions(
                           direction = "auto", sticky = F,
                           maxWidth = 700, closeOnClick = T, closeButton = F),
                         ## radius = ~radius,
                         fillOpacity = 0.7,
                         radius = 4,
                         opacity = 1,
                         weight = 1
                         ) |>
        addLegend(
          data = ibi, "bottomright",
          colors = paste0(npss$color, "; opacity: 0.5; width: 10px; height: 10px; border-radius: 50%"),
          labels = paste0("<div style='display: inline-block; height: 10px; margin-top: 4px; line-height: 10px;'>", npss$label, "</div>"),
          title = leg.title, opacity = 1) |>
        leaflet.extras::addFullscreenControl()

      rv$map

    }

  })

  mapdown <- reactive({
    debuginfo('in mapdown()')
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    m <- rv$map |>
      setView(lng = (lngRng[1]+lngRng[2])/2, lat = (latRng[1]+latRng[2])/2, zoom = input$map_zoom)
    m$x$options$fullscreenControl <- NULL # remove fullscreen control (others are removed automatically)
    m
  })

  output$mapdl <- downloadHandler(
    filename = function() {
      if (!is.null(input$nview_region_only) && input$view_region_only != FALSE) {
        fn <- sprintf('IBI-category-map_%s-region.png', gsub('\'', '', gsub(' ', '-', input$region)))
      } else {
        fn <- 'IBI-category-map_national.png'
      }
      fn 
    },
    content = function(file) {
      mapview::mapshot(mapdown(), file = file,
                       remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                                           "drawToolbar", "easyButton"))
    }
  )


  ## * Table of categories

  output$text <- renderUI({
    debuginfo('Rendering categories')
    
    if (!is.null(input$nview_region_only) && input$view_region_only != FALSE) {
      thresh <- ibi_thresh[input$region]
    } else thresh <- ibi_thresh['No Region']

    splitLayout(cellWidths = rep("25%", 4),
                cellArgs = list(style='white-space: normal;'),
                column(width= 12,
                       h5(strong("A")),
                       h5(paste0("≥ ", thresh$q3)),
                       hr(),
                       h6(includeMarkdown('text/nps-a.md'))),
                column(width= 12,
                       h5(strong("B")),
                       h5(sprintf("< %i and ≥ %i", thresh$q3, thresh$median)),
                       hr(),
                       h6(includeMarkdown('text/nps-b.md'))),
                column(width= 12,
                       h5(strong("C")),
                       h5(sprintf("< %i and ≥ %i", thresh$median, thresh$q1)),
                       hr(),
                       h6(includeMarkdown('text/nps-c.md'))),
                column(width= 12,
                       h5(strong("D")),
                       h5(paste0("< ", thresh$q1)),
                       hr(),
                       h6(includeMarkdown('text/nps-d.md')))
                )

  })

  output$download <- downloadHandler(
    filename = function() {sprintf("IBI-scores_%s.csv", format(Sys.Date()))},
    content = function(fname) {
      fwrite(ibiData(), fname)
    })

})

