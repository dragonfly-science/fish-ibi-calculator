library(data.table)
library(tidyverse)
library(quantreg)

source('fishr-functions.R')

load('data/species_ibi_metrics.rda', v=T)

load('data/fish_names.rda', v=T)
setDT(fish_names)
## load('../fishr/data/fish_names.rda', v=T)
load('../fishr/data/fish_methods.rda', v=T)
setDT(fish_methods)

load('../fishr/data/fish.rda', v=T)
setDT(fish)

cols_needed <- c('SiteID', 'Date', 'Penetration', 
                 'Altitude', 'SpeciesCode')

fish[, date := as.Date(sprintf('%i-%i-01', y, m))]
fish[fish_methods,
     method := i.Method,
     on = c('fishmeth' = 'Abbreviation')]

fish2 <- fish[method == 'Backpack' & nzreach > 10 & y %between% c(1999, 2018)]

setnames(fish2,
         c('date', 'catchname', 'spcode', 'altitude', 'penet'),
         c('Date', 'SiteID', 'SpeciesCode', 'Altitude', 'Penetration'))

fish2 <- fish2[, cols_needed, with = F]

site_metrics_all <- prep.site.metrics(fish2)

d <- copy(fish2)


## * Check data issues

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

    d$SpeciesCode <- tolower(d$SpeciesCode)

    ## *** Check for missing values
    c <- is.na(d$SpeciesCode)
    if (any(c)) {
        d[c, 'SpeciesCode_issues'] <- 1L
        d[c, 'SpeciesCode_txt']    <- 'Value is missing'
    }
    ## *** Check for existence
    c <- !(d$SpeciesCode %in% fish_names[['NZFFD code']])
    if (any(c)) {
        d[c, 'SpeciesCode_issues'] <- 1L
        d[c, 'SpeciesCode_txt']    <- 'Species code not recognised'
    }
    ## *** Check existence in species_ibi_metrics
    c <- !(d$SpeciesCode %in% species_ibi_metrics$spcode)
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

## * Summarise issues

v='SiteID_issues'
for (v in grep('_issues$', names(d), val=T)) {
    ## cat('\n==', v, '\n')
    vt <- sub('_issues$', '_txt', v)
    if (d[, any(!is.na(get(vt)))]) {
        d[!is.na(get(v))]
        print(d[!is.na(get(vt)), .N, by=vt])
    }
}

## * Remove issues

if (any(grepl('_issues$', names(d)))) {
    d[, with_issue := rowSums(.SD, na.rm=T) > 0,
      .SDcols = patterns('_issues$')]
    d[, site_issue := any(with_issue %in% T), .(SiteID, Date)]
    ignoredrows <- sum(d$site_issue == T)
    d <- d[site_issue == F, -c('with_issue', 'site_issue'), with = F]
}
if (any(grepl('_warnings$', names(d)))) {
    d[, with_warnings := rowSums(.SD, na.rm=T) > 0,
      .SDcols = patterns('_warnings$')]
    ignoredrows <- ignoredrows + sum(d$with_warnings == T)
    d <- d[with_warnings == F, -c('with_warnings'), with=F]
}

d

## * IBI calculation

d$Altitude <- as.numeric(d$Altitude)
d$Penetration <- as.numeric(d$Penetration)

site_metrics_all <- d %>%
    prep.site.metrics(species.ibi.metrics = species_ibi_metrics)

qr.1.elev <- qr.construct(y="metric1", x="Altitude", data = site_metrics_all)
qr.2.elev <- qr.construct("metric2", "Altitude", data = site_metrics_all)
qr.3.elev <- qr.construct("metric3", "Altitude", data = site_metrics_all)
qr.4.elev <- qr.construct("metric4", "Altitude", data = site_metrics_all)
qr.5.elev <- qr.construct("metric5", "Altitude", data = site_metrics_all)

qr.1.penet <- qr.construct("metric1", "Penetration", data = site_metrics_all)
qr.2.penet <- qr.construct("metric2", "Penetration", data = site_metrics_all)
qr.3.penet <- qr.construct("metric3", "Penetration", data = site_metrics_all)
qr.4.penet <- qr.construct("metric4", "Penetration", data = site_metrics_all)
qr.5.penet <- qr.construct("metric5", "Penetration", data = site_metrics_all)

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

ibi_scores <- as.data.table(ibi_scores)[unique(as.data.table(d)[, .(SiteID, Date, Altitude, Penetration)]), on = c('Date', 'SiteID', 'Altitude','Penetration')]


save(qr.1.elev, qr.2.elev, qr.3.elev, qr.4.elev, qr.5.elev,
     qr.1.penet, qr.2.penet, qr.3.penet, qr.4.penet, qr.5.penet,
     file = 'data/nz-fitted-quantiles.rdata')

saveRDS(ibi_scores, 'data/NZ-IBI-scores.rds')
