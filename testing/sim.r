## library(shiny)
## library(shinyjs)
library(data.table)
## library(DT)
library(tidyverse)
library(quantreg)
library(ggplot2)
## library(shinyWidgets)
## library(leaflet)
## library(sf)
## ## library(tmap)
## library(leaflet.extras)
## library(kableExtra)
## library(mapview)
## library(reactable)
## library(tippy)

load('../app/data/species_ibi_metrics.rda', v=T)
load('../app/data/fish_names.rda', v=T)
source('../app/fishr-functions.R')

cols_needed <- c('SiteID', 'Date', 'Penetration', 
                 'Altitude', 'SpeciesCode')

dat <- fread('../app/data/example-data.csv')


## * Clean data

dat <- dat[, cols_needed, with=F]

## ** Penetration
if ('Penetration' %in% names(dat)) {
    ## *** Check for missing values
    c <- is.na(dat$Penetration)
    if (any(c)) {
        dat[c, 'Penetration_issues'] <- 1L
        dat[c, 'Penetration_txt']    <- 'Value is missing'
    }
    ## *** Check for non-numeric values
    suppressWarnings({c <- !is.na(dat$Penetration) & is.na(as.numeric(dat$Penetration))})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Penetration_issues'] <- 1L
        dat[c, 'Penetration_txt']    <- 'Value should be numeric'
    }
    ## *** Check for negative values
    suppressWarnings({c <- !is.na(dat$Penetration) & as.numeric(dat$Penetration) < 0})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Penetration_issues'] <- 1L
        dat[c, 'Penetration_txt']    <- 'Value should be positive'
    }
    ## *** Check for excessive values
    suppressWarnings({c <- grepl('^[0-9.]+$', dat$Penetration) & as.numeric(dat$Penetration) > 2000})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Penetration_issues'] <- 1L
        dat[c, 'Penetration_txt']    <- 'Value too high'
    }
}
## ** Altitude
if ('Altitude' %in% names(dat)) {
    ## *** Check for missing values
    c <- is.na(dat$Altitude)
    if (any(c)) {
        dat[c, 'Altitude_issues'] <- 1L
        dat[c, 'Altitude_txt']    <- 'Value is missing'
    }
    ## *** Check for non-numeric values
    suppressWarnings({c <- !is.na(dat$Altitude) & is.na(as.numeric(dat$Altitude))})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Altitude_issues'] <- 1L
        dat[c, 'Altitude_txt']    <- 'Value should be numeric'
    }
    ## *** Check for negative values
    suppressWarnings({c <- !is.na(dat$Altitude) & as.numeric(dat$Altitude) < 0})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Altitude_issues'] <- 1L
        dat[c, 'Altitude_txt']    <- 'Value should be positive'
    }
    ## *** Check for excessive values
    suppressWarnings({c <- !is.na(dat$Altitude) & as.numeric(dat$Altitude) > 3600})
    if (!all(is.na(c)) && any(c)) {
        dat[c, 'Altitude_issues'] <- 1L
        dat[c, 'Altitude_txt']    <- 'Value too high'
    }
}
## ** SpeciesCode
if ('SpeciesCode' %in% names(dat)) {
    ## *** Check for missing values
    c <- is.na(dat$SpeciesCode)
    if (any(c)) {
        dat[c, 'SpeciesCode_issues'] <- 1L
        dat[c, 'SpeciesCode_txt']    <- 'Value is missing'
    }
    ## *** Check for existence
    c <- !(tolower(dat$SpeciesCode) %in% fish_names[['NZFFD code']])
    if (any(c)) {
        dat[c, 'SpeciesCode_issues'] <- 1L
        dat[c, 'SpeciesCode_txt']    <- 'Species code not recognised'
    }
    ## *** Check existence in species_ibi_metrics
    c <- !(tolower(dat$SpeciesCode) %in% species_ibi_metrics$spcode)
    if (any(c)) {
        dat[c, 'SpeciesCode_warnings'] <- 1L
        dat[c, 'SpeciesCode_wtxt']    <- 'Non-fish code or species without IBI metrics'
    }
}        
## ** SiteID
if ('SiteID' %in% names(dat)) {
    ## *** Check for missing values
    c <- is.na(dat$SiteID)
    if (any(c)) {
        dat[c, 'SiteID_issues'] <- 1L
        dat[c, 'SiteID_txt']    <- 'Value is missing'
    }
}
## ** Date
if ('Date' %in% names(dat)) {
    ## *** Check for missing values
    c <- is.na(dat$Date)
    if (any(c)) {
        dat[c, 'Date_issues'] <- 1L
        dat[c, 'Date_txt']    <- 'Value is missing'
    }
}

dat[, with_issue := rowSums(.SD, na.rm=T) > 0,
  .SDcols = patterns('_issues$')]
dat[, site_issue := any(with_issue %in% T), .(SiteID, Date)]
dat <- dat[site_issue == F, -c('with_issue', 'site_issue'), with = F]

dat <- dat[, cols_needed, with=F]

setorder(dat, SiteID, Date)
dat[, id := rleid(SiteID, Date)]

alttriggered <- F
warning <- F

qr.construct <- function(y, x, data = site_metrics_all){
    q <- NULL
    tryCatch({
        q <- rq(paste(y, x, sep = " ~ "), tau = c(1/3, 2/3), data = data, method = 'br')
    }, error = function(e) warning('Quantile regression failed with `br` method. Will try with `sfn`'))
    if (is.null(q)) {
        tryCatch({
            q <- rq(paste(y, x, sep = " ~ "), tau = c(1/3, 2/3), data = data, method = 'sfn')
            alttriggered <<- T
        }, error = function(e) {
            cat('=== Error:\n')
            print(y)
            print(x)
            print(data)
        })
    }
    q
}

## d <- fread('~/Downloads/WhitesBayIBI.csv')
dat[, Altitude := as.numeric(Altitude)]
dat[, Penetration := as.numeric(Penetration)]

d <- copy(dat)
ibi <- function(d) {

    alttriggered <<- F

    site_metrics_all <- d %>%
        prep.site.metrics(species.ibi.metrics = species_ibi_metrics)
    
    qr.1.elev <- qr.construct(y="metric1", x="Altitude", data = site_metrics_all)
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

    setDT(ibi_scores)
    ibi_scores[, alttriggered := alttriggered]
    
    ibi_scores <- ibi_scores[unique(d[, .(SiteID, Date, id)]), on = c('Date', 'SiteID')]
    ibi_scores
}


nsims <- 100

sampled_sites <- lapply(seq_len(nsims), function(n) {
    sample(dat$id, sample(1:20, 1))
})

pids <- progressbar(nsims, 100)
res <- lapply(seq_len(nsims), function(i) {
    if (i %in% pids) cat('*')
    d1 <- dat[id %in% sampled_sites[[i]]]
    r <- ibi(d1)
    r[, sim := i]
    r
})
cat('\n')

res <- rbindlist(res)

res[sim == 2]

r <- res[, .(.N, alt = any(alttriggered %in% T), n.na = sum(is.na(alttriggered))), .(sim)]
r[alt == T, .N, N]


dat[SiteID == 'Harhari Bch']

head(res)

nulls <- sapply(res, is.null)
ex <- sample(which(nulls == T), 1)
d1 <- dat[id %in% sampled_sites[[ex]]]

site_metrics_all <- d1 %>%
    prep.site.metrics(species.ibi.metrics = species_ibi_metrics)
    
qr.1.elev <- qr.construct(y="metric1", x="Altitude", data = site_metrics_all)
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

setDT(ibi_scores)


res[, nsims := .N, sim]

summ <- res[, .(nsims=.N, un=uniqueN(NPSscore)), .(id)]
summ <- res[alttriggered == F, .(nsims=.N, unique_IBS = uniqueN(IBIscore), range_IBS = diff(range(IBIscore)),
                                 unique_NPS=uniqueN(NPSscore)), .(id)]

summ[nsims > 1]
summ[, max(range_IBS)]
summ[which.max(range_IBS)]
res[id == 46]

res[id == 45]

res[, any(alttriggered %in% T), sim][, sum(V1 == T)]
