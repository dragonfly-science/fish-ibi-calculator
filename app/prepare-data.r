library(data.table)
library(sf)


## * REGIONS - Data from https://datafinder.stats.govt.nz/layer/106666-regional-council-2022-generalised/

reg <- read_sf('data/regional-council-2022-generalised.gpkg')
reg <- subset(reg, REGC2022_V1_00_NAME != 'Area Outside Region')
reg$region <- sub(' Region', '', reg$REGC2022_V1_00_NAME)
reg <- reg[, c('region', 'geom')]
reg <- st_transform(reg, crs=27200)
## plot(st_geometry(reg))
regions <- reg


## * Calibration values

## ** From Mike Joy's paper
cal <- fread('data/joy-calibration.csv')
setnames(cal, c('fishtype', 'scale', 'xint', 'yint'))
cal[, metric := c('Native' = 'metric1',
                  'Benthicriffle' = 'metric2',
                  'Benthicpool' = 'metric3',
                  'Pelagicpool' = 'metric4',
                  'Intolerant' = 'metric5')[fishtype]]
cal[, type := c('E' = 'altitude', 'D' = 'penetration')[scale]]
setkey(cal, metric, type)
calibrations <- cal


## * IBI category thresholds

ibi_thresh <- fread('data/regional_ibi_thresholds.csv')
ibi_thresh <- rbind(ibi_thresh, data.table(region = 'No Region', q1 = 18, median = 28, q3 = 34))
setkey(ibi_thresh, region)

## * Species code lookup

species_codes <- fread('data/fish_code_lookup.csv')
species_codes[, spp_code_2004 := tolower(spp_code_2004)]


## * Species IBI metrics

species_ibi_metrics <- fread('data/species_ibi_metrics.csv')

not.in.lookup <- species_ibi_metrics[!species_codes, on = c('spcode' = 'nzffd_spp_code')]
if (nrow(not.in.lookup)) {
  warning('Some species code in IBI metrics not in species lookup:')
  print(not.in.lookup)
}

## * Fish names

fish_names <- fread('data/fish_names.csv')

not.in.lookup <- fish_names[`NZFFD code` != 'nospec'][!species_codes, on = c('NZFFD code' = 'nzffd_spp_code')]
if (nrow(not.in.lookup)) {
  warning('Some species code in fish names not in species lookup:')
  print(not.in.lookup)
}

save(regions, calibrations, ibi_thresh, species_codes, species_ibi_metrics, fish_names,
     file = 'data/app-data.rdata')


