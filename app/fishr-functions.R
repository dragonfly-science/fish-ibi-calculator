#' Add on fish names
#'
#' This function adds full fish names from six digit species codes
#'
#' @export
add.fish.names <- function(.data){
  .data %>%
    left_join(fish_names, by = c("SpeciesCode" = "NZFFD code"))
}


#' Add fishing methods
#'
#' This function adds the full names for fishing methods from abbreviation
#'
#' @export
add.fish.method <- function(.data){
  .data %>%
    left_join(fish_methods, by = c("fishmeth" = "Abbreviation"))
}

#' Add better formatted dates
#'
#' This function takes dates as formatted by the NZFFD and converts them into actual date columns
#'
#' @export
add.fish.dates <- function(.data){
  .data %>%
    mutate(date = as.Date(paste(y, m, "01", sep="-"), "%Y-%m-%d")) %>%
    mutate(m = month(date))
}

#' Prepare site metrics
#'
#' This function prepares site-level summaries for later metric calculations
#'
#' @export
prep.site.metrics <- function(.data, species.ibi.metrics = species_ibi_metrics){
  .data %>%
    unite(Stratum, c(Date, SiteID), remove=FALSE) %>% 
    group_by(Stratum) %>%
    distinct(SpeciesCode, .keep_all = TRUE) %>%
    inner_join(species.ibi.metrics, by = c("SpeciesCode" = "spcode")) %>%  # filtering to only species with metric info. change to left_join to get all.
    summarise(
      Date = first(Date),
      SiteID = first(SiteID),
      Altitude = first(Altitude),
      Penet = first(Penetration),
      # east = first(east),
      #  north = first(north),
      total_sp_richness = n(),
      metric1 = sum(native, na.rm = T),
      metric2 = sum(benthic_riffle, na.rm = T),
      metric3 = sum(benthic_pool, na.rm = T),
      metric4 = sum(pelagic_pool, na.rm = T),
      metric5 = sum(intolerant, na.rm = T),
      number_non_native = sum(non_native, na.rm = T),
      metric6 = metric1/(number_non_native + metric1)
    )
}

#' Fit quantile regression
#'
#' This function fits a quantile regression for scoring of metrics. This improves
#' upon methods which require fitting by eye.
#'
#' @export
#'
#'why is site_metrics_all being specified here, look at the intro vignette re this
qr.construct <- function(y, x, data = site_metrics_all){
  rq(paste(y, x, sep = " ~ "), tau = c(1/3, 2/3), data = data)
}

#' Score metrics according to quantile regression
#'
#' Once you've fit a quantile regression, you then need to score it
#'
#' @export
qr.score <- function(x, y, qr){

  line_66 <- y > coef(qr)[4] * x + coef(qr)[3]
  line_33 <- y > coef(qr)[2] * x + coef(qr)[1]

  case_when(
    line_66 == TRUE ~ 5,
    line_33 == TRUE ~ 3,
    line_66 == FALSE & line_33 == FALSE ~ 1
  )
}

#' Add metric scores
#'
#' Add each metrics scores onto df
#'
#' @export
add.fish.metrics <- function(.data, q1e = qr.1.elev, q2e = qr.2.elev, q3e = qr.3.elev,
                             q4e = qr.4.elev, q5e = qr.5.elev, q1p = qr.1.penet,
                             q2p = qr.2.penet, q3p = qr.3.penet, q4p = qr.4.penet,
                             q5p = qr.5.penet){
  .data %>%
    mutate(metric1_rating_elev = pmap_dbl(list(x = Altitude, y = metric1), qr.score, qr = q1e),
           metric2_rating_elev = pmap_dbl(list(x = Altitude, y = metric2), qr.score, qr = q2e),
           metric3_rating_elev = pmap_dbl(list(x = Altitude, y = metric3), qr.score, qr = q3e),
           metric4_rating_elev = pmap_dbl(list(x = Altitude, y = metric4), qr.score, qr = q4e),
           metric5_rating_elev = pmap_dbl(list(x = Altitude, y = metric5), qr.score, qr = q5e)) %>%
    mutate(metric1_rating_pene = pmap_dbl(list(x = Penet, y = metric1), qr.score, qr = q1p),
           metric2_rating_pene = pmap_dbl(list(x = Penet, y = metric2), qr.score, qr = q2p),
           metric3_rating_pene = pmap_dbl(list(x = Penet, y = metric3), qr.score, qr = q3p),
           metric4_rating_pene = pmap_dbl(list(x = Penet, y = metric4), qr.score, qr = q4p),
           metric5_rating_pene = pmap_dbl(list(x = Penet, y = metric5), qr.score, qr = q5p))

}

#' Add metric six score
#'
#' Metric six is a bit different, so get scored separately
#'
#' @export
add.fish.metric6 <- function(.data){
  .data %>%
    mutate(metric6_rating = case_when(
      # these values are coming from table 3 on page 421
      metric6 > 0.67 ~ 5,
      metric6 >= 0.33 ~ 3,
      metric6 < 0.33 ~ 1
    ))
}

#' Add combined IBI score
#'
#' Takes the individual metric scores and combines them to form the final IBI score
#' (continuous)
#'
#' @export
add.fish.ibi <- function(.data){
  .data %>%
    mutate(IBIscore =
             metric1_rating_elev +
             metric2_rating_elev +
             metric3_rating_elev +
             metric4_rating_elev +
             metric5_rating_elev +
             metric1_rating_pene +
             metric2_rating_pene +
             metric3_rating_pene +
             metric4_rating_pene +
             metric5_rating_pene +
             metric6_rating*2)
}

#' Cut IBI score
#'
#' Takes the continuous IBI overall score and cuts it into the three categories
#' originally proposed by Joy and Death (2004)
#'
#' @export
cut.fish.ibi <- function(.data){
  .data %>%
    mutate(IBIscoreCut = cut(IBIscore, breaks = c(0, 20, 40, 60), labels = c("Low quality",
                                                                                "Medium quality",
                                                                                "High quality")))
}

nps <- function(.data){
  .data %>% 
    mutate(NPSscore = case_when(
      IBIscore >= 34 ~ "A",
      IBIscore >= 28 ~ "B",
      IBIscore >= 18 ~ "C",
      IBIscore < 18 ~ "D"
    ))
}
