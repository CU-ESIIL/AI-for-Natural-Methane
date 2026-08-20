# linear model
library(tidyverse)
library(broom)

config_file <- file.path(getwd(), "CH4_Drought", "config.R")
if (!file.exists(config_file)) config_file <- "config.R"
source(config_file)
analysis_dir <- dirname(normalizePath(config_file))
source(file.path(analysis_dir, "temperature_index.R"))

project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
setwd( project.data.dir)

# Canonical analysis table: the same fluxes.drought_normalized used by 06-15
# (from data/DroughtAnalysis.RDATA, written by 06_BuildAnalysisTable.R). The site-
# blocked regression below still uses the raw gap-filled flux FCH4_F_ANNOPTLM.
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
if ("geometry" %in% names(fluxes.drought_normalized)) fluxes.drought_normalized$geometry <- NULL

fluxes.drought <- fluxes.drought_normalized %>% filter( !is.na(FCH4_F_ANNOPTLM)) # the dataframe to use:

# Add winter/summer (per-site thermal-season) axis if not already present.
if (!"thermal_season" %in% names(fluxes.drought)) {
  fluxes.drought <- add_thermal_season(fluxes.drought)
}
# Add hot/normal/cold temperature-anomaly axis (STI, site-month standardized).
if (!"temp_class" %in% names(fluxes.drought)) {
  fluxes.drought <- add_temp_anomaly_class(fluxes.drought)
}

drought.linear.results <- data.frame()

for( site in fluxes.drought$SITE_ID %>% unique){

  print(site)
  subset <- fluxes.drought %>% filter( SITE_ID == site)
      
      try(lm.spei <- lm(as.formula(paste("FCH4_F_ANNOPTLM ~", DROUGHT_INDEX)), data = subset) %>% summary, silent=T)
   
      try(lm.spei.results <- data.frame(    Intercept =lm.spei$coefficients[1] %>% round(3), # intercept
                                            Intercept.pvalue = lm.spei$coefficients[7] %>% round(3), # intercept - Pvalue
                                            slope = lm.spei$coefficients[2] %>% round(3), # slope
                                            slope.pvalue=lm.spei$coefficients[8] %>% round(3), # slope- pvalue
                                            R2 =lm.spei$r.squared %>% round(3),
                                            mean.spei = subset[[DROUGHT_INDEX]] %>% mean(na.rm=T) %>% round(3),
                                            min.spei = subset[[DROUGHT_INDEX]] %>% min(na.rm=T) %>% round(3),
                                            max.spei = subset[[DROUGHT_INDEX]] %>% max(na.rm=T) %>% round(3),
                                            var.spei = subset[[DROUGHT_INDEX]] %>% var(na.rm=T) %>% round(3),
                                        SITE_ID = site), silent = T)
      
    try( drought.linear.results <- rbind(drought.linear.results,lm.spei.results))
      try(rm( lm.spei), silent =T)
}

save(drought.linear.results,  file='FinalDrought_Data_LineaModel.RDATA')

load(file='FinalDrought_Data_LineaModel.RDATA')

drought.linear.results %>% names
drought.linear.results <-drought.linear.results %>% mutate(F.0 = Intercept + slope*0,
                                                           F.m3 = Intercept + slope*-3)

drought.linear.results %>% ggplot() +geom_segment(
             aes(x = F.0, y = SITE_ID,
                 yend = SITE_ID, xend = F.m3), #use the $ operator to fetch data from our "Females" tibble
             color = "#aeb6bf",
             size = 4.5, #Note that I sized the segment to fit the points
             alpha = .5) +
  geom_point(aes(x = F.0, y = SITE_ID), size = 4, show.legend = TRUE)+
  geom_point(aes(x = F.m3, y = SITE_ID),col="brown", size = 2, show.legend = TRUE) + xlim(-100, 100)


# ---------------------------------------------------------------------------
# Thermal-season stratification of the moisture (SPEI) response.
# Does the SPEI -> methane relationship (spanning extreme-dry to extreme-wet)
# differ between winter (cold-season) and summer (warm-season) events?
# ---------------------------------------------------------------------------

# Per site x season slope of FCH4 ~ SPEI.
drought.linear.byseason <- fluxes.drought %>%
  filter(!is.na(.data[[DROUGHT_INDEX]]), !is.na(FCH4_F_ANNOPTLM), !is.na(thermal_season)) %>%
  group_by(SITE_ID, thermal_season) %>%
  filter(dplyr::n() >= 5, stats::var(.data[[DROUGHT_INDEX]], na.rm = TRUE) > 0) %>%
  group_modify(~ {
    m <- lm(as.formula(paste("FCH4_F_ANNOPTLM ~", DROUGHT_INDEX)), data = .x)
    s <- summary(m)
    tibble(
      n_obs        = nrow(.x),
      intercept    = s$coefficients[1, 1],
      slope        = s$coefficients[2, 1],
      slope.pvalue = s$coefficients[2, 4],
      R2           = s$r.squared,
      mean.spei    = mean(.x[[DROUGHT_INDEX]], na.rm = TRUE)
    )
  }) %>%
  ungroup()

# Pooled interaction: let the SPEI slope differ by season, blocking on site.
spei_x_season.model <- lm(
  as.formula(paste0("FCH4_F_ANNOPTLM ~ ", DROUGHT_INDEX, " * thermal_season + SITE_ID")),
  data = fluxes.drought
)
spei_x_season.terms <- broom::tidy(spei_x_season.model) %>%
  filter(!grepl("^SITE_ID", term))

out_dir <- file.path(analysis_dir, "outputs", "temperature_events")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(drought.linear.byseason, file.path(out_dir, "linear_spei_by_season.csv"))
readr::write_csv(spei_x_season.terms,     file.path(out_dir, "linear_spei_x_season_terms.csv"))

save(drought.linear.results, drought.linear.byseason,
     spei_x_season.model, spei_x_season.terms,
     file = 'FinalDrought_Data_LineaModel.RDATA')

cat("Wrote season-stratified linear results to:", out_dir, "\n")


# ---------------------------------------------------------------------------
# Temperature-anomaly stratification (hot / normal / cold; normal reference).
# Does the SPEI -> methane relationship differ between hot, normal, and cold
# events (hotter/colder than normal for the site and month)?
# ---------------------------------------------------------------------------

linear.by_temp_anomaly <- fluxes.drought %>%
  filter(!is.na(.data[[DROUGHT_INDEX]]), !is.na(FCH4_F_ANNOPTLM), !is.na(temp_class)) %>%
  group_by(SITE_ID, temp_class) %>%
  filter(dplyr::n() >= 5, stats::var(.data[[DROUGHT_INDEX]], na.rm = TRUE) > 0) %>%
  group_modify(~ {
    m <- lm(as.formula(paste("FCH4_F_ANNOPTLM ~", DROUGHT_INDEX)), data = .x)
    s <- summary(m)
    tibble(
      n_obs        = nrow(.x),
      intercept    = s$coefficients[1, 1],
      slope        = s$coefficients[2, 1],
      slope.pvalue = s$coefficients[2, 4],
      R2           = s$r.squared,
      mean.spei    = mean(.x[[DROUGHT_INDEX]], na.rm = TRUE)
    )
  }) %>%
  ungroup()

spei_x_temp.model <- lm(
  as.formula(paste0("FCH4_F_ANNOPTLM ~ ", DROUGHT_INDEX,
                    " * relevel(factor(temp_class), ref = 'normal') + SITE_ID")),
  data = fluxes.drought %>% filter(!is.na(temp_class))
)
spei_x_temp.terms <- broom::tidy(spei_x_temp.model) %>%
  filter(!grepl("^SITE_ID", term))

anom_dir <- file.path(analysis_dir, "outputs", "temperature_anomaly_events")
dir.create(anom_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(linear.by_temp_anomaly, file.path(anom_dir, "linear_spei_by_temp_anomaly.csv"))
readr::write_csv(spei_x_temp.terms,      file.path(anom_dir, "linear_spei_x_temp_anomaly_terms.csv"))

save(drought.linear.results, drought.linear.byseason, spei_x_season.model, spei_x_season.terms,
     linear.by_temp_anomaly, spei_x_temp.model, spei_x_temp.terms,
     file = 'FinalDrought_Data_LineaModel.RDATA')

cat("Wrote temp-anomaly-stratified linear results to:", anom_dir, "\n")
