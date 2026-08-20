# linear model
library(tidyverse)
library(broom)

# Locate the analysis directory (the folder holding config.R) robustly, so this
# script works whether it is run via `Rscript`, RStudio's Source button, or by
# selecting-and-Running the file with any working directory. Same approach as
# run_pipeline.R -- do NOT rely on getwd() being the CH4_Drought folder.
locate_analysis_dir <- function() {
  cand <- character(0)
  a <- commandArgs(FALSE)
  f <- a[grepl("^--file=", a)]
  if (length(f)) cand <- c(cand, dirname(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE)))
  for (i in seq_len(sys.nframe())) {                 # sourced file: frame$ofile
    of <- sys.frame(i)$ofile
    if (!is.null(of)) cand <- c(cand, dirname(normalizePath(of, mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) cand <- c(cand, dirname(normalizePath(p, mustWork = FALSE)))
  }
  cand <- c(cand, getwd(), file.path(getwd(), "CH4_Drought"),
            "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) return(hit[1])
  stop("Could not find config.R. Set the working directory to the CH4_Drought folder.")
}
analysis_dir <- locate_analysis_dir()
message("analysis_dir: ", analysis_dir)
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "temperature_index.R"))

# Write the intermediate linear-model RDATA to the local data/ folder (absolute
# path), so this script does not depend on the lab server volume being mounted or
# on the current working directory. The canonical inputs are already loaded by
# absolute path below.
local_data_dir <- file.path(analysis_dir, "data")
dir.create(local_data_dir, recursive = TRUE, showWarnings = FALSE)
lineamodel_file <- file.path(local_data_dir, "FinalDrought_Data_LineaModel.RDATA")

# Canonical analysis table: the same fluxes.drought_normalized used by 06-15
# (from data/DroughtAnalysis.RDATA, written by 06_BuildAnalysisTable.R). All linear
# models below use the site-normalized anomaly normalized_Fch4 as the response, so
# slopes/offsets and Fig 3 are in methane-anomaly units; site-normalization removes
# a per-site constant and does not change the SPEI slopes.
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
      
      try(lm.spei <- lm(as.formula(paste("normalized_Fch4 ~", DROUGHT_INDEX)), data = subset) %>% summary, silent=T)
   
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

save(drought.linear.results,  file = lineamodel_file)

load(file = lineamodel_file)

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
    m <- lm(as.formula(paste("normalized_Fch4 ~", DROUGHT_INDEX)), data = .x)
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
  as.formula(paste0("normalized_Fch4 ~ ", DROUGHT_INDEX, " * thermal_season + SITE_ID")),
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
     file = lineamodel_file)

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
    m <- lm(as.formula(paste("normalized_Fch4 ~", DROUGHT_INDEX)), data = .x)
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

# Linear STI model: response is the site-normalized anomaly normalized_Fch4 (not
# the raw flux), so the fitted lines and Fig 3 are in methane-anomaly units. The
# SITE_ID blocks are retained; the SPEI slope is unchanged by normalization, only
# the intercept/level shifts to the anomaly scale.
spei_x_temp.model <- lm(
  as.formula(paste0("normalized_Fch4 ~ ", DROUGHT_INDEX,
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
     file = lineamodel_file)

cat("Wrote temp-anomaly-stratified linear results to:", anom_dir, "\n")
