# 17_ScenarioFrequencies_CMIP6.R   (runs before 18_ExtremeEmissionsProjection.R)
# ---------------------------------------------------------------------------
# Build the future frequency of hot / cold / dry / wet events for each SSP,
# consumed by 18_ExtremeEmissionsProjection.R. Two modes:
#
#  MODE 1 (default) "warming-shift": derive frequencies from each SSP's global
#     warming pathway (data/ssp_global_warming_levels.csv, from CMIP6/AR6) by
#     shifting the observed standardized temperature (STI) and moisture (SPEI)
#     distributions. Local warming = LAND_AMPLIFICATION x global warming; the
#     temperature anomaly class boundary is fixed at +/-1 SD (STI), so
#         p_hot(t)  = p_hot0  * P(Z >= 1 - mu(t)) / P(Z >= 1)
#         p_cold(t) = p_cold0 * P(Z <= -1 - mu(t)) / P(Z <= -1),
#     with mu(t) = LAND_AMPLIFICATION * (GWL(t) - GWL_BASELINE) / sigma_local.
#     Moisture shifts use MOISTURE_DRY_SENS / MOISTURE_WET_SENS per deg C.
#     This is fully reproducible and needs no raw CMIP6 fields.
#
#  MODE 2 "cmip6-direct": if per-site CMIP6 tas/pr extractions are present in
#     CMIP6_SITE_DIR, classify each future month directly and tally frequencies
#     (stub provided — plug in your extraction reader).
#
# Output: data/projection_frequency_scenarios.csv (+ server), columns
#         ssp, year, p_cold, p_normal_temp, p_hot, p_dry, p_normal_moist, p_wet.
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))

# ---- observed baselines from the flux data (sigma_local + class frequencies) ----
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
d <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
d$condition <- ifelse(d[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
               ifelse(d[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal"))
d <- d[!is.na(d$temp_class) & !is.na(d$condition), ]

sigma_local <- mean(tapply(d$TA_F, paste(d$SITE_ID, d$month), stats::sd, na.rm = TRUE), na.rm = TRUE)
p_hot0  <- mean(d$temp_class == "hot")
p_cold0 <- mean(d$temp_class == "cold")
p_dry0  <- mean(d$condition == "drought")
p_wet0  <- mean(d$condition == "extreme_wet")
message(sprintf("Baseline: sigma_local=%.2f C | p_hot=%.3f p_cold=%.3f p_dry=%.3f p_wet=%.3f",
                sigma_local, p_hot0, p_cold0, p_dry0, p_wet0))

# ---- MODE 1: warming-shift frequencies ----
build_warming_shift <- function() {
  gwl <- read.csv(file.path(analysis_dir, SSP_GWL_TABLE), stringsAsFactors = FALSE)
  ratio_hi <- function(mu) (1 - pnorm(1 - mu)) / (1 - pnorm(1))   # upper-tail scaling
  ratio_lo <- function(mu) pnorm(-1 - mu) / pnorm(-1)             # lower-tail scaling
  out <- do.call(rbind, lapply(unique(gwl$ssp), function(ssp) {
    s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
    dG <- s$gwl - GWL_BASELINE
    mu <- LAND_AMPLIFICATION * dG / sigma_local
    p_hot  <- pmin(p_hot0  * ratio_hi(mu), 0.9)
    p_cold <- pmin(p_cold0 * ratio_lo(mu), 0.9)
    # moisture: drying raises dry tail, intensification raises wet tail
    p_dry  <- pmin(p_dry0 * (pnorm(-1 + MOISTURE_DRY_SENS * dG) / pnorm(-1)), 0.9)
    p_wet  <- pmin(p_wet0 * ((1 - pnorm(1 - MOISTURE_WET_SENS * dG)) / (1 - pnorm(1))), 0.9)
    data.frame(ssp = ssp, year = s$year,
               p_cold = round(p_cold, 4), p_normal_temp = round(1 - p_cold - p_hot, 4), p_hot = round(p_hot, 4),
               p_dry = round(p_dry, 4), p_normal_moist = round(1 - p_dry - p_wet, 4), p_wet = round(p_wet, 4),
               stringsAsFactors = FALSE)
  }))
  out
}

# ---- MODE 2: direct CMIP6 tally (stub) ----
build_cmip6_direct <- function() {
  # Expected: per-site CMIP6 tas & pr time series in CMIP6_SITE_DIR, one file per
  # (ssp, model). For each future month compute STI (vs the site-month climatology
  # baseline used here) and an SPEI-equivalent, classify into hot/cold and
  # dry/wet, then tally decadal frequencies per SSP (averaged across models).
  # Return the same columns as build_warming_shift(). Left as a stub until the
  # extractions are staged; the pipeline falls back to MODE 1.
  stop("cmip6-direct mode not yet wired: stage per-site CMIP6 tas/pr in ", CMIP6_SITE_DIR)
}

mode <- if (dir.exists(CMIP6_SITE_DIR) &&
            length(list.files(CMIP6_SITE_DIR, pattern = "\\.nc$|\\.csv$")) > 0) "cmip6-direct" else "warming-shift"
message("Frequency mode: ", mode)
freq <- tryCatch(if (mode == "cmip6-direct") build_cmip6_direct() else build_warming_shift(),
                 error = function(e) { message(conditionMessage(e), " -- using warming-shift."); build_warming_shift() })

write.csv(freq, file.path(analysis_dir, "data", "projection_frequency_scenarios.csv"), row.names = FALSE)
# provenance copy in outputs (+ server)
save_output_csv(freq, "extreme_emissions_projection/projection_frequency_scenarios.csv", analysis_dir)
message("Wrote scenario frequencies for ", length(unique(freq$ssp)), " SSPs.")
