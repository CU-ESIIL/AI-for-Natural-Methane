# 21_SensitivityAndDiagnostics.R
# ---------------------------------------------------------------------------
# Diagnostics and robustness checks for the projection workflow:
#   - global site-scaled vs region-weighted projection comparison
#   - weak TEM-MDM/FLUXNET agreement flags
#   - sparse response-bin flags
#   - regional uncertainty flags
#   - deterministic assumption sensitivity for the region-weighted projection
#   - leave-one-site-out influence on the region-weighted projection
#
# Outputs -> outputs/diagnostics/ (+ server).
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))

out_rel <- function(f) file.path("diagnostics", f)
MO <- c("drought", "normal", "extreme_wet")
TE <- c("cold", "normal", "hot")

load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
base_data <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
base_data$condition <- ifelse(base_data[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
                       ifelse(base_data[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal"))
base_data <- base_data[!is.na(base_data$temp_class) &
                         !is.na(base_data$condition) &
                         !is.na(base_data$normalized_Fch4), ]

if (!"LAT" %in% names(base_data)) {
  cand <- c(file.path(SERVER_DIR, "data", "SiteLatLon.csv"),
            file.path(analysis_dir, "data", "SiteLatLon.csv"))
  cf <- cand[file.exists(cand)][1]
  if (is.na(cf)) stop("No site latitude available; provide data/SiteLatLon.csv (site,lat,lon).")
  ll <- read.csv(cf); names(ll) <- tolower(names(ll))
  ll <- ll[, c(grep("site", names(ll))[1], grep("lat", names(ll))[1])]
  names(ll) <- c("SITE_ID", "LAT")
  base_data <- merge(base_data, ll, by = "SITE_ID", all.x = TRUE)
}
base_data <- base_data[!is.na(base_data$LAT), ]

default_reg_cfg <- read.csv(file.path(analysis_dir, REGION_CONFIG), stringsAsFactors = FALSE)
gwl <- read.csv(file.path(analysis_dir, SSP_GWL_TABLE), stringsAsFactors = FALSE)
years <- seq(PROJECTION_BASE_YEAR, PROJECTION_END_YEAR)

assign_region <- function(lat, reg_cfg) {
  a <- abs(lat)
  reg_cfg$region[vapply(a, function(x) which(x > reg_cfg$abs_lat_min - 1e-9 &
                                                x <= reg_cfg$abs_lat_max + 1e-9)[1], integer(1))]
}

ipf <- function(seed, rt, ct, it = 60) {
  J <- seed / sum(seed)
  for (i in seq_len(it)) {
    J <- J * (rt / rowSums(J))
    J <- J * matrix(ct / colSums(J), nrow(J), ncol(J), byrow = TRUE)
  }
  J
}

region_stats <- function(d, reg_cfg) {
  d$region <- assign_region(d$LAT, reg_cfg)
  gcell <- tapply(d$normalized_Fch4, list(d$condition, d$temp_class), mean, na.rm = TRUE)
  lapply(setNames(reg_cfg$region, reg_cfg$region), function(r) {
    sub <- d[d$region == r, ]
    R <- matrix(NA_real_, 3, 3, dimnames = list(MO, TE))
    seed <- matrix(0, 3, 3, dimnames = list(MO, TE))
    for (mi in MO) for (tj in TE) {
      cc <- sub[sub$condition == mi & sub$temp_class == tj, ]
      seed[mi, tj] <- nrow(cc)
      sm <- tapply(cc$normalized_Fch4, cc$SITE_ID, mean, na.rm = TRUE)
      sm <- sm[is.finite(sm)]
      if (length(sm) >= 1) R[mi, tj] <- mean(sm)
    }
    na <- is.na(R)
    R[na] <- gcell[cbind(row(R)[na], col(R)[na])]
    seed[seed == 0] <- 1
    list(R = R, seed = seed, F0 = mean(sub$FCH4_F_ANNOPTLM, na.rm = TRUE),
         sigma = mean(tapply(sub$TA_F, paste(sub$SITE_ID, sub$month), stats::sd, na.rm = TRUE), na.rm = TRUE),
         p_hot0 = mean(sub$temp_class == "hot"), p_cold0 = mean(sub$temp_class == "cold"),
         p_dry0 = mean(sub$condition == "drought"), p_wet0 = mean(sub$condition == "extreme_wet"),
         n_sites = length(unique(sub$SITE_ID)))
  })
}

marg <- function(rs, LA, dG, dry_sens, wet_sens) {
  mu <- LA * dG / rs$sigma
  c(p_dry = min(rs$p_dry0 * pnorm(-1 + dry_sens * dG) / pnorm(-1), 0.9),
    p_wet = min(rs$p_wet0 * (1 - pnorm(1 - wet_sens * dG)) / (1 - pnorm(1)), 0.9),
    p_cold = min(rs$p_cold0 * pnorm(-1 - mu) / pnorm(-1), 0.9),
    p_hot  = min(rs$p_hot0  * (1 - pnorm(1 - mu)) / (1 - pnorm(1)), 0.9))
}

project_region_deterministic <- function(d, reg_cfg = default_reg_cfg,
                                         global_budget = GLOBAL_WETLAND_BUDGET_TG,
                                         dry_sens = MOISTURE_DRY_SENS,
                                         wet_sens = MOISTURE_WET_SENS) {
  RS <- region_stats(d, reg_cfg)
  rows <- list()
  for (ssp in unique(gwl$ssp)) {
    s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
    g <- approx(s$year, s$gwl, years, rule = 2)$y
    total <- rep(0, length(years))
    for (ri in seq_len(nrow(reg_cfg))) {
      r <- reg_cfg$region[ri]
      rs <- RS[[r]]
      LA <- reg_cfg$land_amplification[ri]
      E <- vapply(g, function(gi) {
        p <- marg(rs, LA, gi - GWL_BASELINE, dry_sens, wet_sens)
        J <- ipf(rs$seed,
                 c(p["p_dry"], 1 - p["p_dry"] - p["p_wet"], p["p_wet"]),
                 c(p["p_cold"], 1 - p["p_cold"] - p["p_hot"], p["p_hot"]))
        sum(J * rs$R)
      }, numeric(1))
      budget_r <- reg_cfg$budget_share[ri] * global_budget
      total <- total + ((E - E[1]) / rs$F0) * budget_r
    }
    rows[[ssp]] <- data.frame(ssp = ssp, year = years,
                              additional_Tg_per_yr = total,
                              cumulative_Tg = cumsum(total))
  }
  do.call(rbind, rows)
}

adjust_tropical_share <- function(reg_cfg, tropical_share) {
  out <- reg_cfg
  old_trop <- out$budget_share[out$region == "Tropical"]
  others <- out$region != "Tropical"
  out$budget_share[out$region == "Tropical"] <- tropical_share
  out$budget_share[others] <- out$budget_share[others] * ((1 - tropical_share) / (1 - old_trop))
  out
}

# ---- 1. global vs regional headline comparison -------------------------------
global_summary <- read.csv(file.path(analysis_dir, "outputs", "extreme_emissions_projection",
                                     "extreme_emissions_summary_2100.csv"), stringsAsFactors = FALSE)
regional <- read.csv(file.path(analysis_dir, "outputs", "regional_projection",
                               "regional_global_projection.csv"), stringsAsFactors = FALSE)
regional_2100 <- regional[regional$year == PROJECTION_END_YEAR, ]
projection_compare <- merge(
  global_summary[, c("ssp", "additional_Tg_per_yr_2100", "cumulative_Tg_2020_2100")],
  regional_2100[, c("ssp", "additional_Tg_per_yr_median", "lo", "hi", "cumulative_Tg_median")],
  by = "ssp", suffixes = c("_site_scaled", "_regional")
)
names(projection_compare) <- sub("additional_Tg_per_yr_2100", "site_scaled_Tg_per_yr_2100", names(projection_compare))
names(projection_compare) <- sub("additional_Tg_per_yr_median", "regional_Tg_per_yr_2100", names(projection_compare))
projection_compare$regional_minus_site_scaled <- projection_compare$regional_Tg_per_yr_2100 -
  projection_compare$site_scaled_Tg_per_yr_2100
projection_compare$preferred_headline <- TRUE
save_output_csv(projection_compare, out_rel("projection_global_vs_regional_2100.csv"), analysis_dir)

# ---- 2. model agreement and sparse bins ---------------------------------------
agreement <- read.csv(file.path(analysis_dir, "outputs", "fluxnet_model_comparison",
                                "compound_grid_model_agreement.csv"), stringsAsFactors = FALSE)
agreement$weak_sign_agreement <- agreement$sign_agreement_pct < 50
agreement$opposite_across_cell_pattern <- agreement$pearson_r_across_cells < 0
agreement$interpretation <- ifelse(agreement$weak_sign_agreement | agreement$opposite_across_cell_pattern,
                                   "Do not use this model comparison as validation support without explanation.",
                                   "Model agrees with the observed compound-grid pattern.")
save_output_csv(agreement, out_rel("model_agreement_flags.csv"), analysis_dir)

bins <- read.csv(file.path(analysis_dir, "outputs", "drought_response_curves",
                           "binned_site_mean_response_curves.csv"), stringsAsFactors = FALSE)
bins$sparse_bin <- bins$n_sites < 5 | is.na(bins$se)
sparse_bins <- bins[bins$sparse_bin, ]
save_output_csv(sparse_bins, out_rel("sparse_response_bins.csv"), analysis_dir)

# ---- 3. regional uncertainty flags -------------------------------------------
regional_bd <- read.csv(file.path(analysis_dir, "outputs", "regional_projection",
                                  "regional_breakdown_2100.csv"), stringsAsFactors = FALSE)
regional_bd$interval_width <- regional_bd$hi - regional_bd$lo
regional_bd$crosses_zero <- regional_bd$lo < 0 & regional_bd$hi > 0
regional_bd$few_sites <- regional_bd$n_sites < 10
regional_bd$priority_limitation <- regional_bd$few_sites | regional_bd$crosses_zero
save_output_csv(regional_bd, out_rel("regional_uncertainty_flags_2100.csv"), analysis_dir)

# ---- 4. deterministic sensitivity --------------------------------------------
variant_defs <- list(
  baseline = list(reg_cfg = default_reg_cfg, budget = GLOBAL_WETLAND_BUDGET_TG,
                  dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS),
  budget_100Tg = list(reg_cfg = default_reg_cfg, budget = 100,
                      dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS),
  budget_200Tg = list(reg_cfg = default_reg_cfg, budget = 200,
                      dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS),
  tropical_share_050 = list(reg_cfg = adjust_tropical_share(default_reg_cfg, 0.50),
                            budget = GLOBAL_WETLAND_BUDGET_TG, dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS),
  tropical_share_070 = list(reg_cfg = adjust_tropical_share(default_reg_cfg, 0.70),
                            budget = GLOBAL_WETLAND_BUDGET_TG, dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS),
  dry_sens_low = list(reg_cfg = default_reg_cfg, budget = GLOBAL_WETLAND_BUDGET_TG,
                      dry = MOISTURE_DRY_SENS * 0.5, wet = MOISTURE_WET_SENS),
  dry_sens_high = list(reg_cfg = default_reg_cfg, budget = GLOBAL_WETLAND_BUDGET_TG,
                       dry = MOISTURE_DRY_SENS * 1.5, wet = MOISTURE_WET_SENS),
  wet_sens_low = list(reg_cfg = default_reg_cfg, budget = GLOBAL_WETLAND_BUDGET_TG,
                      dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS * 0.5),
  wet_sens_high = list(reg_cfg = default_reg_cfg, budget = GLOBAL_WETLAND_BUDGET_TG,
                       dry = MOISTURE_DRY_SENS, wet = MOISTURE_WET_SENS * 1.5)
)

sens_rows <- list()
for (variant in names(variant_defs)) {
  v <- variant_defs[[variant]]
  pr <- project_region_deterministic(base_data, v$reg_cfg, v$budget, v$dry, v$wet)
  pr <- pr[pr$year == PROJECTION_END_YEAR, ]
  pr$variant <- variant
  pr$global_budget_Tg <- v$budget
  pr$tropical_budget_share <- v$reg_cfg$budget_share[v$reg_cfg$region == "Tropical"]
  pr$moisture_dry_sens <- v$dry
  pr$moisture_wet_sens <- v$wet
  sens_rows[[variant]] <- pr
}
sensitivity <- do.call(rbind, sens_rows)
sensitivity <- sensitivity[, c("variant", "ssp", "year", "additional_Tg_per_yr", "cumulative_Tg",
                               "global_budget_Tg", "tropical_budget_share",
                               "moisture_dry_sens", "moisture_wet_sens")]
save_output_csv(sensitivity, out_rel("projection_assumption_sensitivity_2100.csv"), analysis_dir)

# ---- 5. leave-one-site-out deterministic influence ----------------------------
site_ids <- sort(unique(base_data$SITE_ID))
baseline_det <- sensitivity[sensitivity$variant == "baseline", c("ssp", "additional_Tg_per_yr")]
names(baseline_det)[2] <- "baseline_additional_Tg_per_yr"
loo_rows <- list()
for (site in site_ids) {
  pr <- project_region_deterministic(base_data[base_data$SITE_ID != site, ], default_reg_cfg,
                                     GLOBAL_WETLAND_BUDGET_TG, MOISTURE_DRY_SENS, MOISTURE_WET_SENS)
  pr <- pr[pr$year == PROJECTION_END_YEAR, c("ssp", "additional_Tg_per_yr")]
  pr$SITE_ID_removed <- site
  pr <- merge(pr, baseline_det, by = "ssp")
  pr$delta_from_baseline <- pr$additional_Tg_per_yr - pr$baseline_additional_Tg_per_yr
  loo_rows[[site]] <- pr
}
loo <- do.call(rbind, loo_rows)
save_output_csv(loo, out_rel("leave_one_site_out_projection_2100.csv"), analysis_dir)

message("Diagnostics complete. Key outputs are in outputs/diagnostics.")
message("Preferred headline source: outputs/regional_projection/regional_global_projection.csv")
