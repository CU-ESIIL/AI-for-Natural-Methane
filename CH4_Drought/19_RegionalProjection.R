# 19_RegionalProjection.R   (region-weighted global version of 13)
# ---------------------------------------------------------------------------
# The site-based projection (13) scales one site-derived fractional response by
# a single global wetland budget, which paints a Northern-dominated response
# onto a tropics-dominated source. This script makes the upscaling REGION-
# EXPLICIT: each latitude band gets its OWN response (+SE), its OWN baseline
# extreme frequencies and temperature variability, its OWN warming (land
# amplification), and its OWN share of the global wetland CH4 budget. Bands are
# summed to a region-weighted global total with Monte-Carlo uncertainty.
#
# Bands / shares / land amplification are editable in data/wetland_region_config.csv.
# Outputs -> outputs/regional_projection/ (+ server).
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))
set.seed(7)
out_rel <- function(f) file.path("regional_projection", f)
MO <- c("drought", "normal", "extreme_wet"); TE <- c("cold", "normal", "hot")

# ---- data + site latitude -> region band ----
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
d <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
d$condition <- ifelse(d[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
               ifelse(d[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal"))
d <- d[!is.na(d$temp_class) & !is.na(d$condition) & !is.na(d$normalized_Fch4), ]

# resolve site latitude (from the table, else a coords CSV on the server/project)
if (!"LAT" %in% names(d)) {
  cand <- c(file.path(SERVER_DIR, "data", "SiteLatLon.csv"),
            file.path(analysis_dir, "data", "SiteLatLon.csv"))
  cf <- cand[file.exists(cand)][1]
  if (is.na(cf)) stop("No site latitude available; provide data/SiteLatLon.csv (site,lat,lon).")
  ll <- read.csv(cf); names(ll) <- tolower(names(ll))
  ll <- ll[, c(grep("site", names(ll))[1], grep("lat", names(ll))[1])]
  names(ll) <- c("SITE_ID", "LAT")
  d <- merge(d, ll, by = "SITE_ID", all.x = TRUE)
}
d <- d[!is.na(d$LAT), ]

reg_cfg <- read.csv(file.path(analysis_dir, REGION_CONFIG), stringsAsFactors = FALSE)
assign_region <- function(lat) {
  a <- abs(lat)
  reg_cfg$region[vapply(a, function(x) which(x > reg_cfg$abs_lat_min - 1e-9 & x <= reg_cfg$abs_lat_max + 1e-9)[1], integer(1))]
}
d$region <- assign_region(d$LAT)

gcell <- tapply(d$normalized_Fch4, list(d$condition, d$temp_class), mean, na.rm = TRUE)  # global fallback

region_stats <- function(sub) {
  R <- SE <- matrix(NA_real_, 3, 3, dimnames = list(MO, TE)); seed <- matrix(0, 3, 3, dimnames = list(MO, TE))
  for (mi in MO) for (tj in TE) {
    cc <- sub[sub$condition == mi & sub$temp_class == tj, ]
    seed[mi, tj] <- nrow(cc)
    sm <- tapply(cc$normalized_Fch4, cc$SITE_ID, mean, na.rm = TRUE); sm <- sm[is.finite(sm)]
    if (length(sm) >= 1) R[mi, tj] <- mean(sm)
    if (length(sm) > 1)  SE[mi, tj] <- stats::sd(sm) / sqrt(length(sm))
  }
  na <- is.na(R); R[na] <- gcell[cbind(row(R)[na], col(R)[na])]; seed[na & seed == 0] <- 1
  SE[is.na(SE)] <- mean(SE, na.rm = TRUE)
  list(R = R, SE = SE, seed = seed, F0 = mean(sub$FCH4_F_ANNOPTLM, na.rm = TRUE),
       sigma = mean(tapply(sub$TA_F, paste(sub$SITE_ID, sub$month), stats::sd, na.rm = TRUE), na.rm = TRUE),
       p_hot0 = mean(sub$temp_class == "hot"), p_cold0 = mean(sub$temp_class == "cold"),
       p_dry0 = mean(sub$condition == "drought"), p_wet0 = mean(sub$condition == "extreme_wet"),
       n_sites = length(unique(sub$SITE_ID)))
}
RS <- lapply(setNames(reg_cfg$region, reg_cfg$region), function(r) region_stats(d[d$region == r, ]))

# ---- projection machinery ----
gwl <- read.csv(file.path(analysis_dir, SSP_GWL_TABLE), stringsAsFactors = FALSE)
years <- seq(PROJECTION_BASE_YEAR, PROJECTION_END_YEAR)
ipf <- function(seed, rt, ct, it = 60) { J <- seed / sum(seed)
  for (i in seq_len(it)) { J <- J * (rt / rowSums(J)); J <- J * matrix(ct / colSums(J), 3, 3, byrow = TRUE) }; J }
marg <- function(rs, LA, dG) {
  mu <- LA * dG / rs$sigma
  c(p_dry = min(rs$p_dry0 * pnorm(-1 + MOISTURE_DRY_SENS * dG) / pnorm(-1), 0.9),
    p_wet = min(rs$p_wet0 * (1 - pnorm(1 - MOISTURE_WET_SENS * dG)) / (1 - pnorm(1)), 0.9),
    p_cold = min(rs$p_cold0 * pnorm(-1 - mu) / pnorm(-1), 0.9),
    p_hot  = min(rs$p_hot0  * (1 - pnorm(1 - mu)) / (1 - pnorm(1)), 0.9))
}
NMC <- PROJECTION_MC_DRAWS
proj_rows <- list(); breakdown <- list(); cont_breakdown <- list()
alloc <- read.csv(file.path(analysis_dir, CONTINENT_ALLOC), row.names = 1)  # continents x bands
for (ssp in unique(gwl$ssp)) {
  s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
  g <- approx(s$year, s$gwl, years, rule = 2)$y
  reg_add <- array(0, dim = c(nrow(reg_cfg), NMC, length(years)))
  for (ri in seq_len(nrow(reg_cfg))) {
    r <- reg_cfg$region[ri]; rs <- RS[[r]]; LA <- reg_cfg$land_amplification[ri]
    Js <- lapply(g, function(gi) { p <- marg(rs, LA, gi - GWL_BASELINE)
      ipf(rs$seed, c(p["p_dry"], 1 - p["p_dry"] - p["p_wet"], p["p_wet"]),
                   c(p["p_cold"], 1 - p["p_cold"] - p["p_hot"], p["p_hot"])) })
    budget_r <- reg_cfg$budget_share[ri] * GLOBAL_WETLAND_BUDGET_TG
    for (k in seq_len(NMC)) {
      Rd <- rs$R + rs$SE * matrix(rnorm(9), 3, 3)
      E <- vapply(Js, function(J) sum(J * Rd), numeric(1))
      reg_add[ri, k, ] <- ((E - E[1]) / rs$F0) * budget_r
    }
  }
  # continent split: each band's fractional response distributed to continents by
  # their share of that band (allocation columns sum to the band budget shares),
  # so continents sum EXACTLY to the band-weighted global total.
  cont_add <- array(0, dim = c(nrow(alloc), NMC, length(years)))
  for (ri in seq_len(nrow(reg_cfg))) {
    b <- reg_cfg$region[ri]
    band_frac <- reg_add[ri, , ] / (reg_cfg$budget_share[ri] * GLOBAL_WETLAND_BUDGET_TG)
    for (ci in seq_len(nrow(alloc)))
      cont_add[ci, , ] <- cont_add[ci, , ] + band_frac * alloc[ci, b] * GLOBAL_WETLAND_BUDGET_TG
  }
  for (ci in seq_len(nrow(alloc))) {
    v <- cont_add[ci, , length(years)]
    cont_breakdown[[paste(ssp, rownames(alloc)[ci])]] <- data.frame(
      ssp = ssp, continent = rownames(alloc)[ci], budget_share = sum(alloc[ci, ]),
      additional_Tg_per_yr_2100 = median(v), lo = quantile(v, 0.05), hi = quantile(v, 0.95))
  }

  glob <- apply(reg_add, c(2, 3), sum); gcum <- t(apply(glob, 1, cumsum))
  q <- function(m, p) apply(m, 2, quantile, probs = p)
  proj_rows[[ssp]] <- data.frame(ssp = ssp, year = years,
    additional_Tg_per_yr_median = apply(glob, 2, median),
    lo = q(glob, 0.05), hi = q(glob, 0.95),
    cumulative_Tg_median = apply(gcum, 2, median),
    cumulative_lo = q(gcum, 0.05), cumulative_hi = q(gcum, 0.95))
  for (ri in seq_len(nrow(reg_cfg))) {
    v <- reg_add[ri, , length(years)]
    breakdown[[paste(ssp, reg_cfg$region[ri])]] <- data.frame(
      ssp = ssp, region = reg_cfg$region[ri], budget_share = reg_cfg$budget_share[ri],
      n_sites = RS[[reg_cfg$region[ri]]]$n_sites,
      additional_Tg_per_yr_2100 = median(v), lo = quantile(v, 0.05), hi = quantile(v, 0.95))
  }
}
proj <- do.call(rbind, proj_rows); bd <- do.call(rbind, breakdown); cbd <- do.call(rbind, cont_breakdown)
save_output_csv(proj[proj$year %% PROJECTION_STEP_YEARS == 0, ], out_rel("regional_global_projection.csv"), analysis_dir)
save_output_csv(cbd, out_rel("continent_contributions_2100.csv"), analysis_dir)
save_output_csv(bd, out_rel("regional_breakdown_2100.csv"), analysis_dir)
resp <- do.call(rbind, lapply(names(RS), function(r) data.frame(region = r, condition = rep(MO, each = 3),
  temp_class = rep(TE, 3), R_mean = as.vector(t(RS[[r]]$R)), R_SE = as.vector(t(RS[[r]]$SE)),
  n_sites = RS[[r]]$n_sites, F0 = RS[[r]]$F0, sigma = RS[[r]]$sigma)))
save_output_csv(resp, out_rel("regional_response.csv"), analysis_dir)

message("Region-weighted global additional CH4 by 2100 (Tg/yr):")
for (ssp in unique(gwl$ssp)) {
  r <- proj[proj$ssp == ssp & proj$year == max(years), ]
  message(sprintf("  %s: +%.1f [%.1f, %.1f]", ssp, r$additional_Tg_per_yr_median, r$lo, r$hi))
}
message("Editable bands/shares: ", REGION_CONFIG)
