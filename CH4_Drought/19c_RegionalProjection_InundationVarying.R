# 19c_RegionalProjection_InundationVarying.R   (EXPLORATORY variant of 19)
# ---------------------------------------------------------------------------
# WHAT THIS ADDS TO 19_RegionalProjection.R
# -----------------------------------------
# Script 19 holds the wetland area (and therefore the per-band CH4 budget) FIXED,
# so the only thing that changes through time is the per-area flux response to a
# shifting frequency of hot/cold/dry/wet extremes. This variant lets the
# INUNDATION FRACTION (wetland area) itself vary with extremes, driven directly
# by each SSP's global-warming level (GWL) and the moisture sensitivities already
# in config.R, with an ASYMMETRIC dry/wet response (drought drains area more
# strongly than wet events re-flood it).
#
# It is a SEPARATE, self-contained script (nothing else in the pipeline changes),
# so we can compare it against the fixed-area result before deciding whether to
# adopt it. Every knob below has a get0() fallback, so config.R needs no edits.
#
# MECHANISM
# ---------
# For region r and year t, with warming departure dG(t) = GWL(t) - GWL_BASELINE:
#
#   moisture forcing (SPEI units, land-amplified):
#       dry_forcing(t)  = LA_r * MOISTURE_DRY_SENS * dG(t)     (>=0, drying)
#       wet_forcing(t)  = LA_r * MOISTURE_WET_SENS * dG(t)     (>=0, wet-tail)
#
#   log area anomaly (asymmetric elasticities; drainage easier than re-flooding):
#       log A_r(t) = INUND_WET_ELASTICITY * wet_forcing(t)
#                  - INUND_DRY_ELASTICITY * dry_forcing(t)
#       A_r(t)     = clamp( exp(log A_r(t)), INUND_AREA_MIN, INUND_AREA_MAX )
#
# A_r(t) is a multiplicative factor on the baseline inundated area (A=1 at the
# baseline period). Total regional emission is the product of a per-area flux
# factor and this area factor, so the additional emission relative to baseline is:
#
#       add_r(t) = budget_r * [ (1 + (E(t)-E_base)/F0) * A_r(t) - 1 ]
#
# When A_r(t) == 1 this collapses EXACTLY to script 19's formula, so any
# difference between the two runs is purely the inundation-variation effect.
#
# Monte Carlo propagates BOTH the response-function SE (as in 19) AND uncertainty
# in the area elasticities (INUND_ELASTICITY_CV), so the reported 5-95% band
# includes deep uncertainty in the area response.
#
# Outputs -> outputs/regional_projection_inundation/ (+ server):
#   regional_global_projection_inundation.csv   varying-area global projection
#   regional_vs_fixed_area_2100.csv             side-by-side vs fixed-area (19)
#   inundation_area_factor.csv                  A_r(t) by region/SSP (diagnostic)
#   regional_breakdown_2100.csv                 per-band 2100 (varying area)
#   figures/inundation_varying_projection.png   comparison figure
# ---------------------------------------------------------------------------

rm(list = ls())
# Locate the folder holding config.R. Robust to being run via `Rscript`,
# `source()`, or the RStudio "Source" button (the latter two pass no --file
# argument, so getwd() alone can point one level up). Mirrors run_pipeline.R.
locate_analysis_dir <- function() {
  cand <- character(0)
  args <- commandArgs(FALSE)
  f <- args[grepl("^--file=", args)]
  if (length(f)) cand <- c(cand, dirname(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE)))
  for (i in seq_len(sys.nframe())) {                 # sourced file: frame$ofile
    of <- sys.frame(i)$ofile
    if (!is.null(of)) cand <- c(cand, dirname(normalizePath(of, mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) cand <- c(cand, dirname(normalizePath(p, mustWork = FALSE)))
  }
  cand <- c(cand, getwd(), file.path(getwd(), "CH4_Drought"))
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) return(hit[1])
  stop("Could not find config.R. Set the working directory to the CH4_Drought folder.")
}
analysis_dir <- locate_analysis_dir()
message("analysis_dir: ", analysis_dir)
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))
set.seed(7)
out_rel <- function(f) file.path("regional_projection_inundation", f)
MO <- c("drought", "normal", "extreme_wet"); TE <- c("cold", "normal", "hot")

# ---- inundation-variation settings ----------------------------------------
# These scalars are FALLBACK defaults, used only if the calibration table from
# 19b (data/inundation_elasticities.csv) is absent. When that file exists, 19c
# uses the fitted PER-BAND elasticities and their bootstrap SEs instead (see the
# calibration loader after reg_cfg below). Elasticity = fractional log-area
# change per SPEI unit; dry > wet encodes the asymmetric (drain-faster) response.
INUND_DRY_ELASTICITY <- get0("INUND_DRY_ELASTICITY", ifnotfound = 0.25) # area loss per unit drying
INUND_WET_ELASTICITY <- get0("INUND_WET_ELASTICITY", ifnotfound = 0.10) # area gain per unit wetting
INUND_AREA_MIN       <- get0("INUND_AREA_MIN",       ifnotfound = 0.30) # floor on area factor
INUND_AREA_MAX       <- get0("INUND_AREA_MAX",       ifnotfound = 2.00) # ceiling on area factor
INUND_ELASTICITY_CV  <- get0("INUND_ELASTICITY_CV",  ifnotfound = 0.50) # MC spread when uncalibrated
INUND_ELASTICITY_FILE <- get0("INUND_ELASTICITY_FILE", ifnotfound = "data/inundation_elasticities.csv")

# ---- data + site latitude -> region band (identical to 19) ----
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
d <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
d$condition <- ifelse(d[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
               ifelse(d[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal"))
d <- d[!is.na(d$temp_class) & !is.na(d$condition) & !is.na(d$normalized_Fch4), ]

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

# ---- per-band elasticities: calibrated (19b) if available, else fallbacks ----
# band_elas(region) returns list(e_dry, e_dry_se, e_wet, e_wet_se). The MC below
# draws each elasticity from Normal(mean, se) (truncated at 0) so the projection
# inherits the REAL fitted uncertainty when calibrated.
.elas_path <- file.path(analysis_dir, INUND_ELASTICITY_FILE)
USE_CALIBRATED <- file.exists(.elas_path)
if (USE_CALIBRATED) {
  .elas_tab <- read.csv(.elas_path, stringsAsFactors = FALSE)
  message("Inundation elasticities: CALIBRATED from ", INUND_ELASTICITY_FILE)
} else {
  message("Inundation elasticities: DEFAULT scalars (run 19b to calibrate from WAD2M).")
}
band_elas <- function(region) {
  if (USE_CALIBRATED) {
    e <- .elas_tab[.elas_tab$region == region, ]
    if (nrow(e)) return(list(e_dry = e$e_dry[1], e_dry_se = e$e_dry_se[1],
                             e_wet = e$e_wet[1], e_wet_se = e$e_wet_se[1]))
  }
  list(e_dry = INUND_DRY_ELASTICITY, e_dry_se = INUND_DRY_ELASTICITY * INUND_ELASTICITY_CV,
       e_wet = INUND_WET_ELASTICITY, e_wet_se = INUND_WET_ELASTICITY * INUND_ELASTICITY_CV)
}

assign_region <- function(lat) {
  a <- abs(lat)
  reg_cfg$region[vapply(a, function(x) which(x > reg_cfg$abs_lat_min - 1e-9 & x <= reg_cfg$abs_lat_max + 1e-9)[1], integer(1))]
}
d$region <- assign_region(d$LAT)
gcell <- tapply(d$normalized_Fch4, list(d$condition, d$temp_class), mean, na.rm = TRUE)

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

# Deterministic (mean-elasticity) area factor A_r(t) for diagnostics/reporting.
area_factor <- function(LA, dG, e_dry = INUND_DRY_ELASTICITY, e_wet = INUND_WET_ELASTICITY) {
  logA <- e_wet * (LA * MOISTURE_WET_SENS * dG) - e_dry * (LA * MOISTURE_DRY_SENS * dG)
  pmin(pmax(exp(logA), INUND_AREA_MIN), INUND_AREA_MAX)
}

NMC <- PROJECTION_MC_DRAWS
alloc <- read.csv(file.path(analysis_dir, CONTINENT_ALLOC), row.names = 1)  # continents x bands
proj_rows <- list(); proj_rows_fixed <- list(); breakdown <- list(); area_rows <- list(); cont_breakdown <- list()
for (ssp in unique(gwl$ssp)) {
  s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
  g  <- approx(s$year, s$gwl, years, rule = 2)$y
  dG <- g - GWL_BASELINE

  reg_add       <- array(0, dim = c(nrow(reg_cfg), NMC, length(years)))  # varying area
  reg_add_fixed <- array(0, dim = c(nrow(reg_cfg), NMC, length(years)))  # A == 1 (== script 19)

  for (ri in seq_len(nrow(reg_cfg))) {
    r <- reg_cfg$region[ri]; rs <- RS[[r]]; LA <- reg_cfg$land_amplification[ri]
    Js <- lapply(dG, function(gi) { p <- marg(rs, LA, gi)
      ipf(rs$seed, c(p["p_dry"], 1 - p["p_dry"] - p["p_wet"], p["p_wet"]),
                   c(p["p_cold"], 1 - p["p_cold"] - p["p_hot"], p["p_hot"])) })
    budget_r <- reg_cfg$budget_share[ri] * GLOBAL_WETLAND_BUDGET_TG
    be <- band_elas(r)   # per-band elasticity mean + SE (calibrated or fallback)

    # diagnostic (mean-elasticity) area trajectory for this region/ssp
    A_mean <- area_factor(LA, dG, be$e_dry, be$e_wet)
    area_rows[[paste(ssp, r)]] <- data.frame(ssp = ssp, region = r, year = years,
                                             area_factor = round(A_mean, 4))

    for (k in seq_len(NMC)) {
      Rd <- rs$R + rs$SE * matrix(rnorm(9), 3, 3)
      E  <- vapply(Js, function(J) sum(J * Rd), numeric(1))
      flux_factor <- 1 + (E - E[1]) / rs$F0

      # draw per-band elasticities from their sampling distribution (truncated >= 0)
      e_dry <- max(rnorm(1, be$e_dry, be$e_dry_se), 0)
      e_wet <- max(rnorm(1, be$e_wet, be$e_wet_se), 0)
      A_k   <- area_factor(LA, dG, e_dry, e_wet)

      reg_add[ri, k, ]       <- budget_r * (flux_factor * A_k - 1)
      reg_add_fixed[ri, k, ] <- budget_r * (flux_factor - 1)
    }
  }

  # continent split of the VARYING-area response, using the same continent x band
  # allocation as 19 (columns sum to each band's budget share, so continents sum
  # EXACTLY to the band-weighted global total for the inundation-varying run too).
  cont_add <- array(0, dim = c(nrow(alloc), NMC, length(years)))
  for (ri in seq_len(nrow(reg_cfg))) {
    b <- reg_cfg$region[ri]
    budget_r <- reg_cfg$budget_share[ri] * GLOBAL_WETLAND_BUDGET_TG
    band_frac <- reg_add[ri, , ] / budget_r
    for (ci in seq_len(nrow(alloc)))
      cont_add[ci, , ] <- cont_add[ci, , ] + band_frac * alloc[ci, b] * GLOBAL_WETLAND_BUDGET_TG
  }
  for (ci in seq_len(nrow(alloc))) {
    v <- cont_add[ci, , length(years)]
    cont_breakdown[[paste(ssp, rownames(alloc)[ci])]] <- data.frame(
      ssp = ssp, continent = rownames(alloc)[ci], budget_share = sum(alloc[ci, ]),
      additional_Tg_per_yr_2100 = median(v), lo = quantile(v, 0.05), hi = quantile(v, 0.95))
  }

  glob  <- apply(reg_add,       c(2, 3), sum); gcum  <- t(apply(glob,  1, cumsum))
  globf <- apply(reg_add_fixed, c(2, 3), sum)
  q <- function(m, p) apply(m, 2, quantile, probs = p)
  proj_rows[[ssp]] <- data.frame(ssp = ssp, year = years,
    additional_Tg_per_yr_median = apply(glob, 2, median),
    lo = q(glob, 0.05), hi = q(glob, 0.95),
    cumulative_Tg_median = apply(gcum, 2, median),
    cumulative_lo = q(gcum, 0.05), cumulative_hi = q(gcum, 0.95))
  proj_rows_fixed[[ssp]] <- data.frame(ssp = ssp, year = years,
    additional_Tg_per_yr_median_fixed = apply(globf, 2, median))

  for (ri in seq_len(nrow(reg_cfg))) {
    v <- reg_add[ri, , length(years)]
    be2 <- band_elas(reg_cfg$region[ri])
    breakdown[[paste(ssp, reg_cfg$region[ri])]] <- data.frame(
      ssp = ssp, region = reg_cfg$region[ri], budget_share = reg_cfg$budget_share[ri],
      n_sites = RS[[reg_cfg$region[ri]]]$n_sites,
      area_factor_2100 = round(area_factor(reg_cfg$land_amplification[ri], dG[length(dG)], be2$e_dry, be2$e_wet), 3),
      additional_Tg_per_yr_2100 = median(v), lo = quantile(v, 0.05), hi = quantile(v, 0.95))
  }
}
proj  <- do.call(rbind, proj_rows)
projf <- do.call(rbind, proj_rows_fixed)
bd    <- do.call(rbind, breakdown)
cbd   <- do.call(rbind, cont_breakdown)
area  <- do.call(rbind, area_rows)

save_output_csv(proj[proj$year %% PROJECTION_STEP_YEARS == 0, ],
                out_rel("regional_global_projection_inundation.csv"), analysis_dir)
save_output_csv(area[area$year %% PROJECTION_STEP_YEARS == 0, ],
                out_rel("inundation_area_factor.csv"), analysis_dir)
save_output_csv(bd, out_rel("regional_breakdown_2100.csv"), analysis_dir)
save_output_csv(cbd, out_rel("continent_contributions_2100_inundation.csv"), analysis_dir)

# side-by-side varying vs fixed area at 2100
cmp <- merge(proj[, c("ssp", "year", "additional_Tg_per_yr_median")],
             projf, by = c("ssp", "year"))
cmp2100 <- cmp[cmp$year == max(years), ]
cmp2100$delta_from_area <- cmp2100$additional_Tg_per_yr_median -
                           cmp2100$additional_Tg_per_yr_median_fixed
cmp2100$pct_change <- round(100 * cmp2100$delta_from_area /
                            abs(cmp2100$additional_Tg_per_yr_median_fixed), 1)
cmp2100[, sapply(cmp2100, is.numeric)] <- round(cmp2100[, sapply(cmp2100, is.numeric)], 3)
save_output_csv(cmp2100, out_rel("regional_vs_fixed_area_2100.csv"), analysis_dir)

# ---- comparison figure: varying (solid) vs fixed (dashed) area ----
ssps <- unique(proj$ssp)
# SSP line colors: non-red/blue qualitative ramp (green -> gold -> orange -> magenta)
cols <- setNames(c("#1b9e77", "#e6ab02", "#d95f02", "#e7298a")[seq_along(ssps)], ssps)
draw <- function() {
  # (a) spans the full top row (largest); (b) and (c) share the bottom row.
  layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE), heights = c(1.4, 1))
  par(cex.axis = 1.15, cex.lab = 1.3, cex.main = 1.35)
  panel <- function(l) mtext(l, side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)
  # (a) TOP: additional emission RATE, varying (solid) vs fixed (dashed)
  par(mar = c(4.6, 5.2, 3, 1.5))
  plot(NA, xlim = range(years), ylim = range(proj$lo, proj$hi),
       xlab = "Year", ylab = expression("Additional wetland CH"[4] * " (Tg/yr)"), main = "")
  panel("(a)")
  abline(h = 0, col = "gray70")
  for (ssp in ssps) {
    sv <- proj[proj$ssp == ssp, ]; sf <- projf[projf$ssp == ssp, ]
    polygon(c(sv$year, rev(sv$year)), c(sv$lo, rev(sv$hi)),
            col = adjustcolor(cols[ssp], 0.12), border = NA)
    lines(sv$year, sv$additional_Tg_per_yr_median, col = cols[ssp], lwd = 3)
    lines(sf$year, sf$additional_Tg_per_yr_median_fixed, col = cols[ssp], lwd = 2.5, lty = 2)
  }
  legend("topleft", legend = ssps, col = cols, lwd = 3, bty = "n", cex = 1.15)
  # (b) BOTTOM-LEFT: area factor trajectories (strongest-warming SSP)
  topssp <- ssps[length(ssps)]
  a <- area[area$ssp == topssp, ]
  regs <- unique(a$region); rcol <- setNames(c("#1b9e77", "#e6ab02", "#7570b3", "#66a61e")[seq_along(regs)], regs)
  par(mar = c(4.6, 5.2, 3, 1.5))
  plot(NA, xlim = range(years), ylim = range(area$area_factor),
       xlab = "Year", ylab = "Inundation area factor", main = "")
  panel("(b)")
  abline(h = 1, col = "gray70")
  for (rg in regs) { ar <- a[a$region == rg, ]; lines(ar$year, ar$area_factor, col = rcol[rg], lwd = 3) }
  legend("bottomleft", legend = regs, col = rcol, lwd = 3, bty = "n", cex = 1.15)
  # (c) BOTTOM-RIGHT: cumulative additional CH4 by SSP (median + 5-95% CI)
  yend <- max(years)
  cm  <- sapply(ssps, function(s) proj$cumulative_Tg_median[proj$ssp == s & proj$year == yend])
  clo <- sapply(ssps, function(s) proj$cumulative_lo[proj$ssp == s & proj$year == yend])
  chi <- sapply(ssps, function(s) proj$cumulative_hi[proj$ssp == s & proj$year == yend])
  yl <- range(0, clo, chi); yl <- yl + c(-0.08, 0.18) * diff(yl)
  par(mar = c(6.5, 5.2, 3, 1.5))                     # tall bottom margin for rotated SSP labels
  bp <- barplot(cm, col = cols[ssps], names.arg = ssps, las = 2, ylim = yl,
                ylab = expression("Cumulative CH"[4] * " 2020-2100 (Tg)"), main = "")
  abline(h = 0, col = "gray70")
  arrows(bp, clo, bp, chi, angle = 90, code = 3, length = 0.05, col = "gray20")
  text(bp, chi, round(cm), pos = 3, xpd = TRUE, font = 2, cex = 1.05)
  panel("(c)")
}
save_output_figure(draw, out_rel("figures/inundation_varying_projection.png"),
                   analysis_dir, width = 2200, height = 2000, res = 220)

# ---- calibration diagnostic figure (moved here from 19b) --------------------
# 19b is slow (WAD2M x SPEI calibration + bootstrap), so the plotting lives here
# instead: it is rebuilt from the CSVs 19b writes -- data/inundation_elasticities.csv
# (loaded above as .elas_tab) and outputs/.../inundation_calibration_fit.csv (the
# binned area response per band). Skips cleanly if 19b has not been run.
.cal_fit_path <- file.path(analysis_dir, "outputs", "regional_projection_inundation",
                           "inundation_calibration_fit.csv")
if (USE_CALIBRATED && file.exists(.cal_fit_path)) {
  cal_fit <- read.csv(.cal_fit_path, stringsAsFactors = FALSE)
  cal_elas <- .elas_tab
  cal_bands <- cal_elas$region
  cal_cols <- setNames(c("#1b9e77", "#e6ab02", "#7570b3", "#66a61e")[seq_along(cal_bands)], cal_bands)
  draw_cal <- function() {
    par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))
    # (a) binned area response with fitted piecewise line per band
    plot(NA, xlim = c(-3, 3), ylim = range(cal_fit$ybar, na.rm = TRUE),
         xlab = "SPEI anomaly", ylab = "log(area / climatology)",
         main = "Area response to SPEI (binned + fit)")
    mtext("(a)", side = 3, line = 0.6, adj = 0, font = 2, cex = 1.1)
    abline(h = 0, v = 0, col = "gray80")
    for (bd in cal_bands) {
      fp <- cal_fit[cal_fit$band == bd, ]
      points(fp$xmid, fp$ybar, col = cal_cols[bd], pch = 19)
      e <- cal_elas[cal_elas$region == bd, ]
      xx <- seq(-3, 3, 0.1); yy <- e$e_wet * pmax(xx, 0) + e$e_dry * pmin(xx, 0)
      lines(xx, yy, col = cal_cols[bd], lwd = 2)
    }
    legend("topleft", legend = cal_bands, col = cal_cols[cal_bands], pch = 19, lwd = 2, bty = "n")
    # (b) fitted elasticities with bootstrap 90% CI
    ne <- nrow(cal_elas); at <- seq_len(ne); off <- 0.12
    plot(NA, xlim = c(0.5, ne + 0.5),
         ylim = range(0, cal_elas$e_dry_hi, cal_elas$e_wet_hi, na.rm = TRUE),
         xaxt = "n", xlab = "", ylab = "Elasticity (d log area / d SPEI)",
         main = "Fitted elasticities (90% CI)")
    mtext("(b)", side = 3, line = 0.6, adj = 0, font = 2, cex = 1.1)
    axis(1, at, cal_elas$region)
    arrows(at - off, cal_elas$e_dry_lo, at - off, cal_elas$e_dry_hi, angle = 90, code = 3, length = 0.04, col = "#d95f02")
    points(at - off, cal_elas$e_dry, pch = 19, col = "#d95f02")
    arrows(at + off, cal_elas$e_wet_lo, at + off, cal_elas$e_wet_hi, angle = 90, code = 3, length = 0.04, col = "#e7298a")
    points(at + off, cal_elas$e_wet, pch = 19, col = "#e7298a")
    legend("topright", legend = c("e_dry", "e_wet"), col = c("#d95f02", "#e7298a"), pch = 19, bty = "n")
  }
  save_output_figure(draw_cal, out_rel("figures/inundation_calibration.png"),
                     analysis_dir, width = 2200, height = 1000, res = 180)
} else {
  message("Skipping calibration diagnostic figure (needs 19b outputs: ",
          "inundation_elasticities.csv + inundation_calibration_fit.csv).")
}

message("Inundation-varying regional projection complete.")
message("Additional CH4 by 2100 (Tg/yr) -- varying area vs fixed area:")
for (ssp in ssps) {
  rv <- proj[proj$ssp == ssp & proj$year == max(years), ]
  rf <- projf[projf$ssp == ssp & projf$year == max(years), ]
  message(sprintf("  %-9s varying %+6.1f [%.1f, %.1f]   fixed %+6.1f   (area effect %+.1f Tg/yr)",
                  ssp, rv$additional_Tg_per_yr_median, rv$lo, rv$hi,
                  rf$additional_Tg_per_yr_median_fixed,
                  rv$additional_Tg_per_yr_median - rf$additional_Tg_per_yr_median_fixed))
}
if (USE_CALIBRATED) {
  message("Elasticities: CALIBRATED per band from ", INUND_ELASTICITY_FILE,
          " (clamp [", INUND_AREA_MIN, ", ", INUND_AREA_MAX, "]).")
  print(.elas_tab[, intersect(c("region","e_dry","e_dry_se","e_wet","e_wet_se","r2"), names(.elas_tab))],
        row.names = FALSE)
} else {
  message("Elasticities: DEFAULT dry=", INUND_DRY_ELASTICITY, " wet=", INUND_WET_ELASTICITY,
          " (CV=", INUND_ELASTICITY_CV, "); run 19b to calibrate from WAD2M.")
}
message("Compare against fixed-area script 19 via regional_vs_fixed_area_2100.csv")
