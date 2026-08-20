# 18_ExtremeEmissionsProjection.R   (runs after 17_ScenarioFrequencies_CMIP6.R)
# ---------------------------------------------------------------------------
# Project additional wetland methane driven by climate extremes, now -> 2100,
# for all SSPs. Three ingredients:
#
#  (1) RESPONSE  R[c] +/- SE[c] : site-level mean normalized CH4 anomaly (and its
#      between-site SE) for each compound class c = moisture x temperature.
#  (2) FREQUENCY : projected occupancy of each class. Marginal hot/cold/dry/wet
#      frequencies come from 17_ScenarioFrequencies_CMIP6.R; the JOINT 3x3
#      occupancy is obtained by Iterative Proportional Fitting (IPF) of the
#      OBSERVED joint table to those marginals, which PRESERVES the observed
#      hot-dry (and other) co-occurrence structure rather than assuming
#      independence.
#  (3) UNCERTAINTY : Monte Carlo over R[c] ~ Normal(mean, SE) gives a 5-95%
#      range on the projected emissions.
#
#      E(t)   = sum_c J[c](t) * R[c]
#      dE(t)  = E(t) - E(base)
#      dTg(t) = (dE(t) / F0) * global wetland budget      (F0 = baseline mean flux)
#      cumulative = annual integral of dTg(t).
#
# Outputs -> outputs/extreme_emissions_projection/ (+ server).
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))
set.seed(42)

out_rel <- function(f) file.path("extreme_emissions_projection", f)
moist <- c("drought", "normal", "extreme_wet")
temp  <- c("cold", "normal", "hot")

# ---- (1) response with per-cell uncertainty (site-level) + observed joint seed ----
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
d <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
d$condition <- ifelse(d[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
               ifelse(d[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal"))
d <- d[!is.na(d$temp_class) & !is.na(d$condition) & !is.na(d$normalized_Fch4), ]

R <- matrix(NA_real_, 3, 3, dimnames = list(moist, temp))
SE <- matrix(NA_real_, 3, 3, dimnames = list(moist, temp))
seed <- matrix(0, 3, 3, dimnames = list(moist, temp))
for (mi in moist) for (tj in temp) {
  sub <- d[d$condition == mi & d$temp_class == tj, ]
  seed[mi, tj] <- nrow(sub)
  site_means <- tapply(sub$normalized_Fch4, sub$SITE_ID, mean, na.rm = TRUE)
  site_means <- site_means[is.finite(site_means)]
  R[mi, tj]  <- mean(site_means)
  SE[mi, tj] <- if (length(site_means) > 1) stats::sd(site_means) / sqrt(length(site_means)) else NA_real_
}
SE[is.na(SE)] <- mean(SE, na.rm = TRUE)
F0     <- mean(fluxes.drought_normalized$FCH4_F_ANNOPTLM, na.rm = TRUE)
BUDGET <- get0("GLOBAL_WETLAND_BUDGET_TG", ifnotfound = 150)

resp_out <- data.frame(condition = rep(moist, each = 3), temp_class = rep(temp, 3),
                       R_mean = as.vector(t(R)), R_SE = as.vector(t(SE)),
                       seed_n = as.vector(t(seed)))
save_output_csv(resp_out, out_rel("compound_response_with_uncertainty.csv"), analysis_dir)

# ---- (2) marginals -> IPF joint (preserves observed association) ----
freq <- read.csv(file.path(analysis_dir, PROJECTION_FREQ_TABLE), stringsAsFactors = FALSE)
ipf <- function(seed, row_t, col_t, iters = 60) {
  J <- seed / sum(seed)
  for (i in seq_len(iters)) {
    J <- J * (row_t / rowSums(J))
    J <- J * matrix(col_t / colSums(J), nrow(J), ncol(J), byrow = TRUE)
  }
  J
}
joint_for <- function(p_dry, p_wet, p_cold, p_hot)
  ipf(seed, c(p_dry, 1 - p_dry - p_wet, p_wet), c(p_cold, 1 - p_cold - p_hot, p_hot))

# annual interpolation of marginals within each SSP
years_all <- seq(get0("PROJECTION_BASE_YEAR", ifnotfound = 2020),
                 get0("PROJECTION_END_YEAR",  ifnotfound = 2100))
build_joints <- function(ssp) {
  s <- freq[freq$ssp == ssp, ]; s <- s[order(s$year), ]
  ip <- function(col) approx(s$year, s[[col]], xout = years_all, rule = 2)$y
  Map(function(pd, pw, pc, ph) joint_for(pd, pw, pc, ph),
      ip("p_dry"), ip("p_wet"), ip("p_cold"), ip("p_hot"))
}

# ---- (3) Monte Carlo over response uncertainty ----
NMC <- get0("PROJECTION_MC_DRAWS", ifnotfound = 3000)
project_ssp <- function(ssp) {
  Js <- build_joints(ssp)
  rate <- matrix(0, NMC, length(years_all)); cum <- rate
  for (k in seq_len(NMC)) {
    Rd <- R + SE * matrix(rnorm(9), 3, 3)
    E <- vapply(Js, function(J) sum(J * Rd), numeric(1))
    dTg <- ((E - E[1]) / F0) * BUDGET
    rate[k, ] <- dTg; cum[k, ] <- cumsum(dTg)
  }
  qs <- function(m, p) apply(m, 2, quantile, probs = p)
  data.frame(ssp = ssp, year = years_all,
             additional_Tg_per_yr_median = apply(rate, 2, median),
             additional_Tg_per_yr_lo = qs(rate, 0.05), additional_Tg_per_yr_hi = qs(rate, 0.95),
             cumulative_Tg_median = apply(cum, 2, median),
             cumulative_Tg_lo = qs(cum, 0.05), cumulative_Tg_hi = qs(cum, 0.95))
}
proj <- do.call(rbind, lapply(unique(freq$ssp), project_ssp))

proj_dec <- proj[proj$year %% PROJECTION_STEP_YEARS == 0, ]
proj_dec[, -(1:2)] <- round(proj_dec[, -(1:2)], 2)
save_output_csv(proj_dec, out_rel("extreme_emissions_projection_uncertainty.csv"), analysis_dir)

summary_2100 <- do.call(rbind, lapply(unique(proj$ssp), function(ssp) {
  r <- proj[proj$ssp == ssp & proj$year == max(proj$year), ]
  data.frame(ssp = ssp,
             additional_Tg_per_yr_2100 = round(r$additional_Tg_per_yr_median, 2),
             ci90 = sprintf("[%.1f, %.1f]", r$additional_Tg_per_yr_lo, r$additional_Tg_per_yr_hi),
             cumulative_Tg_2020_2100 = round(r$cumulative_Tg_median),
             cumulative_ci90 = sprintf("[%.0f, %.0f]", r$cumulative_Tg_lo, r$cumulative_Tg_hi))
}))
save_output_csv(summary_2100, out_rel("extreme_emissions_summary_2100.csv"), analysis_dir)

# ---- figure ----
ssps <- unique(proj$ssp)
cols <- setNames(c("#4575b4", "#74add1", "#f46d43", "#a50026")[seq_along(ssps)], ssps)
draw <- function() {
  par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))
  plot(NA, xlim = range(years_all), ylim = range(proj$additional_Tg_per_yr_hi, proj$additional_Tg_per_yr_lo),
       xlab = "Year", ylab = "Additional wetland CH4 from extremes (Tg/yr)",
       main = "Extra emission rate (5-95% band)")
  abline(h = 0, col = "gray70")
  for (ssp in ssps) {
    s <- proj[proj$ssp == ssp, ]
    polygon(c(s$year, rev(s$year)), c(s$additional_Tg_per_yr_lo, rev(s$additional_Tg_per_yr_hi)),
            col = adjustcolor(cols[ssp], 0.15), border = NA)
    lines(s$year, s$additional_Tg_per_yr_median, col = cols[ssp], lwd = 2.5)
  }
  legend("topleft", legend = ssps, col = cols, lwd = 2.5, bty = "n")
  cm <- sapply(ssps, function(ssp) proj$cumulative_Tg_median[proj$ssp == ssp & proj$year == max(proj$year)])
  lo <- sapply(ssps, function(ssp) proj$cumulative_Tg_lo[proj$ssp == ssp & proj$year == max(proj$year)])
  hi <- sapply(ssps, function(ssp) proj$cumulative_Tg_hi[proj$ssp == ssp & proj$year == max(proj$year)])
  bp <- barplot(cm, col = cols, ylab = "Cumulative extra CH4 2020-2100 (Tg)", ylim = c(0, max(hi) * 1.15),
                main = "Cumulative additional emissions (5-95%)", las = 2)
  arrows(bp, lo, bp, hi, angle = 90, code = 3, length = 0.05, col = "gray20")
  text(bp, hi, round(cm), pos = 3, xpd = TRUE, font = 2)
}
save_output_figure(draw, out_rel("figures/extreme_emissions_projection.png"),
                   analysis_dir, width = 2200, height = 1000, res = 180)

message("Projection complete (IPF joint + Monte Carlo response uncertainty).")
print(summary_2100, row.names = FALSE)
message("Frequencies: ", PROJECTION_FREQ_TABLE, " (regenerate via 17_ScenarioFrequencies_CMIP6.R)")
