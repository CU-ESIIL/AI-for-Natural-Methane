# 21d_DurationSensitivity.R   (diagnostic; runs after the projection block)
# ---------------------------------------------------------------------------
# DURATION SENSITIVITY of the extreme-emissions projection, across ALL three
# upscaling approaches. The headline projections define "drought" on the 1-month
# moisture index (SPEI1) and so apply the SHORT-duration drought response - the
# enhancement regime. Observations show that response weakens and reverses with
# accumulation window, becoming strong suppression at multi-year (SPEI48) scales
# (Fig 4c/5c). This script re-runs each upscaling approach with the SUSTAINED-
# drought (SPEI48) compound-grid response, holding the projected class
# frequencies and joint (IPF) structure fixed, so the short-vs-long contrast
# isolates the effect of drought duration on the response function.
#
# Three upscaling approaches (matching 18 / 19 / 19c), each short vs long:
#   global    : global site-scaled projection (script 18); global marginals from
#               PROJECTION_FREQ_TABLE.
#   fixed     : region-weighted, fixed inundated area (script 19); per-band
#               marginal shifts via the SSP warming pathway.
#   variable  : region-weighted, variable inundation (script 19c, the headline);
#               adds the per-band area-elasticity factor A_r(t).
#
# Point estimates are the deterministic (mean-path) medians, which reproduce the
# reported MC medians to within rounding (short arm ~= 14.1 / 10.5 / 8.5 Tg/yr at
# SSP5-8.5 for global/fixed/variable). Frequencies are SPEI1-based (from 17), so
# this is a RESPONSE-side sensitivity, not a re-estimate of future drought
# frequency or persistence.
#
# Outputs -> outputs/diagnostics/duration_sensitivity_projection.csv (+ figure)
# ---------------------------------------------------------------------------

rm(list = ls())
.a <- commandArgs(FALSE); .f <- .a[grepl("^--file=", .a)]
.cand <- if (length(.f)) dirname(normalizePath(sub("^--file=", "", .f[1]), mustWork = FALSE)) else character(0)
.cand <- c(.cand, getwd(), file.path(getwd(), "CH4_Drought"),
           "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
.hit <- .cand[file.exists(file.path(.cand, "config.R"))]
if (!length(.hit)) stop("Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
analysis_dir <- .hit[1]
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))

MO <- c("drought", "normal", "extreme_wet"); TE <- c("cold", "normal", "hot")
LONG_IDX <- "SPEI48"; BUDGET <- get0("GLOBAL_WETLAND_BUDGET_TG", ifnotfound = 150)
AMIN <- get0("INUND_AREA_MIN", ifnotfound = 0.30); AMAX <- get0("INUND_AREA_MAX", ifnotfound = 2.00)

# --- site set from the canonical analysis table ---
da <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
if (!file.exists(da)) stop("DroughtAnalysis.RDATA not found; run 06_BuildAnalysisTable.R first.")
e <- new.env(); load(da, envir = e); keep <- unique(as.character(e$fluxes.drought_normalized$SITE_ID))

# --- multi-window SPEI + flux (SPEI1..SPEI48) ---
fdf <- file.path(analysis_dir, "data", "fluxes_drought.csv")
if (!file.exists(fdf)) fdf <- file.path(SERVER_DIR, "data", "fluxes_drought.csv")
if (!file.exists(fdf)) stop("fluxes_drought.csv not found (needs SPEI1..SPEI48).")
fd <- read.csv(fdf, check.names = FALSE)
fd <- fd[fd$SITE_ID %in% keep & is.finite(fd$FCH4_F_ANNOPTLM) & is.finite(fd$TA_F), ]
nn <- fd[is.finite(fd$SPEI1) & fd$SPEI1 > NORMAL_LOWER & fd$SPEI1 < NORMAL_UPPER, ]
fnorm <- tapply(nn$FCH4_F_ANNOPTLM, nn$SITE_ID, mean, na.rm = TRUE)
fd <- fd[fd$SITE_ID %in% names(fnorm), ]; fd$norm <- fd$FCH4_F_ANNOPTLM - fnorm[fd$SITE_ID]
key <- paste(fd$SITE_ID, fd$month, sep = "@@")
mu <- tapply(fd$TA_F, key, mean, na.rm = TRUE); sdv <- tapply(fd$TA_F, key, stats::sd, na.rm = TRUE)
nk <- tapply(fd$TA_F, key, function(x) sum(is.finite(x)))
okv <- is.finite(sdv[key]) & sdv[key] > 0 & nk[key] >= get0("TEMP_MIN_MONTH_OBS", ifnotfound = 5)
sti <- ifelse(okv, (fd$TA_F - mu[key]) / sdv[key], NA)
fd$tc <- ifelse(is.na(sti), NA, ifelse(sti <= COLD_THRESHOLD, "cold", ifelse(sti >= HOT_THRESHOLD, "hot", "normal")))
condf <- function(idx) ifelse(!is.finite(fd[[idx]]), NA,
                       ifelse(fd[[idx]] <= DROUGHT_THRESHOLD, "drought",
                       ifelse(fd[[idx]] >= WET_THRESHOLD, "extreme_wet", "normal")))
fd$c1 <- condf(DROUGHT_INDEX); fd$c48 <- condf(LONG_IDX)

# --- latitude -> band ---
latf <- c(file.path(analysis_dir, "data", "SiteLatLon.csv"), file.path(SERVER_DIR, "data", "SiteLatLon.csv"),
          file.path(analysis_dir, "outputs", "claude", "site_response_map_data.csv"))
lf <- latf[file.exists(latf)][1]; if (is.na(lf)) stop("No site latitude file (SiteLatLon.csv or site_response_map_data.csv).")
ll <- read.csv(lf); names(ll) <- tolower(names(ll))
lat <- setNames(ll[[grep("lat", names(ll))[1]]], ll[[grep("site", names(ll))[1]]])
fd$lat <- lat[fd$SITE_ID]
reg_cfg <- read.csv(file.path(analysis_dir, REGION_CONFIG))
assign_band <- function(l) { a <- abs(l)
  reg_cfg$region[vapply(a, function(x) which(x > reg_cfg$abs_lat_min - 1e-9 & x <= reg_cfg$abs_lat_max + 1e-9)[1], integer(1))] }
fd$band <- assign_band(fd$lat)
el <- read.csv(file.path(analysis_dir, "data", "inundation_elasticities.csv")); el <- el[match(reg_cfg$region, el$region), ]

# --- response builders ---
gcellf <- function(cc) { m <- matrix(NA, 3, 3, dimnames = list(MO, TE))
  for (i in 1:3) for (j in 1:3) { v <- fd$norm[!is.na(cc) & cc == MO[i] & !is.na(fd$tc) & fd$tc == TE[j]]
    if (length(v)) m[i, j] <- mean(v, na.rm = TRUE) }; m }
Rfun <- function(sel_band, cc, gcell) { R <- matrix(NA, 3, 3, dimnames = list(MO, TE))
  for (i in 1:3) for (j in 1:3) { sel <- sel_band & !is.na(cc) & cc == MO[i] & !is.na(fd$tc) & fd$tc == TE[j] & is.finite(fd$norm)
    sm <- tapply(fd$norm[sel], fd$SITE_ID[sel], mean, na.rm = TRUE); sm <- sm[is.finite(sm)]
    if (length(sm) >= 1) R[i, j] <- mean(sm) }
  na <- is.na(R); R[na] <- gcell[na]; R }
seedfun <- function(sel_band) { s <- matrix(0, 3, 3, dimnames = list(MO, TE))
  for (i in 1:3) for (j in 1:3) s[i, j] <- sum(sel_band & !is.na(fd$c1) & fd$c1 == MO[i] & !is.na(fd$tc) & fd$tc == TE[j])
  s[s == 0] <- 1; s }
g1 <- gcellf(fd$c1); g48 <- gcellf(fd$c48)
allsel <- rep(TRUE, nrow(fd))
seedG <- seedfun(allsel); F0G <- mean(fd$FCH4_F_ANNOPTLM, na.rm = TRUE)
RG <- list(short = Rfun(allsel, fd$c1, g1), long = Rfun(allsel, fd$c48, g48))
BS <- list()
for (ri in seq_len(nrow(reg_cfg))) { r <- reg_cfg$region[ri]; sel <- fd$band == r & !is.na(fd$band)
  sub <- fd[sel, ]
  BS[[r]] <- list(seed = seedfun(sel), F0 = mean(sub$FCH4_F_ANNOPTLM, na.rm = TRUE),
    sigma = mean(tapply(sub$TA_F, paste(sub$SITE_ID, sub$month), stats::sd, na.rm = TRUE), na.rm = TRUE),
    p_dry0 = mean(sub$c1 == "drought", na.rm = TRUE), p_wet0 = mean(sub$c1 == "extreme_wet", na.rm = TRUE),
    p_cold0 = mean(sub$tc == "cold", na.rm = TRUE), p_hot0 = mean(sub$tc == "hot", na.rm = TRUE),
    R = list(short = Rfun(sel, fd$c1, g1), long = Rfun(sel, fd$c48, g48))) }

# --- projection machinery ---
ipf <- function(seed, rt, ct, it = 60) { J <- seed / sum(seed)
  for (i in seq_len(it)) { J <- J * (rt / rowSums(J)); J <- J * matrix(ct / colSums(J), 3, 3, byrow = TRUE) }; J }
marg <- function(b, LA, dG) { mu <- LA * dG / b$sigma
  c(pd = min(b$p_dry0 * pnorm(-1 + MOISTURE_DRY_SENS * dG) / pnorm(-1), 0.9),
    pw = min(b$p_wet0 * (1 - pnorm(1 - MOISTURE_WET_SENS * dG)) / (1 - pnorm(1)), 0.9),
    pc = min(b$p_cold0 * pnorm(-1 - mu) / pnorm(-1), 0.9),
    ph = min(b$p_hot0  * (1 - pnorm(1 - mu)) / (1 - pnorm(1)), 0.9)) }
areaf <- function(LA, dG, ed, ew) pmin(pmax(exp(ew * (LA * MOISTURE_WET_SENS * dG) - ed * (LA * MOISTURE_DRY_SENS * dG)), AMIN), AMAX)
gwl <- read.csv(file.path(analysis_dir, SSP_GWL_TABLE)); years <- seq(PROJECTION_BASE_YEAR, PROJECTION_END_YEAR)
freq <- read.csv(file.path(analysis_dir, PROJECTION_FREQ_TABLE))

proj_global <- function(ssp, w, full = FALSE) { s <- freq[freq$ssp == ssp, ]; s <- s[order(s$year), ]
  ip <- function(c) approx(s$year, s[[c]], years, rule = 2)$y; pd <- ip("p_dry"); pw <- ip("p_wet"); pc <- ip("p_cold"); ph <- ip("p_hot")
  E <- sapply(seq_along(years), function(t) sum(ipf(seedG, c(pd[t], 1 - pd[t] - pw[t], pw[t]), c(pc[t], 1 - pc[t] - ph[t], ph[t])) * RG[[w]]))
  v <- (E - E[1]) / F0G * BUDGET; if (full) v else v[length(years)] }
proj_regional <- function(ssp, w, varying, full = FALSE) { s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
  g <- approx(s$year, s$gwl, years, rule = 2)$y; dG <- g - GWL_BASELINE; tot <- rep(0, length(years))
  for (ri in seq_len(nrow(reg_cfg))) { r <- reg_cfg$region[ri]; b <- BS[[r]]; LA <- reg_cfg$land_amplification[ri]
    br <- reg_cfg$budget_share[ri] * BUDGET
    Js <- lapply(dG, function(gi) { p <- marg(b, LA, gi)
      ipf(b$seed, c(p["pd"], 1 - p["pd"] - p["pw"], p["pw"]), c(p["pc"], 1 - p["pc"] - p["ph"], p["ph"])) })
    E <- sapply(Js, function(J) sum(J * b$R[[w]])); ff <- 1 + (E - E[1]) / b$F0
    A <- if (varying) areaf(LA, dG, el$e_dry[ri], el$e_wet[ri]) else rep(1, length(years))
    tot <- tot + br * (ff * A - 1) }
  if (full) tot else tot[length(years)] }
# region area-factor trajectory (deterministic, for panel b)
area_traj <- function(ssp, ri) { s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
  dG <- approx(s$year, s$gwl, years, rule = 2)$y - GWL_BASELINE
  areaf(reg_cfg$land_amplification[ri], dG, el$e_dry[ri], el$e_wet[ri]) }

approaches <- c(global = "Global site-scaled", fixed = "Regional, fixed area",
                variable = "Regional, variable inundation")
rows <- list()
for (ap in names(approaches)) for (ssp in unique(gwl$ssp)) {
  sh <- if (ap == "global") proj_global(ssp, "short") else proj_regional(ssp, "short", ap == "variable")
  lo <- if (ap == "global") proj_global(ssp, "long")  else proj_regional(ssp, "long",  ap == "variable")
  rows[[paste(ap, ssp)]] <- data.frame(approach = approaches[[ap]], ssp = ssp,
    short_SPEI1_Tg_yr_2100 = round(sh, 2), long_SPEI48_Tg_yr_2100 = round(lo, 2),
    pct_reduction = round(100 * (1 - lo / sh), 1))
}
out <- do.call(rbind, rows); rownames(out) <- NULL
save_output_csv(out, file.path("diagnostics", "duration_sensitivity_projection.csv"), analysis_dir)

# --- comparison figure: short vs long by approach, per SSP ---
save_output_figure(function() {
  ssps <- unique(gwl$ssp); par(mfrow = c(2, 2), mar = c(3, 4.2, 2.6, 1))
  aps <- names(approaches); col_short <- "#2166ac"; col_long <- "#8c510a"
  for (ssp in ssps) {
    m <- sapply(aps, function(ap) { r <- out[out$ssp == ssp & out$approach == approaches[[ap]], ]
      c(r$short_SPEI1_Tg_yr_2100, r$long_SPEI48_Tg_yr_2100) })
    bp <- barplot(m, beside = TRUE, col = c(col_short, col_long), border = "#333333",
                  names.arg = c("Global", "Fixed", "Variable"), las = 1,
                  ylim = c(min(0, min(m)) * 1.1, max(m) * 1.15))
    abline(h = 0, col = "#888888")
    text(as.vector(bp), as.vector(m), sprintf("%.1f", as.vector(m)), pos = 3, cex = 0.75, xpd = NA)
    title(ssp, adj = 0, font.main = 2, cex.main = 1)
    if (ssp == ssps[1]) legend("topright", c("Short (SPEI1)", "Sustained (SPEI48)"),
                               fill = c(col_short, col_long), border = "#333333", bty = "n", cex = 0.85)
    mtext(expression("Additional CH"[4] * " 2100 (Tg yr"^-1 * ")"), 2, 2.6, cex = 0.75)
  }
}, file.path("diagnostics", "figures", "duration_x_upscaling.png"), analysis_dir, width = 1900, height = 1500, res = 190)

# ---------------------------------------------------------------------------
# Trajectories for the combined headline figure (drawn in 22_Figures.R).
# We overlay SHORT (SPEI1) vs SUSTAINED (SPEI48) drought on the variable-
# inundation projection (script 19c). Rather than draw the PNG here -- which
# would fight 19c for ownership of inundation_varying_projection.png depending
# on run order -- 21d only WRITES the full-annual trajectories and the shared
# area-factor curves; 22_Figures.R (always last in the pipeline) reads them and
# draws the figure.
#
# The short arm is the published 19c headline: we read its Monte-Carlo median
# and 5-95% band from regional_global_projection_inundation.csv and anchor the
# deterministic engine to it per SSP (scalar k, ~1 since the deterministic path
# reproduces the MC median to within rounding), then apply the SAME short->long
# deterministic transform to obtain the sustained-drought arm on the identical
# scale. Panel (b), the inundation area factor, is duration-independent (set by
# warming and the area elasticities), so it is shared by both arms.
# Runs after 19c; if 19c has not been run this block skips cleanly.
# ---------------------------------------------------------------------------
pub_path <- c(file.path(analysis_dir, "outputs", "regional_projection_inundation",
                        "regional_global_projection_inundation.csv"),
              file.path(get0("SERVER_OUTPUTS", ifnotfound = ""),
                        "regional_projection_inundation",
                        "regional_global_projection_inundation.csv"))
pub_path <- pub_path[file.exists(pub_path)][1]
if (is.na(pub_path)) {
  message("Skipping combined inundation figure: run 19c first (published CSV not found).")
} else {
  pub <- read.csv(pub_path, stringsAsFactors = FALSE)
  ssps_f <- unique(pub$ssp); yend <- max(years); nT <- length(years); eps <- 1e-6

  # deterministic short & long full trajectories (variable-inundation approach)
  det <- setNames(lapply(ssps_f, function(ssp) {
    sh <- proj_regional(ssp, "short", TRUE, full = TRUE)
    lg <- proj_regional(ssp, "long",  TRUE, full = TRUE)
    list(short = sh, long = lg, csh = cumsum(sh), clg = cumsum(lg))
  }), ssps_f)

  # per-SSP anchor to the published short arm; carry the transform to the long arm
  anchor <- setNames(lapply(ssps_f, function(ssp) {
    ps <- pub[pub$ssp == ssp & pub$year == yend, ]; ds <- det[[ssp]]
    kR <- if (abs(ds$short[nT]) > eps) ps$additional_Tg_per_yr_median / ds$short[nT] else 1
    kC <- if (abs(ds$csh[nT])   > eps) ps$cumulative_Tg_median      / ds$csh[nT]   else 1
    list(short_rate = ds$short * kR, long_rate = ds$long * kR,
         short_cum  = ds$csh   * kC, long_cum  = ds$clg  * kC)
  }), ssps_f)

  # full-annual trajectories consumed by 22_Figures.R (panels a + c)
  traj <- do.call(rbind, lapply(ssps_f, function(ssp) {
    pb <- pub[pub$ssp == ssp, ]; pb <- pb[order(pb$year), ]; a <- anchor[[ssp]]
    data.frame(ssp = ssp, year = years,
      short_rate = a$short_rate, long_rate = a$long_rate,
      short_rate_lo = approx(pb$year, pb$lo, years, rule = 2)$y,
      short_rate_hi = approx(pb$year, pb$hi, years, rule = 2)$y,
      short_cum = a$short_cum, long_cum = a$long_cum,
      short_cum_lo = approx(pb$year, pb$cumulative_lo, years, rule = 2)$y,
      short_cum_hi = approx(pb$year, pb$cumulative_hi, years, rule = 2)$y)
  }))
  save_output_csv(traj, file.path("diagnostics", "duration_sensitivity_trajectories.csv"), analysis_dir)

  # shared inundation area-factor curves consumed by 22_Figures.R (panel b)
  afac <- do.call(rbind, lapply(ssps_f, function(ssp)
    do.call(rbind, lapply(seq_len(nrow(reg_cfg)), function(ri)
      data.frame(ssp = ssp, region = reg_cfg$region[ri], year = years,
                 area_factor = area_traj(ssp, ri))))))
  save_output_csv(afac, file.path("diagnostics", "duration_area_factor.csv"), analysis_dir)
  message("Wrote duration trajectories + area factors; 22_Figures.R draws inundation_varying_projection.png.")
}

# --- per-continent short vs long contributions at 2100 (variable inundation) ---
# Mirrors the continent split in 19c: continent = sum over bands of the band's
# 2100 response fraction (flux_factor x area - 1) weighted by the continent x band
# allocation, times the global budget. 22_Figures.R uses the short/long ratio here
# to anchor the sustained-drought choropleth to the published short-drought map.
alloc <- read.csv(file.path(analysis_dir, CONTINENT_ALLOC), row.names = 1, check.names = FALSE)
band_frac_2100 <- function(ssp, w) {
  s <- gwl[gwl$ssp == ssp, ]; s <- s[order(s$year), ]
  dG <- approx(s$year, s$gwl, years, rule = 2)$y - GWL_BASELINE
  sapply(seq_len(nrow(reg_cfg)), function(ri) {
    r <- reg_cfg$region[ri]; b <- BS[[r]]; LA <- reg_cfg$land_amplification[ri]
    Js <- lapply(dG, function(gi) { p <- marg(b, LA, gi)
      ipf(b$seed, c(p["pd"], 1 - p["pd"] - p["pw"], p["pw"]),
                  c(p["pc"], 1 - p["pc"] - p["ph"], p["ph"])) })
    E <- sapply(Js, function(J) sum(J * b$R[[w]])); ff <- 1 + (E - E[1]) / b$F0
    A <- areaf(LA, dG, el$e_dry[ri], el$e_wet[ri])
    (ff * A - 1)[length(years)]
  })
}
W <- as.matrix(alloc[, reg_cfg$region])   # continents x bands, ordered to reg_cfg
cont_dur <- do.call(rbind, lapply(unique(gwl$ssp), function(ssp) {
  data.frame(ssp = ssp, continent = rownames(alloc),
             short_Tg_2100 = as.numeric(W %*% band_frac_2100(ssp, "short")) * BUDGET,
             long_Tg_2100  = as.numeric(W %*% band_frac_2100(ssp, "long"))  * BUDGET)
}))
save_output_csv(cont_dur, file.path("diagnostics", "duration_continent_contributions.csv"), analysis_dir)
message("Wrote per-continent short vs long contributions for the fig7 choropleth.")

message("Duration x upscaling sensitivity (additional Tg/yr at 2100):")
print(out, row.names = FALSE)
