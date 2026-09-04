# =====================================================================
# 22_Figures.R

# Manuscript figures (fig1-fig7) for the CH4 x compound extreme-events
# analysis. Faithful base-R ports of the original matplotlib scripts in
# outputs/claude/ (figs.py, projection.py, projection2.py, contmap.py,
# regmap.py), but every number is read from the CSVs produced by the
# proper R pipeline scripts -- NOT from hard-coded literals and NOT from
# the Python-generated duplicates in outputs/claude/.
#
# CSV provenance (all made by proper R scripts):
#   07_ConditionChanges.R        -> outputs/temperature_anomaly_events/
#                                     moisture_by_temp_anomaly_summary.csv
#                                     temp_anomaly_only_methane_terms.csv
#                                     twoway_anova_moisture_x_temp_anomaly.csv
#                                     site_hot_minus_cold_contrast.csv
#   11_Linear.R                  -> outputs/temperature_anomaly_events/
#                                     linear_spei_x_temp_anomaly_terms.csv
#   12_Q10.R                     -> outputs/temperature_anomaly_events/
#                                     q10_by_temp_anomaly.csv
#   09_RF_Importance.R          -> outputs/temperature_events/
#                                     rf_variable_importance.csv
#   15_Compare_FLUXNET_Models.R  -> outputs/fluxnet_model_comparison/
#                                     site_month_matched_deltas_by_spei.csv  (fig5)
#   raw panel (data/fluxes_drought.csv) + data/ssp_global_warming_levels.csv (fig8/fig9)
#   18_ExtremeEmissionsProjection.R -> outputs/extreme_emissions_projection/
#                                     extreme_emissions_projection.csv
#                                     extreme_emissions_projection_uncertainty.csv
#   19_RegionalProjection.R      -> outputs/regional_projection/
#                                     regional_breakdown_2100.csv
#
# Two figure-only tables have no upstream R producer (they were only ever
# written by the Python contmap.py). To honour the "make the CSVs in the
# proper R script" requirement, THIS script builds them in R -- from the
# pipeline's site_hot_minus_cold_contrast.csv plus site coordinates in the
# FLUXNET-CH4 metadata file -- and writes them before plotting:
#   outputs/claude/site_response_map_data.csv
#   outputs/claude/site_response_by_continent.csv
#
# Figures written to outputs/claude/:
#   fig1_compound_grid.png  fig2_marginal_partition.png  fig3_spei_by_temp.png
#   fig4_q10_rf.png  fig5_tem_eval.png  fig6_projection.png
#   fig6b_projection_uncertainty.png  fig6_projection_results.png
#   fig7_continent_map.png  fig7_regional_map.png  fig7_continent_choropleth.png
#   fig8_discussion_trend.png (fig 8)  fig9_future_extension.png (fig 9)
#
# fig7_continent_choropleth.png needs sf + ggplot2 + rnaturalearth (+
# rnaturalearthdata); it pulls clean continent polygons from Natural Earth and
# skips cleanly if those packages are unavailable. All other figures use base R.
#
# fig8/fig9 also have no upstream R producer, so this script computes their
# trend tables in R and writes them alongside the figures:
#   outputs/claude/discussion_state_trends.csv       (observed 2006-2019 trends)
#   outputs/claude/discussion_future_extension.csv   (forward SSP extension)
#
# Not reproduced: the mp-*/dp-*/pg-*/cc/mm/test_* PNGs are throwaway drafts.
# Base R covers every figure except the optional choropleth noted above.
# =====================================================================

# ---------------------------------------------------------------------
# 0. Paths (all relative to the project root that holds this script)
# ---------------------------------------------------------------------
get_script_dir <- function() {
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f)))
  sf <- sys.frames()
  for (i in rev(seq_along(sf))) {
    fn <- sf[[i]]$ofile
    if (!is.null(fn)) return(dirname(normalizePath(fn)))
  }
  getwd()
}

PROJECT <- Sys.getenv("CH4_BASE", unset = get_script_dir())
OUTPUTS <- file.path(PROJECT, "outputs")
TAE  <- file.path(OUTPUTS, "temperature_anomaly_events")   # 07 / 11_Linear / 12_Q10
TE   <- file.path(OUTPUTS, "temperature_events")           # 09
FMC  <- file.path(OUTPUTS, "fluxnet_model_comparison")     # 15 (feeds fig5)
EEP  <- file.path(OUTPUTS, "extreme_emissions_projection") # 18
RP   <- file.path(OUTPUTS, "regional_projection")          # 19 (fixed-area, comparison)
RPI  <- file.path(OUTPUTS, "regional_projection_inundation") # 19c (variable inundation, headline)
CL   <- file.path(OUTPUTS, "claude")                       # figure + generated-CSV output
DATA <- file.path(PROJECT, "data")
OUT  <- CL
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE)

message("PROJECT = ", PROJECT)
message("output  = ", OUT)

rd <- function(...) read.csv(file.path(...), stringsAsFactors = FALSE, check.names = FALSE)

# ---------------------------------------------------------------------
# 1. Shared style (mirrors the matplotlib palette)
# ---------------------------------------------------------------------
TEMP  <- c(cold = "#2166ac", normal = "#cccccc", hot = "#b2182b")            # blue -> red
MOIST <- c(drought = "#8c510a", normal = "#cccccc", extreme_wet = "#35978f") # brown -> teal
CH4_RAMP <- colorRampPalette(c("#8c510a", "#f7f7f7", "#2166ac"))             # brown-white-blue
# Sequential purple ramp (light = low forcing -> dark = high forcing). Purple is
# deliberately not used by the temperature (blue/red), moisture (brown/teal), or
# model (slate/grey) palettes, so the SSP scenarios read as a distinct set.
SSPCOL <- c("SSP1-2.6" = "#c2a5cf", "SSP2-4.5" = "#9970ab",
            "SSP3-7.0" = "#762a83", "SSP5-8.5" = "#40004b")

# two-slope normaliser: vmin->0, vcenter->0.5, vmax->1 (matches TwoSlopeNorm)
twoslope <- function(v, vmin, vcenter, vmax) {
  t <- ifelse(v <= vcenter,
              0.5 * (v - vmin) / (vcenter - vmin),
              0.5 + 0.5 * (v - vcenter) / (vmax - vcenter))
  pmin(pmax(t, 0), 1)
}
ramp_col <- function(v, vmin, vcenter, vmax, ramp = CH4_RAMP, n = 256) {
  pal <- ramp(n)
  pal[round(twoslope(v, vmin, vcenter, vmax) * (n - 1)) + 1]
}
png_open <- function(file, w, h, res = 200) {
  png(file.path(OUT, file), width = w, height = h, units = "in", res = res)
}

# ---------------------------------------------------------------------
# 2. Build the site-level response CSVs IN R (previously Python-only)
#    -> outputs/claude/site_response_map_data.csv
#    -> outputs/claude/site_response_by_continent.csv
# ---------------------------------------------------------------------
# continent from the 2-letter FLUXNET site prefix (matches contmap.py);
# RU is split east/west by longitude.
CC_MAP <- c(US="North America", CA="North America", BR="South America", AR="South America",
            DE="Europe", FI="Europe", CH="Europe", NL="Europe", UK="Europe", IT="Europe",
            SE="Europe", FR="Europe", AT="Europe", CZ="Europe", DK="Europe", ES="Europe",
            CN="Asia", HK="Asia", ID="Asia", MY="Asia", JP="Asia", PH="Asia", KR="Asia",
            BW="Africa", ZA="Africa", NZ="Oceania", AU="Oceania")
site_continent <- function(site_id, lon) {
  cc <- substr(site_id, 1, 2)
  out <- unname(CC_MAP[cc])
  out[cc == "RU"] <- ifelse(lon[cc == "RU"] > 60, "Asia", "Europe")
  out[is.na(out)] <- "Other"
  out
}

build_site_response_csvs <- function() {
  contrast_f <- file.path(TAE, "site_hot_minus_cold_contrast.csv")
  meta_f <- file.path(DATA, "TEM_MDM_SIMULATIONdata",
                      "FLX_AA-Flx_CH4-META_20201112135337801132.csv")
  if (!file.exists(contrast_f)) {
    message("  (skip CSV build: ", basename(contrast_f), " not found; run 07_ConditionChanges.R)")
    return(invisible(FALSE))
  }
  ct <- rd(contrast_f)
  # response column is methane_hot_minus_cold in the R pipeline output
  rc <- intersect(c("methane_hot_minus_cold", "hot_minus_cold"), names(ct))[1]
  res <- data.frame(SITE_ID = ct$SITE_ID, hot_minus_cold = ct[[rc]],
                    stringsAsFactors = FALSE)
  # coordinates from the FLUXNET-CH4 metadata file
  if (!file.exists(meta_f)) {
    message("  (skip CSV build: site metadata file not found)")
    return(invisible(FALSE))
  }
  meta <- rd(meta_f)[, c("SITE_ID", "LAT", "LON")]
  names(meta) <- c("SITE_ID", "lat", "lon")
  res <- merge(res, meta, by = "SITE_ID")
  res <- res[is.finite(res$lat) & is.finite(res$lon) & is.finite(res$hot_minus_cold), ]
  res$continent <- site_continent(res$SITE_ID, res$lon)
  res <- res[, c("SITE_ID", "lat", "lon", "continent", "hot_minus_cold")]
  write.csv(res, file.path(OUT, "site_response_map_data.csv"), row.names = FALSE)

  # per-continent summary (mean, sd, count, se), sorted by mean
  ag <- do.call(rbind, lapply(split(res$hot_minus_cold, res$continent), function(x)
    data.frame(mean = mean(x), std = if (length(x) > 1) sd(x) else NA,
               count = length(x))))
  cont <- data.frame(continent = rownames(ag), ag, row.names = NULL)
  cont$se <- cont$std / sqrt(cont$count)
  cont <- cont[order(cont$mean), ]
  write.csv(cont, file.path(OUT, "site_response_by_continent.csv"), row.names = FALSE)
  message("  built site_response_map_data.csv (", nrow(res),
          " sites) + site_response_by_continent.csv")
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# FIG 1: compound event grid (moisture x temperature)
# ---------------------------------------------------------------------
fig1 <- function() {
  d <- rd(TAE, "moisture_by_temp_anomaly_summary.csv")
  mo <- c("drought", "normal", "extreme_wet")   # columns (x)
  te <- c("cold", "normal", "hot")              # rows (y, top->bottom)
  M <- matrix(NA, 3, 3); N <- matrix(NA, 3, 3)
  for (i in seq_along(te)) for (j in seq_along(mo)) {
    r <- d[d$temp_class == te[i] & d$condition == mo[j], ]
    M[i, j] <- r$normalized_Fch4_mean; N[i, j] <- r$n_obs
  }
  png_open("fig1_compound_grid.png", 5.6, 5.0)
  par(mar = c(4.5, 5, 4, 6), xpd = NA)
  plot(NA, xlim = c(0.5, 3.5), ylim = c(3.5, 0.5), axes = FALSE, xlab = "", ylab = "", asp = 1)
  for (i in 1:3) for (j in 1:3) {
    rect(j - 0.5, i - 0.5, j + 0.5, i + 0.5, col = ramp_col(M[i, j], -8, 0, 17),
         border = "white", lwd = 3)
    v <- M[i, j]
    text(j, i, sprintf("%+.1f\n(n=%s)", v, format(N[i, j], big.mark = ",")),
         col = ifelse(v > 11 | v < -6, "white", "#222222"), font = 2, cex = 0.95)
  }
  rect(1.5, 1.5, 2.5, 2.5, border = "#222222", lwd = 2.2, lty = 2)
  text(2, 1.66, "reference", font = 3, cex = 0.7, col = "#222222")
  xlab <- c("Extreme dry", "Normal", "Extreme wet"); ylab <- c("Cold", "Normal", "Hot")
  xcol <- c(MOIST["drought"], "#888888", MOIST["extreme_wet"])
  ycol <- c(TEMP["cold"], "#888888", TEMP["hot"])
  for (j in 1:3) text(j, 3.75, xlab[j], font = 2, col = xcol[j], cex = 0.95)
  for (i in 1:3) text(0.28, i, ylab[i], font = 2, col = ycol[i], srt = 90, cex = 0.95)
  mtext("Moisture anomaly (SPEI)", side = 1, line = 2.2, font = 2)
  mtext("Temperature anomaly (STI)", side = 2, line = 3.2, font = 2)
 # title("Methane anomaly across (nmol m⁻² s⁻¹)", font.main = 2, cex.main = 1.05, line = 2.2)
  vals <- seq(-8, 17, length.out = 100); by <- seq(0.5, 3.5, length.out = 100)
  for (k in 1:99) rect(3.75, by[k], 3.95, by[k + 1], col = ramp_col(vals[k], -8, 0, 17), border = NA)
  rect(3.75, 0.5, 3.95, 3.5, border = "#888888")
  # y runs 0.5 (bottom) -> 3.5 (top); the colorbar is drawn -8 (bottom) -> +17 (top),
  # so labels must ascend to match (previously reversed, which flipped the legend).
  text(4.2, c(0.5, 2, 3.5), sprintf("%+d", c(-8, 4, 17)), cex = 0.7)
  text(4.5, 2, expression("Mean normalized methane anomaly (nmol " * m^-2 ~ s^-1 * ")"), srt = 90, cex = 0.75)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 2: (a) marginal temperature effect  (b) variance partition
# ---------------------------------------------------------------------
fig2 <- function() {
  tm <- rd(TAE, "temp_anomaly_only_methane_terms.csv")
  pick <- function(pat) tm[grepl(pat, tm$term) & !grepl("condition|SPEI|Intercept", tm$term), ][1, ]
  cold <- pick("cold"); hot <- pick("hot")
  eff <- c(cold = cold$estimate, normal = 0, hot = hot$estimate)
  se  <- c(cold = cold$std.error, normal = 0, hot = hot$std.error)

  aov <- rd(TAE, "twoway_anova_moisture_x_temp_anomaly.csv")
  gss <- function(pat) aov$sumsq[grepl(pat, aov$term)][1]
  ss <- c(gss("^temp_class$"), gss("^condition$"), gss(":"))

  # Larger axis annotation sizes (tick numbers, axis titles, category labels,
  # panel letters, value labels).
  CEX_AXIS <- 1.35; CEX_NAMES <- 1.4; CEX_LAB <- 1.35; CEX_VAL <- 1.2; CEX_PANEL <- 1.5

  png_open("fig2_marginal_partition.png", 9.6, 4.6)
  par(mfrow = c(1, 2), mar = c(4.2, 5.2, 3.2, 1.2), mgp = c(3, 0.8, 0))
  cols <- TEMP[c("cold", "normal", "hot")]
  # Data-driven y-limits (was hardcoded c(-11, 11), which clipped whiskers/labels
  # when the site set changed). Leave headroom for the value labels above the
  # whiskers and for the p-value note near the top.
  lab_off <- 1.1
  ytop <- max(eff + se, na.rm = TRUE); ybot <- min(eff - se, na.rm = TRUE)
  span <- max(ytop - ybot, 1)
  ylim_a <- c(min(0, ybot) - 0.14 * span - lab_off,
              max(0, ytop) + 0.24 * span + lab_off)
  bp <- barplot(eff, col = cols, border = "#333333", ylim = ylim_a,
                names.arg = c("Cold", "Normal", "Hot"), las = 1,
                cex.axis = CEX_AXIS, cex.names = CEX_NAMES)
  abline(h = 0, col = "#999999")
  arrows(bp, eff - se, bp, eff + se, angle = 90, code = 3, length = 0.05, col = "#333333")
  # place labels clear of the error-bar whiskers (beyond eff +/- se)
  text(bp, eff + ifelse(eff >= 0, se + lab_off, -(se + lab_off)), sprintf("%+.1f", eff),
       font = 2, cex = CEX_VAL)
  mtext(expression(bold("Methane anomaly (nmol " * m^-2 ~ s^-1 * ")")), side = 2, line = 3.0,
        font = 2, cex = CEX_LAB)
  title("(a)", adj = 0, font.main = 2, cex.main = CEX_PANEL)
  text(mean(bp), ylim_a[2] - 0.04 * diff(ylim_a), "hot vs cold p<0.001",
       font = 3, cex = 0.95, col = "#555555")

  terms <- c("Temperature\nanomaly", "Moisture\nanomaly", "Interaction")
  par(mar = c(4.6, 8, 3.2, 2.4), mgp = c(3, 0.8, 0))
  # Data-driven x-limit with headroom. Place each value label INSIDE the bar tip
  # when the bar is long enough, otherwise just outside — so the label on the
  # longest bar is never clipped by the panel edge (the earlier pos=4 labels ran
  # off the right side regardless of the fixed xlim).
  xmax <- max(ss, na.rm = TRUE) * 1.18
  bp2 <- barplot(rev(ss), horiz = TRUE, col = "grey70", border = "#333333",
                 names.arg = rev(terms), las = 1, xlim = c(0, xmax),
                 cex.axis = CEX_AXIS, cex.names = CEX_NAMES)
  vals <- rev(ss); labs <- sprintf("%.0fk", vals / 1e3)
  inside <- is.finite(vals) & vals > 0.18 * xmax
  text(x = ifelse(inside, vals - 0.015 * xmax, vals + 0.015 * xmax),
       y = bp2, labels = labs, pos = ifelse(inside, 2, 4),
       font = 2, cex = CEX_VAL, col = "#222222", xpd = NA)
  mtext("Sum of squares (Type-I)", side = 1, line = 2.8, font = 2, cex = CEX_LAB)
  title("(b)", adj = 0, font.main = 2, cex.main = CEX_PANEL)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 3: SPEI response by temperature class (interaction mechanism)
# ---------------------------------------------------------------------
fig3 <- function() {
  tm <- rd(TAE, "linear_spei_x_temp_anomaly_terms.csv")
  est_where <- function(cond) tm$estimate[cond][1]
  is_spei <- grepl("SPEI", tm$term); is_cold <- grepl("cold", tm$term); is_hot <- grepl("hot", tm$term)
  slope_norm <- est_where(is_spei & !is_cold & !is_hot)                 # SPEI1 main effect
  b <- c(cold = est_where(!is_spei & is_cold), normal = 0, hot = est_where(!is_spei & is_hot))
  m <- c(cold = slope_norm + est_where(is_spei & is_cold),
         normal = slope_norm,
         hot = slope_norm + est_where(is_spei & is_hot))
  spei <- seq(-2.5, 2.5, length.out = 100)
  yl <- range(sapply(c("cold", "normal", "hot"), function(c) b[c] + m[c] * spei))

  png_open("fig3_spei_by_temp.png", 6.4, 4.6)
  par(mar = c(4.2, 4.5, 3, 1))
  plot(NA, xlim = c(-2.5, 2.5), ylim = yl, xlab = "", ylab = "", las = 1)
  rect(-2.5, yl[1], -1, yl[2], col = adjustcolor(MOIST["drought"], 0.14), border = NA)
  rect(1, yl[1], 2.5, yl[2], col = adjustcolor(MOIST["extreme_wet"], 0.14), border = NA)
  abline(h = 0, v = 0, col = "#bbbbbb")
  for (c in c("cold", "normal", "hot")) lines(spei, b[c] + m[c] * spei, col = TEMP[c], lwd = 3)
  ybot <- yl[1] + 0.04 * diff(yl)   # anchor the band labels to the bottom of the panel
  text(-1.75, ybot, "drier", col = MOIST["drought"], font = 2, cex = 0.9)
  text(1.75, ybot, "wetter", col = MOIST["extreme_wet"], font = 2, cex = 0.9)
  mtext("Moisture anomaly (SPEI)", side = 1, line = 2.5, font = 2)
  mtext(expression(bold("Methane anomaly (nmol " * m^-2 ~ s^-1 * ")")), side = 2, line = 2.6, font = 2)
  title("", font.main = 2, cex.main = 1.1)
  legend("topright", bty = "n", cex = 0.95, lwd = 3, col = TEMP[c("cold", "normal", "hot")],
         legend = sprintf("%s (slope %+.1f)", c("Cold", "Normal", "Hot"),
                          m[c("cold", "normal", "hot")]))
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 4: (a) Q10 stability  (b) random-forest importance
#        (c) drought response vs SPEI accumulation window (duration)
#        (d) duration x temperature (short vs multi-year drought by temp class)
# Q10 (a) and duration x temperature (d) bars use the temperature palette (cold
# blue, normal grey, hot red); panel titles omitted (only the (a)-(d) letters).
# Panels (c)-(d) use script-14's drought deltas and,
# for the temperature stratification, the multi-window SPEI in data/fluxes_drought.csv
# restricted to the canonical analysis sites (data/DroughtAnalysis.RDATA).
# ---------------------------------------------------------------------
fig4 <- function() {
  BAR <- "grey85"; FSL <- file.path(OUTPUTS, "fluxnet_short_long_drought")

  q <- rd(TAE, "q10_by_temp_anomaly.csv")
  q <- q[match(c("cold", "normal", "hot"), q$temp_class), ]
  q10 <- q$Q10_mean; qse <- q$Q10_sd / sqrt(q$n_obs)

  rf <- rd(TE, "rf_variable_importance.csv")
  imp_col <- intersect(c("%IncMSE", "X.IncMSE", "pct_inc_mse"), names(rf))[1]
  lab_col <- intersect(c("label", "variable"), names(rf))[1]
  rf <- rf[order(rf[[imp_col]]), ]
  temp_lab <- c("Air temperature", "Thermal season (winter-summer)", "Temp anomaly (hot-cold)")
  is_temp <- rf[[lab_col]] %in% temp_lab
  lab <- rf[[lab_col]]
  lab <- sub(" \\(winter-summer\\)", "", lab); lab <- sub(" \\(hot-cold\\)", "", lab)

  # canonical analysis site set (43 sites) from the shared table
  keep <- NULL
  da <- file.path(DATA, "DroughtAnalysis.RDATA")
  if (file.exists(da)) { e <- new.env(); load(da, envir = e)
    if (!is.null(e$fluxes.drought_normalized))
      keep <- unique(as.character(e$fluxes.drought_normalized$SITE_ID)) }

  # (c) drought delta by SPEI accumulation window (script 14)
  cwin <- NULL
  fslf <- file.path(FSL, "site_month_matched_condition_deltas_by_spei_window.csv")
  if (file.exists(fslf)) {
    dw <- rd(FSL, "site_month_matched_condition_deltas_by_spei_window.csv")
    dw <- dw[dw$condition == "drought", ]
    if (!is.null(keep)) dw <- dw[dw$SITE_ID %in% keep, ]
    WINv <- c(SPEI1 = 1, SPEI3 = 3, SPEI6 = 6, SPEI12 = 12, SPEI24 = 24, SPEI36 = 36, SPEI48 = 48)
    cwin <- do.call(rbind, lapply(names(WINv), function(w) {
      v <- dw$delta_normalized_Fch4[dw$Drought.IDX == w]; v <- v[is.finite(v)]
      data.frame(mo = WINv[[w]], mean = mean(v), se = stats::sd(v) / sqrt(length(v))) }))
  }

  # (d) duration x temperature: drought - near-normal delta of normalized CH4 by
  # temp class at a short (SPEI1) vs multi-year (SPEI48) accumulation window.
  dt <- NULL
  fdf <- file.path(DATA, "fluxes_drought.csv")
  if (file.exists(fdf)) {
    fd <- rd(DATA, "fluxes_drought.csv")
    if (!is.null(keep)) fd <- fd[fd$SITE_ID %in% keep, ]
    fd <- fd[is.finite(fd$FCH4_F_ANNOPTLM) & is.finite(fd$TA_F), ]
    nn <- fd[is.finite(fd$SPEI1) & fd$SPEI1 > -0.5 & fd$SPEI1 < 0.5, ]
    fnorm <- tapply(nn$FCH4_F_ANNOPTLM, nn$SITE_ID, mean, na.rm = TRUE)
    fd <- fd[fd$SITE_ID %in% names(fnorm), ]
    fd$norm <- fd$FCH4_F_ANNOPTLM - fnorm[fd$SITE_ID]
    key <- paste(fd$SITE_ID, fd$month, sep = "@@")
    mu <- tapply(fd$TA_F, key, mean, na.rm = TRUE); sdv <- tapply(fd$TA_F, key, stats::sd, na.rm = TRUE)
    nk <- tapply(fd$TA_F, key, function(x) sum(is.finite(x)))
    okv <- is.finite(sdv[key]) & sdv[key] > 0 & nk[key] >= 5
    sti <- ifelse(okv, (fd$TA_F - mu[key]) / sdv[key], NA)
    fd$tc <- ifelse(is.na(sti), NA, ifelse(sti <= -1, "cold", ifelse(sti >= 1, "hot", "normal")))
    delta_by <- function(w, tc) {
      sub <- fd[!is.na(fd$tc) & fd$tc == tc, ]
      dr <- sub[is.finite(sub[[w]]) & sub[[w]] <= -1, ]
      nm <- sub[is.finite(sub$SPEI1) & sub$SPEI1 > -0.5 & sub$SPEI1 < 0.5, ]
      drm <- tapply(dr$norm, dr$SITE_ID, mean, na.rm = TRUE)
      nmm <- tapply(nm$norm, nm$SITE_ID, mean, na.rm = TRUE)
      cm <- intersect(names(drm)[is.finite(drm)], names(nmm)[is.finite(nmm)])
      d <- drm[cm] - nmm[cm]; c(mean = mean(d), se = stats::sd(d) / sqrt(length(d))) }
    tcs <- c("cold", "normal", "hot")
    dt <- list(short = sapply(tcs, function(t) delta_by("SPEI1", t)),
               long  = sapply(tcs, function(t) delta_by("SPEI48", t)))
  }

  CEXA <- 1.05; CEXL <- 1.1; CEXP <- 1.4; CEXV <- 1.0
  png_open("fig4_q10_rf.png", 10, 8.4)
  par(mfrow = c(2, 2))

  # (a) Q10 stability (light grey bars, labels above)
  par(mar = c(3.6, 5, 3, 1), mgp = c(3, 0.7, 0))
  bp <- barplot(q10, col = TEMP[c("cold", "normal", "hot")], border = "#333333",
                ylim = c(0, max(q10 + qse) * 1.18),
                names.arg = c("Cold", "Normal", "Hot"), las = 1, cex.axis = CEXA, cex.names = CEXA)
  arrows(bp, q10 - qse, bp, q10 + qse, angle = 90, code = 3, length = 0.05, col = "#333333")
  text(bp, q10 + qse + 0.12, sprintf("%.2f", q10), font = 2, cex = CEXV)
  mtext(expression(bold("Temperature sensitivity Q"[10])), side = 2, line = 3, cex = CEXL)
  title("(a)", adj = 0, font.main = 2, cex.main = CEXP)

  # (b) random-forest importance
  par(mar = c(4, 9.5, 3, 1.5), mgp = c(3, 0.7, 0))
  cols_rf <- ifelse(is_temp, "#b2182b", "grey55"); y <- seq_len(nrow(rf))
  plot(NA, xlim = c(0, max(rf[[imp_col]]) * 1.05), ylim = c(0.5, length(y) + 0.5),
       axes = FALSE, xlab = "", ylab = "")
  segments(0, y, rf[[imp_col]], y, col = cols_rf, lwd = 2.5)
  points(rf[[imp_col]], y, pch = 19, col = cols_rf, cex = 1.15)
  axis(1, cex.axis = CEXA)
  for (k in y) axis(2, at = k, labels = lab[k], las = 1, tick = FALSE,
                    col.axis = cols_rf[k], font.axis = ifelse(is_temp[k], 2, 1), cex.axis = 0.85)
  mtext("% increase in MSE", side = 1, line = 2.4, font = 2, cex = CEXL)
  title("(b)", adj = 0, font.main = 2, cex.main = CEXP)

  # (c) response vs duration
  par(mar = c(4.2, 5, 3, 1), mgp = c(3, 0.7, 0))
  if (!is.null(cwin)) {
    x <- log2(cwin$mo); ylc <- range(c(cwin$mean - cwin$se, cwin$mean + cwin$se)) * 1.15
    plot(NA, xlim = c(min(x) - 0.15, max(x) + 0.35), ylim = ylc, axes = FALSE, xlab = "", ylab = "")
    abline(h = 0, col = "#888888")
    arrows(x, cwin$mean - cwin$se, x, cwin$mean + cwin$se, angle = 90, code = 3, length = 0.04, col = "#333333")
    lines(x, cwin$mean, col = "#333333", lwd = 2)
    # match Fig 1 emission colors: high/positive = blue, low/negative = brown
    points(x, cwin$mean, pch = 21, bg = ifelse(cwin$mean >= 0, "#2166ac", "#8c510a"), col = "#333333", cex = 1.5)
    axis(1, at = x, labels = cwin$mo, cex.axis = CEXA); axis(2, las = 1, cex.axis = CEXA)
    mtext("SPEI accumulation window (months)", side = 1, line = 2.4, font = 2, cex = CEXL)
    mtext(expression(bold("Drought " * Delta * "CH"[4] * " (nmol m"^-2 * " s"^-1 * ")")), side = 2, line = 3, cex = CEXL)
  } else { plot.new(); text(0.5, 0.5, "duration data unavailable\n(run 14_FLUXNET_ShortLongDrought.R)", cex = 0.9) }
  title("(c)", adj = 0, font.main = 2, cex.main = CEXP)

  # (d) duration x temperature
  par(mar = c(3.6, 5, 3, 1), mgp = c(3, 0.7, 0))
  if (!is.null(dt)) {
    sm <- dt$short["mean", ]; se_s <- dt$short["se", ]; lm_ <- dt$long["mean", ]; se_l <- dt$long["se", ]
    yld <- range(c(sm - se_s, lm_ - se_l, sm + se_s, lm_ + se_l)) * 1.15
    M <- rbind(sm, lm_)
    # colour bars by temperature class (cold blue, normal grey, hot red); short =
    # solid, multi-year = lighter fill + hatch in the same colour.
    solid <- TEMP[c("cold", "normal", "hot")]
    light <- sapply(solid, function(h) grDevices::adjustcolor(h, 0.45))
    barcols <- as.vector(rbind(solid, light))              # cold(s,l), normal(s,l), hot(s,l)
    bp2 <- barplot(M, beside = TRUE, col = barcols, border = "#333333", names.arg = c("Cold", "Normal", "Hot"),
                   las = 1, ylim = yld, cex.axis = CEXA, cex.names = CEXA)
    barplot(rbind(rep(NA, 3), lm_), beside = TRUE, col = as.vector(rbind(rep(NA, 3), solid)),
            density = 18, angle = 45, border = NA, add = TRUE, axes = FALSE, names.arg = rep("", 3))
    abline(h = 0, col = "#888888")
    arrows(bp2[1, ], sm - se_s, bp2[1, ], sm + se_s, angle = 90, code = 3, length = 0.04, col = "#333333")
    arrows(bp2[2, ], lm_ - se_l, bp2[2, ], lm_ + se_l, angle = 90, code = 3, length = 0.04, col = "#333333")
    mtext(expression(bold("Drought " * Delta * "CH"[4] * " (nmol m"^-2 * " s"^-1 * ")")), side = 2, line = 3, cex = CEXL)
    mtext("Temperature anomaly class", side = 1, line = 2.4, font = 2, cex = CEXL)
    legend("bottomleft", c("Short (1 mo)", "Multi-year (48 mo)"), fill = c("grey55", grDevices::adjustcolor("grey55", 0.45)),
           density = c(NA, 18), angle = 45, border = "#333333", bty = "n", cex = 0.85)
  } else { plot.new(); text(0.5, 0.5, "duration x temperature data unavailable\n(needs data/fluxes_drought.csv)", cex = 0.9) }
  title("(d)", adj = 0, font.main = 2, cex.main = CEXP)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 5: TEM-MDM process-model evaluation against FLUXNET-CH4
#        (self-contained here in fig5(); base-R, no ggplot)
#   (a) mean drought response (delta normalized CH4 vs normal), obs vs model
#   (b) direction of the site-level drought response (diverging bars) with the
#       model-vs-observation sign-agreement annotated. Both panels use only the
#       sites where FLUXNET and the model overlap at the primary SPEI window.
# ---------------------------------------------------------------------
lighten <- function(hex, amount = 0.55) {
  m <- col2rgb(hex) / 255; l <- m + (1 - m) * amount; rgb(l[1, ], l[2, ], l[3, ])
}
txt_for <- function(hex) {
  m <- col2rgb(hex) / 255; lum <- 0.299 * m[1, ] + 0.587 * m[2, ] + 0.114 * m[3, ]
  ifelse(lum < 0.5, "white", "grey15")
}
fig5 <- function() {
  SPEI_WINDOW <- "SPEI1"; MODEL_NAME <- "TEM-MDM"; MODEL_SCENARIO <- "modelssm"
  f <- file.path(FMC, "site_month_matched_deltas_by_spei.csv")
  if (!file.exists(f)) {
    message("  (skip fig5: ", basename(f), " not found; run 15_Compare_FLUXNET_Models.R)")
    return(invisible(FALSE))
  }
  dall <- rd(f)
  d <- dall[dall$Drought.IDX == SPEI_WINDOW & dall$condition == "drought" & !is.na(dall$delta_response), ]
  obs <- d[d$source == "FLUXNET", c("SITE_ID", "delta_response")]
  mod <- d[d$source == MODEL_NAME & d$scenario == MODEL_SCENARIO, c("SITE_ID", "delta_response")]
  names(obs)[2] <- "obs"; names(mod)[2] <- "mod"
  paired <- merge(obs, mod, by = "SITE_ID")            # overlapping sites only
  n_overlap <- length(unique(paired$SITE_ID))          # accurate count of overlapping sites
  sign_agree <- round(100 * mean(sign(paired$obs) == sign(paired$mod)))
  col_obs <- "#2C3E50"; col_model <- "grey"; col_modln <- "grey55"
  means <- c(mean(paired$obs), mean(paired$mod))

  # (c) obs-vs-model drought response by SPEI accumulation window (duration)
  WINc <- c(SPEI1 = 1, SPEI3 = 3, SPEI6 = 6, SPEI12 = 12, SPEI24 = 24, SPEI36 = 36, SPEI48 = 48)
  ser <- function(src, scen) do.call(rbind, lapply(names(WINc), function(w) {
    v <- dall$delta_response[dall$Drought.IDX == w & dall$condition == "drought" &
                             dall$source == src & (is.na(scen) | dall$scenario == scen)]
    v <- v[is.finite(v)]
    data.frame(mo = WINc[[w]], mean = mean(v), se = stats::sd(v) / sqrt(length(v))) }))
  so <- ser("FLUXNET", NA); smo <- ser(MODEL_NAME, MODEL_SCENARIO)

  png_open("fig5_tem_eval.png", 11, 9)
  layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE), widths = c(1, 1.25), heights = c(1, 1))
  # (a) mean drought response
  par(mar = c(3.5, 5, 3.5, 1))
  ylim_a <- max(abs(means)) * 1.35 * c(-1, 1)
  bp <- barplot(means, col = c(col_obs, col_model), border = NA, ylim = ylim_a,
                names.arg = c("FLUXNET", MODEL_NAME), las = 1)
  abline(h = 0, lty = 2, col = "grey55")
  text(bp, means + ifelse(means >= 0, 1, -1) * diff(ylim_a) * 0.05,
       sprintf("%+.1f", means), font = 2, cex = 1.1)
  text(bp, ifelse(means >= 0, -1, 1) * diff(ylim_a) * 0.04, paste0("n = ", n_overlap),
       col = "grey30", cex = 0.85)
  mtext(expression(bold(Delta*" normalized CH"[4]*" under drought")), 2, 3, cex = 0.95)
  title("(a)", adj = 0, font.main = 2, cex.main = 1.05)
  # (b) direction of site-level response (diverging bars)
  par(mar = c(5, 6, 3.5, 1))
  plot(NA, xlim = c(-100, 100), ylim = c(0.5, 2.5), axes = FALSE, xlab = "", ylab = "")
  rows <- list(list(y = 2, v = paired$obs, base = col_obs, lab = "FLUXNET"),
               list(y = 1, v = paired$mod, base = col_model, lab = MODEL_NAME))
  for (rw in rows) {
    n <- length(rw$v); hi <- 100 * sum(rw$v > 0) / n; lo <- 100 * sum(rw$v < 0) / n
    llo <- lighten(rw$base)
    rect(0, rw$y - 0.31, hi, rw$y + 0.31, col = rw$base, border = "white")
    rect(-lo, rw$y - 0.31, 0, rw$y + 0.31, col = llo, border = "white")
    # label inside the segment when wide enough, else just outside the bar end
    if (hi >= 10) text(hi / 2, rw$y, sprintf("%.0f%%", hi), col = txt_for(rw$base), font = 2, cex = 1)
    else          text(hi + 1, rw$y, sprintf("%.0f%%", hi), col = "grey15", font = 2, cex = 1, pos = 4)
    if (lo >= 10) text(-lo / 2, rw$y, sprintf("%.0f%%", lo), col = txt_for(llo), font = 2, cex = 1)
    else          text(-lo - 1, rw$y, sprintf("%.0f%%", lo), col = "grey15", font = 2, cex = 1, pos = 2)
  }
  abline(v = 0, col = "grey40")
  axis(1, at = seq(-100, 100, 25), labels = abs(seq(-100, 100, 25)))
  axis(2, at = c(2, 1), labels = c("FLUXNET", MODEL_NAME), las = 1, tick = FALSE)
  mtext("Share of sites (%)", 1, 2.3, cex = 0.9)
  mtext("Lower  <-  |  ->  Higher   (lighter = lower)", 1, 3.5, cex = 0.72)
  title("(b)", adj = 0, font.main = 2, cex.main = 1.05)
  #mtext(sprintf("Model-observation sign agreement: %d%% (n = %d overlapping sites)",
  #              sign_agree, n_overlap), 3, 0.2, cex = 0.8, col = "grey30")

  # (c) drought response vs duration: FLUXNET vs model at each SPEI window
  par(mar = c(4.2, 5, 3.5, 1), mgp = c(3, 0.7, 0))
  x <- log2(WINc)
  yl <- range(c(so$mean - so$se, so$mean + so$se, smo$mean - smo$se, smo$mean + smo$se), na.rm = TRUE)
  plot(NA, xlim = c(min(x) - 0.15, max(x) + 0.2), ylim = yl, axes = FALSE, xlab = "", ylab = "")
  abline(h = 0, lty = 2, col = "grey55")
  arrows(x, smo$mean - smo$se, x, smo$mean + smo$se, angle = 90, code = 3, length = 0.04, col = col_modln)
  lines(x, smo$mean, col = col_modln, lwd = 2.5)
  points(x, smo$mean, pch = 22, bg = col_model, col = "#333333", cex = 1.5)
  arrows(x, so$mean - so$se, x, so$mean + so$se, angle = 90, code = 3, length = 0.04, col = col_obs)
  lines(x, so$mean, col = col_obs, lwd = 2.5)
  points(x, so$mean, pch = 21, bg = col_obs, col = "#333333", cex = 1.5)
  axis(1, at = x, labels = WINc); axis(2, las = 1)
  mtext("SPEI accumulation window (months)", 1, 2.5, font = 2, cex = 0.95)
  mtext(expression(bold(Delta*" normalized CH"[4]*" under drought")), 2, 3, cex = 0.95)
  legend("bottomleft", c("FLUXNET", MODEL_NAME), lwd = 2.5, pch = c(21, 22),
         col = c(col_obs, col_modln), pt.bg = c(col_obs, col_model), pt.cex = 1.3, bty = "n", cex = 0.95)
  title("(c)", adj = 0, font.main = 2, cex.main = 1.05)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 6: projected additional wetland CH4 - variable inundation (headline)
#        vs fixed area (comparison). Source: 19c regional_projection_inundation/
#        (median + 5-95%) and 19 regional_projection/ (fixed-area median).
#   (a) additional Tg/yr to 2100 by SSP: variable median + 5-95% band, fixed dashed
#   (b) additional Tg/yr at 2100 by SSP: variable vs fixed bars (variable 5-95%)
# ---------------------------------------------------------------------
fig6 <- function() {
  vf <- file.path(RPI, "regional_global_projection_inundation.csv")
  ff <- file.path(RP,  "regional_global_projection.csv")
  if (!file.exists(vf)) {
    message("  (skip fig6: ", basename(vf), " not found; run 19c)"); return(invisible(FALSE)) }
  pv <- read.csv(vf, stringsAsFactors = FALSE, check.names = FALSE)
  pf <- if (file.exists(ff)) read.csv(ff, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  ssps <- intersect(names(SSPCOL), unique(pv$ssp))
  png_open("fig6_projection.png", 10, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))
  # (a) rate over time: variable median + 5-95% band; fixed area dashed
  yhi <- max(pv$hi, 0, na.rm = TRUE)
  plot(NA, xlim = range(pv$year), ylim = range(0, yhi), xlab = "", ylab = "", las = 1)
  for (s in ssps) { d <- pv[pv$ssp == s, ]
    polygon(c(d$year, rev(d$year)), c(d$lo, rev(d$hi)), col = adjustcolor(SSPCOL[s], 0.15), border = NA) }
  for (s in ssps) { d <- pv[pv$ssp == s, ]
    lines(d$year, d$additional_Tg_per_yr_median, col = SSPCOL[s], lwd = 2.4) }
  if (!is.null(pf)) for (s in ssps) { d <- pf[pf$ssp == s, ]
    lines(d$year, d$additional_Tg_per_yr_median, col = SSPCOL[s], lwd = 1.5, lty = 2) }
  abline(h = 0, col = "#999999")
  mtext("Year", 1, 2.4, font = 2)
  mtext(expression(bold("Additional wetland CH"[4]*" (Tg yr"^-1*")")), 2, 2.8)
  title("(a)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.8)
  legend("bottomright", legend = c("variable inundation", "fixed area"),
         lwd = c(2.4, 1.5), lty = c(1, 2), col = "#555555", bty = "n", cex = 0.72)
  # (b) 2100 additional rate: variable vs fixed grouped bars (variable 5-95% whiskers)
  yr2100 <- max(pv$year)
  v2 <- pv[pv$year == yr2100, ]; v2 <- v2[match(ssps, v2$ssp), ]
  f2 <- if (!is.null(pf)) { z <- pf[pf$year == yr2100, ]; z[match(ssps, z$ssp), ] } else NULL
  M <- rbind(v2$additional_Tg_per_yr_median,
             if (!is.null(f2)) f2$additional_Tg_per_yr_median else rep(NA_real_, length(ssps)))
  colnames(M) <- ssps
  cols_b <- as.vector(rbind(unname(SSPCOL[ssps]), rep("#cccccc", length(ssps))))
  par(mar = c(6.5, 5, 3, 1))
  bp <- barplot(M, beside = TRUE, col = cols_b, border = "#333", las = 2,
                names.arg = ssps, ylim = c(0, max(v2$hi, M, na.rm = TRUE) * 1.15))
  arrows(bp[1, ], v2$lo, bp[1, ], v2$hi, angle = 90, code = 3, length = 0.03, col = "#333")
  mtext(expression(bold("Additional CH"[4]*" at 2100 (Tg yr"^-1*")")), 2, 2.8)
  title("(b)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", legend = c("variable inundation (SSP color)", "fixed area"),
         fill = c("#762a83", "#cccccc"), border = "#333", bty = "n", cex = 0.72)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 6b / results: projection with Monte-Carlo 5-95% uncertainty
# ---------------------------------------------------------------------
fig6b <- function(outfile = "fig6b_projection_uncertainty.png") {
  p <- rd(EEP, "extreme_emissions_projection_uncertainty.csv")
  ssps <- intersect(names(SSPCOL), unique(p$ssp))
  png_open(outfile, 10.2, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))
  plot(NA, xlim = range(p$year), ylim = range(0, p$additional_Tg_per_yr_hi),
       xlab = "", ylab = "", las = 1)
  for (s in ssps) { d <- p[p$ssp == s, ]
    polygon(c(d$year, rev(d$year)), c(d$additional_Tg_per_yr_lo, rev(d$additional_Tg_per_yr_hi)),
            col = adjustcolor(SSPCOL[s], 0.15), border = NA) }
  for (s in ssps) { d <- p[p$ssp == s, ]
    lines(d$year, d$additional_Tg_per_yr_median, col = SSPCOL[s], lwd = 2.4) }
  abline(h = 0, col = "#999999")
  mtext("Year", 1, 2.4, font = 2)
  mtext(expression(bold("Additional wetland CH"[4]*" (Tg yr"^-1*")")), 2, 2.8)
  title("(a)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  r <- p[p$year == max(p$year), ]; r <- r[match(ssps, r$ssp), ]
  cm <- r$cumulative_Tg_median
  clo <- r[[intersect(c("cumulative_Tg_lo", "cumulative_lo"), names(r))[1]]]
  chi <- r[[intersect(c("cumulative_Tg_hi", "cumulative_hi"), names(r))[1]]]
  par(mar = c(6.5, 5, 3, 1))   # extra bottom room for the vertical SSP labels
  bp <- barplot(cm, col = SSPCOL[ssps], border = "#333", names.arg = ssps, las = 2,
                ylim = c(0, max(chi) * 1.12))
  arrows(bp, clo, bp, chi, angle = 90, code = 3, length = 0.04, col = "#333")
  text(bp, chi + max(chi) * 0.03, sprintf("%.0f", cm), font = 2)
  mtext(expression(bold("Cumulative extra CH"[4]*" 2020-2100 (Tg)")), 2, 2.8)
  title("(b)", adj = 0, font.main = 2, cex.main = 1)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 7 (continent): site map of hot-cold response + per-continent bars
# ---------------------------------------------------------------------
fig7_continent <- function() {
  s  <- rd(OUT, "site_response_map_data.csv")
  cs <- rd(OUT, "site_response_by_continent.csv")
  cs <- cs[order(cs$mean), ]
  CONTCOL <- c("North America"="#4C78A8", "South America"="#72B7B2", "Europe"="#54A24B",
               "Asia"="#E45756", "Africa"="#EECA3B", "Oceania"="#B279A2", "Other"="#888888")
  png_open("fig7_continent_map.png", 12, 5.4)
  layout(matrix(c(1, 2), 1, 2), widths = c(2.4, 1))
  par(mar = c(4, 4, 3, 5), xpd = NA)
  plot(NA, xlim = c(-172, 182), ylim = c(-58, 82), xlab = "", ylab = "", las = 1)
  rect(-172, -58, 182, 82, col = "#eef3f6", border = NA)
  abline(h = seq(-60, 90, 30), col = "#cdd6dc"); abline(v = seq(-150, 180, 30), col = "#cdd6dc")
  abline(h = c(0, 23.5, -23.5), col = "#b9c4cb", lty = 2)
  points(s$lon, s$lat, pch = 21, bg = ramp_col(s$hot_minus_cold, -30, 0, 30),
         col = "#333", cex = 1.2, lwd = 0.5)
  labs <- list("North America"=c(-100,55), "South America"=c(-60,-15), "Europe"=c(15,60),
               "Africa"=c(20,0), "Asia"=c(100,62), "Oceania"=c(170,-40))
  for (nm in names(labs)) if (nm %in% s$continent)
    text(labs[[nm]][1], labs[[nm]][2], nm, col = "#555", font = 3, cex = 0.8)
  mtext("Longitude", 1, 2.4, font = 2); mtext("Latitude", 2, 2.6, font = 2)
  title("(a)", adj = 0, cex.main = 1, font.main = 2)
  vv <- seq(-30, 30, length.out = 100); yb <- seq(-50, 40, length.out = 100)
  for (k in 1:99) rect(192, yb[k], 202, yb[k + 1], col = ramp_col(vv[k], -30, 0, 30), border = NA)
  text(210, c(-50, 40), c("-30", "+30"), cex = 0.7)
  par(mar = c(4, 8, 3, 2), xpd = FALSE)
  cols_b <- CONTCOL[cs$continent]; cols_b[is.na(cols_b)] <- "#888"
  se <- cs$se; se[is.na(se)] <- 0
  bp <- barplot(cs$mean, horiz = TRUE, col = cols_b, border = "#333",
                names.arg = sprintf("%s\n(n=%d)", cs$continent, cs$count), las = 1,
                xlim = range(c(cs$mean - se, cs$mean + se, 0)) * 1.1, cex.names = 0.75)
  arrows(cs$mean - se, bp, cs$mean + se, bp, angle = 90, code = 3, length = 0.03, col = "#333")
  abline(v = 0, col = "#888")
  mtext(expression("Mean hot - cold normalized FCH"[4]), 1, 2.5, font = 2, cex = 0.9)
  title("(b)", adj = 0, cex.main = 1, font.main = 2)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 7 (regional): latitude-band map + grouped regional bars by SSP
# ---------------------------------------------------------------------
fig7_regional <- function() {
  # headline = variable-inundation band breakdown (19c); fixed-area (19) fallback
  rb_f <- file.path(RPI, "regional_breakdown_2100.csv")
  if (!file.exists(rb_f)) rb_f <- file.path(RP, "regional_breakdown_2100.csv")
  rb <- read.csv(rb_f, stringsAsFactors = FALSE, check.names = FALSE)
  share_col <- intersect(c("budget_share", "share"), names(rb))[1]
  sites_f <- file.path(OUT, "site_response_map_data.csv")
  sites <- if (file.exists(sites_f)) rd(sites_f) else NULL
  band <- function(lat) ifelse(abs(lat) <= 23.5, "Tropical", ifelse(abs(lat) <= 50, "Temperate", "Boreal"))
  if (!is.null(sites)) sites$region <- band(sites$lat)
  SSPS <- intersect(names(SSPCOL), unique(rb$ssp)); MAP_SSP <- "SSP2-4.5"
  if (!(MAP_SSP %in% SSPS)) MAP_SSP <- SSPS[1]
  REGCOL <- c("Tropical"="#c2843b", "Temperate"="#4C9A6B", "Boreal"="#3b7bb0")
  bands <- list(Boreal = list(c(50, 83), c(-83, -50)),
                Temperate = list(c(23.5, 50), c(-50, -23.5)), Tropical = list(c(-23.5, 23.5)))
  mrow <- function(r) rb[rb$ssp == MAP_SSP & rb$region == r, ]

  png_open("fig7_regional_map.png", 13, 5.6)
  layout(matrix(c(1, 2), 1, 2), widths = c(2.6, 1))
  par(mar = c(4, 4, 3, 5), xpd = NA)
  plot(NA, xlim = c(-172, 182), ylim = c(-60, 84), xlab = "", ylab = "", las = 1)
  for (r in names(bands)) {
    col <- ramp_col(mrow(r)$additional_Tg_per_yr_2100, -2, 0, 2)
    for (sp in bands[[r]]) rect(-172, sp[1], 182, sp[2], col = col, border = NA)
  }
  abline(h = 0, col = "#9fb0b8", lty = 2)
  if (!is.null(sites)) points(sites$lon, sites$lat, pch = 21, bg = "#222", col = "white",
                              cex = 0.9, lwd = 0.4)
  lab_y <- c(Boreal = 63, Temperate = 37, Tropical = 0)
  for (r in names(lab_y)) {
    v <- mrow(r); nsit <- if ("n_sites" %in% names(v)) v$n_sites else
      if (!is.null(sites)) sum(sites$region == r) else NA
    text(-165, lab_y[r], adj = c(0, 0.5), cex = 0.72,
         sprintf("%s\nbudget %d%%   n=%s sites\n%+.1f Tg/yr [%.1f, %.1f]",
                 r, round(v[[share_col]] * 100), nsit,
                 v$additional_Tg_per_yr_2100, v$lo, v$hi))
  }
  mtext("Longitude", 1, 2.4, font = 2); mtext("Latitude", 2, 2.6, font = 2)
  title("(a)", adj = 0, cex.main = 0.95, font.main = 2)
  vv <- seq(-2, 2, length.out = 100); yb <- seq(-40, 50, length.out = 100)
  for (k in 1:99) rect(192, yb[k], 202, yb[k + 1], col = ramp_col(vv[k], -2, 0, 2), border = NA)
  text(210, c(-40, 50), c("-2", "+2"), cex = 0.7)

  par(mar = c(4, 4.5, 3, 1), xpd = FALSE)
  regs <- c("Tropical", "Temperate", "Boreal")
  M <- sapply(SSPS, function(s) sapply(regs, function(r)
    rb$additional_Tg_per_yr_2100[rb$ssp == s & rb$region == r]))
  rownames(M) <- regs; colnames(M) <- SSPS
  bp <- barplot(M, beside = TRUE, col = REGCOL[regs], border = "#333",
                names.arg = sub("SSP", "", SSPS), ylim = c(min(0, M), max(M) * 1.25), las = 1,
                legend.text = regs, args.legend = list(x = "topleft", bty = "n", cex = 0.8))
  abline(h = 0, col = "#888")
  text(colMeans(bp), max(M) * 1.15, sprintf("Σ %.1f", colSums(M)), font = 2, cex = 0.8)
  mtext("SSP", 1, 2.4, font = 2)
  mtext(expression(bold("Additional CH"[4]*" by 2100 (Tg yr"^-1*")")), 2, 2.6, cex = 0.9)
  title("(b)", adj = 0, cex.main = 0.95, font.main = 2)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 7 (choropleth): continent polygons filled by 2100 contribution.
#   Uses Natural Earth Admin-0 countries via rnaturalearth, dissolved to
#   continents (the bundled ne_50m_continents shapefile is broken - Australia
#   drops out - so it is no longer used). Robinson projection. Requires
#   sf + ggplot2 + rnaturalearth; skips cleanly if unavailable so the base-R
#   figures still run. Output: fig7_continent_choropleth.png
# ---------------------------------------------------------------------
fig7_choropleth <- function() {
  # The bundled ne_50m_continents shapefile has broken geometry (Australia is
  # lost no matter how it is repaired), so pull clean continent polygons from
  # Natural Earth's Admin-0 countries via rnaturalearth and dissolve by
  # continent. This is the maintained, artifact-free source and includes
  # Australia. Needs sf + ggplot2 + rnaturalearth (+ rnaturalearthdata).
  need <- c("sf", "rnaturalearth", "rnaturalearthdata")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message("  (skip fig7_choropleth: install.packages(c(",
            paste(sprintf('\"%s\"', miss), collapse = ", "), ")))")
    return(invisible(FALSE))
  }
  # headline = variable-inundation continent contributions (19c); fixed-area (19) fallback
  cc_csv <- file.path(RPI, "continent_contributions_2100_inundation.csv")
  if (!file.exists(cc_csv)) cc_csv <- file.path(RP, "continent_contributions_2100.csv")
  if (!file.exists(cc_csv)) {
    message("  (skip fig7_choropleth: continent contributions CSV missing; run 19c or 19)")
    return(invisible(FALSE))
  }
  suppressPackageStartupMessages(library(sf))
  sf::sf_use_s2(FALSE)
  MAP_SSP <- "SSP2-4.5"
  bd <- read.csv(cc_csv, stringsAsFactors = FALSE); bd <- bd[bd$ssp == MAP_SSP, ]

  # sustained-drought (SPEI48) contributions: anchor the published short-drought
  # continent map (MC median) to the sustained arm via the per-continent short/long
  # ratio computed deterministically in 21d. If 21d has not been run, falls back
  # to a single short-drought map.
  dur_csv <- file.path(OUTPUTS, "diagnostics", "duration_continent_contributions.csv")
  have_long <- file.exists(dur_csv)
  bd$short_val <- bd$additional_Tg_per_yr_2100
  if (have_long) {
    dd <- read.csv(dur_csv, stringsAsFactors = FALSE); dd <- dd[dd$ssp == MAP_SSP, ]
    m <- merge(bd[, c("continent", "short_val")], dd, by = "continent", all.x = TRUE)
    m$ratio <- ifelse(abs(m$short_Tg_2100) > 1e-9, m$long_Tg_2100 / m$short_Tg_2100, NA)
    m$long_val <- m$short_val * m$ratio
    bd <- merge(bd, m[, c("continent", "long_val")], by = "continent", all.x = TRUE)
  }

  world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")  # offline via rnaturalearthdata
  world <- world[!is.na(world$continent) &
                 !world$continent %in% c("Antarctica", "Seven seas (open ocean)"), ]
  world <- st_make_valid(world)
  # Natural Earth's `continent` field labels the Russian Federation "Asia"; force
  # the whole country into Europe so the map matches the Russia = Europe convention
  # used in the WAD2M continent x band allocation.
  nm <- if ("admin" %in% names(world)) world$admin
        else if ("sovereignt" %in% names(world)) world$sovereignt else world$name
  world$continent[!is.na(nm) & nm %in% c("Russia", "Russian Federation")] <- "Europe"
  # dissolve countries -> one polygon per continent (base R; no dplyr needed)
  cs <- sort(unique(world$continent))
  geoms <- do.call(c, lapply(cs, function(k) st_union(st_geometry(world[world$continent == k, ]))))
  cont <- st_sf(continent = cs, geometry = geoms, crs = st_crs(world))
  cont <- st_transform(cont, "ESRI:54030")   # Robinson

  # diverging value -> colour ramp, shared by BOTH maps and the bar chart so the
  # bar fills follow the map legend. Duration (short vs sustained) is encoded by a
  # hatch overlay on the sustained bars, NOT by colour.
  ramp <- grDevices::colorRampPalette(c("#8c510a", "#f7f7f7", "#2166ac"))(256)
  lim  <- max(abs(c(bd$short_val, if (have_long) bd$long_val)), na.rm = TRUE)
  col_for <- function(v) { i <- round((v + lim) / (2 * lim) * 255) + 1
    i[i < 1] <- 1; i[i > 256] <- 256; out <- ramp[i]; out[is.na(v)] <- "grey92"; out }
  vs <- setNames(bd$short_val, bd$continent)[cont$continent]
  vl <- if (have_long) setNames(bd$long_val, bd$continent)[cont$continent] else NULL

  # panel letter, pinned to a fixed device column (ndc x) so (a)/(b)/(c) align
  # vertically regardless of each panel's margins.
  plab <- function(lab) {
    x <- grconvertX(0.015, "ndc", "user"); y <- grconvertY(0.97, "npc", "user")
    text(x, y, lab, font = 2, cex = 1.3, adj = c(0, 1), xpd = NA)
  }
  # shared horizontal colour bar across the top (one legend for both maps + bars)
  draw_hcbar <- function() {
    par(mar = c(2.6, 7, 2.4, 7))
    plot.new(); plot.window(xlim = c(-lim, lim), ylim = c(0, 1), xaxs = "i")
    n <- length(ramp); xs <- seq(-lim, lim, length.out = n + 1)
    rect(xs[-(n + 1)], 0, xs[-1], 1, col = ramp, border = NA)
    rect(-lim, 0, lim, 1, border = "grey40")
    at <- pretty(c(-lim, lim), 5); at <- at[abs(at) <= lim]
    axis(1, at = at, labels = sprintf("%+.1f", at), cex.axis = 0.9, tck = -0.28, mgp = c(2, 0.55, 0))
    mtext(expression("Additional wetland CH"[4] * " in 2100 (Tg yr"^-1 * ")"),
          side = 3, line = 0.3, cex = 0.95)
  }
  draw_map <- function(vals, tag) {
    par(mar = c(0.5, 0.5, 1.0, 0.5))
    plot(st_geometry(cont), col = col_for(vals), border = "grey30", lwd = 0.5)
    plab(tag)
  }

  if (!have_long) {
    message("  (fig7_choropleth: 21d duration CSV not found; drawing short-drought map only)")
    png(file.path(OUT, "fig7_continent_choropleth.png"), width = 1700, height = 1080, res = 200)
    layout(matrix(1:2, 2, 1), heights = c(0.30, 1))
    draw_hcbar(); draw_map(vs, "")
    dev.off(); return(invisible(TRUE))
  }

  png(file.path(OUT, "fig7_continent_choropleth.png"), width = 1700, height = 2350, res = 210)
  layout(matrix(1:4, 4, 1), heights = c(0.32, 1, 1, 1.2))
  draw_hcbar()
  draw_map(vs, "(a)")
  draw_map(vl, "(b)")

  # (c) per-continent bars: fill colour follows the shared legend (value -> ramp);
  #     short vs sustained shown by a hatch overlay on the sustained bar.
  par(mar = c(4.6, 8.7, 2.4, 1.5))
  o <- order(bd$short_val); mb <- bd[o, ]
  M  <- rbind(short = mb$short_val, long = mb$long_val)
  colmat <- as.vector(rbind(col_for(mb$short_val), col_for(mb$long_val)))
  bp <- barplot(M, beside = TRUE, horiz = TRUE, names.arg = mb$continent, las = 1,
                col = colmat, border = "grey20", xlim = c(-lim * 1.2, lim * 1.2),
                xlab = expression("Additional CH"[4] * " 2100 (Tg yr"^-1 * ")"))
  Mh <- M; Mh["short", ] <- NA                       # hatch overlay on sustained bars only
  barplot(Mh, beside = TRUE, horiz = TRUE, col = "grey20", density = 17, angle = 45,
          add = TRUE, axes = FALSE, names.arg = rep("", ncol(M)))
  abline(v = 0, col = "grey50")
  vv <- as.vector(M)
  text(vv, as.vector(bp), sprintf("%.2f", vv),
       pos = ifelse(vv >= 0, 4, 2), cex = 0.72, xpd = NA)
  legend("topleft", c("Short-term drought (SPEI1)", "Sustained drought (SPEI48)"),
         fill = "grey85", density = c(NA, 17), angle = 45, border = "grey20", bty = "n",
         cex = 1.3, y.intersp = 1.2)
  plab("(c)")
  dev.off()
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Shared state panel for FIG 8 / FIG 9 (computed once from the raw panel)
#   Standardized temperature index (STI) within each site-month, hot/cold and
#   drought/wet classes, the compound hot-dry state, baseline marginals, the
#   observed 3x3 seed counts, and sigma_local (mean within-site-month TA SD).
# ---------------------------------------------------------------------
.state_env <- new.env()
get_state_panel <- function() {
  if (!is.null(.state_env$panel)) return(.state_env$panel)
  f <- file.path(DATA, "fluxes_drought.csv")
  if (!file.exists(f)) { message("  (state panel: ", f, " not found)"); return(NULL) }
  d <- rd(f)[, c("SITE_ID", "month", "TA_F", "SPEI1", "TIMESTAMP")]
  d <- d[is.finite(d$TA_F) & is.finite(d$SPEI1), ]
  d$year <- as.integer(substr(as.character(d$TIMESTAMP), 1, 4))
  g <- interaction(d$SITE_ID, d$month, drop = TRUE)
  d$m <- ave(d$TA_F, g, FUN = function(x) mean(x, na.rm = TRUE))
  d$s <- ave(d$TA_F, g, FUN = function(x) sd(x, na.rm = TRUE))
  d$nn <- ave(d$TA_F, g, FUN = length)
  ok <- is.finite(d$s) & d$s > 0 & d$nn >= 5
  d <- d[ok, ]; d$STI <- (d$TA_F - d$m) / d$s
  d$temp_class <- ifelse(d$STI >= 1, "hot", ifelse(d$STI <= -1, "cold", "normal"))
  d$condition  <- ifelse(d$SPEI1 <= -1, "drought", ifelse(d$SPEI1 >= 1, "extreme_wet", "normal"))
  d$hot_dry <- d$temp_class == "hot" & d$condition == "drought"
  sigma_local <- mean(tapply(d$TA_F, g[ok], function(x) sd(x, na.rm = TRUE)), na.rm = TRUE)
  seed <- table(factor(d$condition, c("drought", "normal", "extreme_wet")),
                factor(d$temp_class, c("cold", "normal", "hot")))
  panel <- list(
    d = d, sigma_local = sigma_local,
    p_hot0 = mean(d$temp_class == "hot"),  p_cold0 = mean(d$temp_class == "cold"),
    p_dry0 = mean(d$condition == "drought"), p_wet0 = mean(d$condition == "extreme_wet"),
    seed = matrix(as.numeric(seed), 3, 3))
  .state_env$panel <- panel
  panel
}

# ---------------------------------------------------------------------
# FIG 8: observed trends in the climate states that drive CH4 anomalies
#        (2006-2019 balanced tower panel). Trend table computed + written in R.
#   (a) annual frequency of the compound hot-dry state (% of site-months)
#   (b) annual mean STI (temperature anomaly)
# ---------------------------------------------------------------------
fig8 <- function() {
  sp <- get_state_panel(); if (is.null(sp)) { message("  (skip fig8)"); return(invisible(FALSE)) }
  d <- sp$d[sp$d$year >= 2006 & sp$d$year <= 2019, ]
  yr <- sort(unique(d$year))
  hotdry <- sapply(yr, function(y) 100 * mean(d$hot_dry[d$year == y]))
  mSTI   <- sapply(yr, function(y) mean(d$STI[d$year == y]))
  nobs   <- sapply(yr, function(y) sum(d$year == y))
  trend <- data.frame(year = yr, hotdry_pct = hotdry, mean_STI = mSTI, n = nobs)
  write.csv(trend, file.path(OUT, "discussion_state_trends.csv"), row.names = FALSE)
  fa <- lm(hotdry_pct ~ year, trend); fb <- lm(mean_STI ~ year, trend)

  # ---- per-site (within-site) trends: mean slope, % positive, one-sample t ----
  # Written to discussion_within_site_trends.csv so the Discussion text can cite
  # verified per-site trends (network-mean trends are in discussion_state_trends).
  d$hot <- d$temp_class == "hot"
  site_slope <- function(sid, valfun) {
    s  <- d[d$SITE_ID == sid, ]; ys <- sort(unique(s$year))
    if (length(ys) < 5) return(NA_real_)
    series <- sapply(ys, function(y) valfun(s[s$year == y, ]))
    unname(coef(lm(series ~ ys))[2])
  }
  metrics <- list(hotdry_pct = function(z) 100 * mean(z$hot_dry),
                  mean_STI   = function(z) mean(z$STI),
                  hot_pct    = function(z) 100 * mean(z$hot))
  ws <- do.call(rbind, lapply(names(metrics), function(mn) {
    sl <- vapply(unique(d$SITE_ID), site_slope, numeric(1), valfun = metrics[[mn]])
    sl <- sl[is.finite(sl)]; n <- length(sl)
    data.frame(metric = mn, n_sites = n, mean_slope_per_yr = mean(sl),
               pct_positive = 100 * mean(sl > 0),
               t_stat = mean(sl) / (sd(sl) / sqrt(n)))
  }))
  write.csv(ws, file.path(OUT, "discussion_within_site_trends.csv"), row.names = FALSE)
  # network-level trend slopes + t (companion to the annual series)
  net <- rbind(
    data.frame(metric = "hotdry_pct", slope_per_yr = coef(fa)[2],
               t_stat = summary(fa)$coefficients[2, 3]),
    data.frame(metric = "mean_STI", slope_per_yr = coef(fb)[2],
               t_stat = summary(fb)$coefficients[2, 3]))
  write.csv(net, file.path(OUT, "discussion_network_trends.csv"), row.names = FALSE)

  png_open("fig8_discussion_trend.png", 11, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 4.7, 3.2, 1))
  # (a)
  bp <- barplot(hotdry, names.arg = yr, col = "#c9a15a", border = "#8c510a",
                ylim = c(0, max(hotdry) * 1.2), las = 2, cex.names = 0.75)
  xx <- as.numeric(bp)
  lines(xx, predict(fa, data.frame(year = yr)), col = "#b2182b", lwd = 2.5)
  mtext("Hot-dry state (% of site-days)", 2, 3, font = 2, cex = 0.95)
  title("(a)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", bty = "n", cex = 0.85, text.col = "#b2182b",
         legend = bquote("trend " * .(sprintf("%+.2f", coef(fa)[2])) * " pp yr"^-1))
  # (b)
  plot(yr, mSTI, type = "n", xlab = "", ylab = "", las = 1)
  abline(h = 0, col = "#cccccc")
  points(yr, mSTI, pch = 19, col = "#c0392b", cex = 1.1); lines(yr, mSTI, col = "#c0392b", lwd = 1.5)
  lines(yr, predict(fb, data.frame(year = yr)), col = "#333333", lwd = 2.5)
  mtext("Year", 1, 2.4, font = 2); mtext("Mean STI (temperature anomaly), z", 2, 2.8, font = 2, cex = 0.95)
  title("(b)", adj = 0, font.main = 2, cex.main = 1)
  legend("topright", bty = "n", cex = 0.85, text.col = "#333333",
         legend = bquote("trend " * .(sprintf("%+.3f", coef(fb)[2])) * " z yr"^-1))
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 9: forward extension of the two drivers to 2100 under each SSP
#        (distribution-shift + IPF, ported from 12/projection logic; the
#        forward table is computed and written in R).
#   (a) hot-dry state frequency (%)   (b) mean STI (temperature anomaly, z)
# ---------------------------------------------------------------------
fig9 <- function() {
  sp <- get_state_panel(); if (is.null(sp)) { message("  (skip fig9)"); return(invisible(FALSE)) }
  gf <- file.path(DATA, "ssp_global_warming_levels.csv")
  if (!file.exists(gf)) { message("  (skip fig9: GWL table not found)"); return(invisible(FALSE)) }
  gwl <- rd(gf)
  # constants (from config.R when available, else the documented defaults)
  cfg <- file.path(PROJECT, "config.R"); e <- new.env()
  if (file.exists(cfg)) try(sys.source(cfg, e), silent = TRUE)
  gv <- function(n, d) if (exists(n, envir = e, inherits = FALSE)) get(n, envir = e) else d
  LA <- gv("LAND_AMPLIFICATION", 1.5); GWL_BASE <- gv("GWL_BASELINE", 1.15)
  K_DRY <- gv("MOISTURE_DRY_SENS", 0.15); K_WET <- gv("MOISTURE_WET_SENS", 0.05)
  SIG <- sp$sigma_local
  ipf <- function(seed, rt, ct, iters = 60) {
    J <- seed / sum(seed)
    for (i in 1:iters) { J <- J * (rt / rowSums(J)); J <- J * rep(ct / colSums(J), each = nrow(J)) }
    J
  }
  rows <- list()
  for (s in unique(gwl$ssp)) {
    gs <- gwl[gwl$ssp == s, ]
    for (k in seq_len(nrow(gs))) {
      dG <- gs$gwl[k] - GWL_BASE; mu <- LA * dG / SIG
      p_hot <- min(sp$p_hot0 * (1 - pnorm(1 - mu)) / (1 - pnorm(1)), 0.9)
      p_dry <- min(sp$p_dry0 * pnorm(-1 + K_DRY * dG) / pnorm(-1), 0.9)
      p_wet <- min(sp$p_wet0 * (1 - pnorm(1 - K_WET * dG)) / (1 - pnorm(1)), 0.9)
      p_cold <- min(sp$p_cold0 * pnorm(-1 - mu) / pnorm(-1), 0.9)
      J <- ipf(sp$seed, c(p_dry, 1 - p_dry - p_wet, p_wet), c(p_cold, 1 - p_cold - p_hot, p_hot))
      rows[[length(rows) + 1]] <- data.frame(ssp = s, year = gs$year[k],
        hot_dry_pct = 100 * J[1, 3], mean_STI = mu)
    }
  }
  fut <- do.call(rbind, rows)
  write.csv(fut, file.path(OUT, "discussion_future_extension.csv"), row.names = FALSE)
  ssps <- intersect(names(SSPCOL), unique(fut$ssp))

  png_open("fig9_future_extension.png", 11, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 4.7, 3.2, 1))
  plot(NA, xlim = range(fut$year), ylim = c(0, max(fut$hot_dry_pct) * 1.05),
       xlab = "", ylab = "", las = 1)
  for (s in ssps) { z <- fut[fut$ssp == s, ]; lines(z$year, z$hot_dry_pct, col = SSPCOL[s], lwd = 2.4) }
  mtext("Year", 1, 2.4, font = 2); mtext("Hot-dry state (% of site-time)", 2, 3, font = 2, cex = 0.95)
  title("(a)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  plot(NA, xlim = range(fut$year), ylim = c(0, max(fut$mean_STI) * 1.05),
       xlab = "", ylab = "", las = 1)
  for (s in ssps) { z <- fut[fut$ssp == s, ]; lines(z$year, z$mean_STI, col = SSPCOL[s], lwd = 2.4) }
  mtext("Year", 1, 2.4, font = 2); mtext("Mean STI (temperature anomaly, z)", 2, 2.8, font = 2, cex = 0.95)
  title("(b)", adj = 0, font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG (headline SI): variable-inundation projection with SHORT (SPEI1) vs
# SUSTAINED (SPEI48) drought in EVERY panel. This overwrites the 19c figure at
# outputs/regional_projection_inundation/figures/inundation_varying_projection.png
# so the manuscript figure always carries the duration sensitivity. Because
# 22_Figures.R runs last, this is the authoritative writer of that PNG (19c may
# also write a short-only version earlier; it is superseded here). Source CSVs
# come from 21d_DurationSensitivity.R:
#   outputs/diagnostics/duration_sensitivity_trajectories.csv  (panels a, c)
#   outputs/diagnostics/duration_area_factor.csv               (panel b)
# Skips cleanly if 21d has not been run.
#   (a) additional-emission rate: solid = short (SPEI1) + published 5-95% band,
#       dashed = sustained (SPEI48); one color per SSP.
#   (b) inundation area factor by region (top SSP) -- duration-independent.
#   (c) cumulative 2020-2100: short (published, +CI) vs sustained bars per SSP.
# ---------------------------------------------------------------------
fig_inundation_varying <- function() {
  DIAG <- file.path(OUTPUTS, "diagnostics")
  tf <- file.path(DIAG, "duration_sensitivity_trajectories.csv")
  af <- file.path(DIAG, "duration_area_factor.csv")
  if (!file.exists(tf) || !file.exists(af)) {
    message("  (skip inundation_varying: run 21d first)"); return(invisible(FALSE)) }
  tr <- rd(tf); afd <- rd(af)
  ssps <- unique(tr$ssp); yrs <- sort(unique(tr$year)); yend <- max(yrs)
  cols <- setNames(c("#1b9e77", "#e6ab02", "#d95f02", "#e7298a")[seq_along(ssps)], ssps)
  outdir <- file.path(RPI, "figures"); if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  png(file.path(outdir, "inundation_varying_projection.png"), width = 2200, height = 2000, res = 220)
  on.exit(dev.off(), add = TRUE)
  layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE), heights = c(1.4, 1))
  par(cex.axis = 1.15, cex.lab = 1.3, cex.main = 1.35)
  panel <- function(l) mtext(l, side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)

  # (a) rate: solid short (+ published band), dashed sustained
  par(mar = c(4.6, 5.2, 3, 1.5))
  yr_rng <- range(tr$short_rate_lo, tr$short_rate_hi, tr$long_rate, 0)
  plot(NA, xlim = range(yrs), ylim = yr_rng, xlab = "Year",
       ylab = expression("Additional wetland CH"[4] * " (Tg/yr)"), main = "")
  panel("(a)"); abline(h = 0, col = "gray70")
  for (s in ssps) { d <- tr[tr$ssp == s, ]; d <- d[order(d$year), ]
    polygon(c(d$year, rev(d$year)), c(d$short_rate_lo, rev(d$short_rate_hi)),
            col = adjustcolor(cols[s], 0.12), border = NA)
    lines(d$year, d$short_rate, col = cols[s], lwd = 3)
    lines(d$year, d$long_rate,  col = cols[s], lwd = 2.5, lty = 2) }
  legend("topleft", legend = ssps, col = cols, lwd = 3, bty = "n", cex = 1.1)
  legend("bottomright", legend = c("Short (SPEI1)", "Sustained (SPEI48)"),
         lty = c(1, 2), lwd = c(3, 2.5), col = "gray30", bty = "n", cex = 1.05)

  # (b) inundation area factor by region (top SSP) -- duration-independent
  topssp <- ssps[length(ssps)]; ab <- afd[afd$ssp == topssp, ]; regs <- unique(ab$region)
  rcol <- setNames(c("#1b9e77", "#e6ab02", "#7570b3", "#66a61e")[seq_along(regs)], regs)
  par(mar = c(4.6, 5.2, 3, 1.5))
  plot(NA, xlim = range(yrs), ylim = range(ab$area_factor, 1),
       xlab = "Year", ylab = "Inundation area factor", main = "")
  panel("(b)"); abline(h = 1, col = "gray70")
  for (rg in regs) { a1 <- ab[ab$region == rg, ]; a1 <- a1[order(a1$year), ]
    lines(a1$year, a1$area_factor, col = rcol[rg], lwd = 3) }
  legend("bottomleft", legend = regs, col = rcol, lwd = 3, bty = "n", cex = 1.1)

  # (c) cumulative short (published, +CI) vs sustained
  e <- tr[tr$year == yend, ]; e <- e[match(ssps, e$ssp), ]
  M <- rbind(short = e$short_cum, long = e$long_cum)
  colvec <- as.vector(rbind(cols[ssps], adjustcolor(cols[ssps], 0.45)))
  yl <- range(0, e$short_cum_lo, e$short_cum_hi, e$long_cum); yl <- yl + c(-0.08, 0.18) * diff(yl)
  par(mar = c(6.5, 5.2, 3, 1.5))
  bp <- barplot(M, beside = TRUE, names.arg = ssps, las = 2, ylim = yl, col = colvec,
                border = "#333333", ylab = expression("Cumulative CH"[4] * " 2020-2100 (Tg)"), main = "")
  abline(h = 0, col = "gray70")
  arrows(bp[1, ], e$short_cum_lo, bp[1, ], e$short_cum_hi, angle = 90, code = 3, length = 0.045, col = "gray20")
  text(bp[1, ], e$short_cum_hi, round(e$short_cum), pos = 3, xpd = TRUE, font = 2, cex = 0.9)
  text(bp[2, ], pmax(e$long_cum, 0), round(e$long_cum), pos = 3, xpd = TRUE, cex = 0.9, col = "gray30")
  legend("topleft", legend = c("Short (SPEI1)", "Sustained (SPEI48)"),
         fill = c("gray25", adjustcolor("gray25", 0.45)), border = "#333333", bty = "n", cex = 1.0)
  panel("(c)")
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# Run everything
# ---------------------------------------------------------------------
message("Building figure-only CSVs in R ...")
build_site_response_csvs()

figs <- list(fig1 = fig1, fig2 = fig2, fig3 = fig3, fig4 = fig4, fig5 = fig5, fig6 = fig6,
             fig6b = function() fig6b("fig6b_projection_uncertainty.png"),
             fig6_results = function() fig6b("fig6_projection_results.png"),
             fig7_continent = fig7_continent, fig7_regional = fig7_regional,
             fig7_choropleth = fig7_choropleth, fig8 = fig8, fig9 = fig9,
             fig_inundation_varying = fig_inundation_varying)

message("Drawing figures ...")
for (nm in names(figs)) {
  ok <- tryCatch({ figs[[nm]](); TRUE },
                 error = function(e) { message("  !! ", nm, " failed: ", conditionMessage(e)); FALSE })
  if (ok) message("  wrote ", nm)
}
message("Done. Figures + generated CSVs in: ", OUT)
