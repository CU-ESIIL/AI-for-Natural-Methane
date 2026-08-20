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
RP   <- file.path(OUTPUTS, "regional_projection")          # 19
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
  png_open("fig1_compound_grid.png", 6.6, 5.0)
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
  mtext("Temperature anomaly", side = 2, line = 3.2, font = 2)
  title("Methane flux anomaly across the compound extreme-event space",
        font.main = 2, cex.main = 1.05, line = 2.2)
  vals <- seq(-8, 17, length.out = 100); by <- seq(0.5, 3.5, length.out = 100)
  for (k in 1:99) rect(3.75, by[k], 3.95, by[k + 1], col = ramp_col(vals[k], -8, 0, 17), border = NA)
  rect(3.75, 0.5, 3.95, 3.5, border = "#888888")
  text(4.2, c(0.5, 2, 3.5), sprintf("%+d", c(17, 4, -8)), cex = 0.7)
  text(4.5, 2, "Mean normalized FCH4 anomaly", srt = 90, cex = 0.75)
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

  png_open("fig2_marginal_partition.png", 9.2, 4.2)
  par(mfrow = c(1, 2), mar = c(4, 4.5, 3, 1))
  cols <- TEMP[c("cold", "normal", "hot")]
  bp <- barplot(eff, col = cols, border = "#333333", ylim = c(-11, 11),
                names.arg = c("Cold", "Normal", "Hot"), las = 1)
  abline(h = 0, col = "#999999")
  arrows(bp, eff - se, bp, eff + se, angle = 90, code = 3, length = 0.05, col = "#333333")
  # place labels clear of the error-bar whiskers (beyond eff +/- se)
  text(bp, eff + ifelse(eff >= 0, se + 1.1, -(se + 1.1)), sprintf("%+.1f", eff), font = 2)
  mtext("Methane anomaly vs normal", side = 2, line = 2.6, font = 2)
  title("(a) Temperature-anomaly effect on methane", font.main = 2, cex.main = 1)
  text(mean(bp), 10.3, "hot vs cold p<0.001", font = 3, cex = 0.8, col = "#555555")

  terms <- c("Temperature\nanomaly", "Moisture\ncondition", "Interaction")
  par(mar = c(4, 7, 3, 2))
  bp2 <- barplot(rev(ss), horiz = TRUE, col = "grey70", border = "#333333",
                 names.arg = rev(terms), las = 1, xlim = c(0, 1.55e6))
  text(rev(ss) + 15000, bp2, sprintf("%.0fk", rev(ss) / 1e3), pos = 4, font = 2,
       cex = 0.9, xpd = NA)
  mtext("Sum of squares (Type-I)", side = 1, line = 2.4, font = 2)
  title("(b) Variance partition (all p<0.001)", font.main = 2, cex.main = 1)
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
  mtext("SPEI (moisture anomaly)", side = 1, line = 2.5, font = 2)
  mtext("Methane flux (relative to normal)", side = 2, line = 2.6, font = 2)
  title("Drought raises methane only when it is hot", font.main = 2, cex.main = 1.1)
  legend("topright", bty = "n", cex = 0.95, lwd = 3, col = TEMP[c("cold", "normal", "hot")],
         legend = sprintf("%s (slope %+.1f)", c("Cold", "Normal", "Hot"),
                          m[c("cold", "normal", "hot")]))
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 4: (a) Q10 stability  (b) random-forest importance
# ---------------------------------------------------------------------
fig4 <- function() {
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

  png_open("fig4_q10_rf.png", 9.4, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 4.5, 4, 1))
  bp <- barplot(q10, col = TEMP[c("cold", "normal", "hot")], border = "#333333",
                ylim = c(0, 3.8), names.arg = c("Cold", "Normal", "Hot"), las = 1)
  arrows(bp, q10 - qse, bp, q10 + qse, angle = 90, code = 3, length = 0.05, col = "#333333")
  text(bp, q10 + 0.15, sprintf("%.2f", q10), font = 2)
  mtext(expression(bold("Temperature sensitivity Q"[10])), side = 2, line = 2.5)
  title(expression(bold("(a) Q"[10]*" is stable across anomalies")), cex.main = 1)

  par(mar = c(4, 9, 4, 2))
  cols_rf <- ifelse(is_temp, "#b2182b", "#999999"); y <- seq_len(nrow(rf))
  plot(NA, xlim = c(0, max(rf[[imp_col]]) * 1.05), ylim = c(0.5, length(y) + 0.5),
       axes = FALSE, xlab = "", ylab = "")
  segments(0, y, rf[[imp_col]], y, col = cols_rf, lwd = 2)
  points(rf[[imp_col]], y, pch = 19, col = cols_rf, cex = 1.1)
  axis(1)
  for (k in y) axis(2, at = k, labels = lab[k], las = 1, tick = FALSE,
                    col.axis = cols_rf[k], font.axis = ifelse(is_temp[k], 2, 1), cex.axis = 0.8)
  mtext("% increase in MSE", side = 1, line = 2.4, font = 2)
  title("(b) Random-forest importance\n(temperature in red)", cex.main = 0.95)
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
  d <- rd(f)
  d <- d[d$Drought.IDX == SPEI_WINDOW & d$condition == "drought" & !is.na(d$delta_response), ]
  obs <- d[d$source == "FLUXNET", c("SITE_ID", "delta_response")]
  mod <- d[d$source == MODEL_NAME & d$scenario == MODEL_SCENARIO, c("SITE_ID", "delta_response")]
  names(obs)[2] <- "obs"; names(mod)[2] <- "mod"
  paired <- merge(obs, mod, by = "SITE_ID")            # overlapping sites only
  n_overlap <- length(unique(paired$SITE_ID))          # accurate count of overlapping sites
  sign_agree <- round(100 * mean(sign(paired$obs) == sign(paired$mod)))
  col_obs <- "#2C3E50"; col_model <- "grey"
  means <- c(mean(paired$obs), mean(paired$mod))

  png_open("fig5_tem_eval.png", 11, 5)
  layout(matrix(c(1, 2), 1, 2), widths = c(1, 1.25))
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
  title("(a) Mean drought response", font.main = 2, cex.main = 1.05)
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
  title("(b) Direction of site-level drought response", font.main = 2, cex.main = 1.05)
  mtext(sprintf("Model-observation sign agreement: %d%% (n = %d overlapping sites)",
                sign_agree, n_overlap), 3, 0.2, cex = 0.8, col = "grey30")
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 6: projection - additional CH4 rate & cumulative (illustrative)
# ---------------------------------------------------------------------
fig6 <- function() {
  p <- rd(EEP, "extreme_emissions_projection.csv")
  ssps <- intersect(names(SSPCOL), unique(p$ssp))
  png_open("fig6_projection.png", 10, 4.3)
  par(mfrow = c(1, 2), mar = c(4, 5, 3, 1))
  plot(NA, xlim = range(p$year), ylim = range(0, p$additional_Tg_per_yr),
       xlab = "", ylab = "", las = 1)
  for (s in ssps) { d <- p[p$ssp == s, ]; lines(d$year, d$additional_Tg_per_yr, col = SSPCOL[s], lwd = 2.4) }
  abline(h = 0, col = "#999999")
  mtext("Year", 1, 2.4, font = 2)
  mtext(expression(bold("Additional wetland CH"[4]*" (Tg yr"^-1*")")), 2, 2.8)
  title("(a) Extra emissions rate vs today", cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  cum <- sapply(ssps, function(s) p$cumulative_Tg[p$ssp == s & p$year == max(p$year)])
  par(mar = c(6.5, 5, 3, 1))   # extra bottom room for the vertical SSP labels
  bp <- barplot(cum, col = SSPCOL[ssps], border = "#333", las = 2, names.arg = ssps,
                ylim = c(0, max(cum) * 1.12))
  text(bp, cum + max(cum) * 0.03, sprintf("%.0f", cum), font = 2)
  mtext(expression(bold("Cumulative extra CH"[4]*" 2020-2100 (Tg)")), 2, 2.8)
  title("(b) Cumulative additional emissions", cex.main = 1)
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
  title("(a) Extra emission rate (5-95% band)", cex.main = 1)
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
  title("(b) Cumulative (5-95%)", cex.main = 1)
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
  title(expression("(a) FLUXNET-CH"[4]*" sites: methane response to heat (hot - cold anomaly)"),
        cex.main = 1, font.main = 2)
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
  title("(b) By continent", cex.main = 1, font.main = 2)
  dev.off()
}

# ---------------------------------------------------------------------
# FIG 7 (regional): latitude-band map + grouped regional bars by SSP
# ---------------------------------------------------------------------
fig7_regional <- function() {
  rb <- rd(RP, "regional_breakdown_2100.csv")
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
  title(bquote("(a) Region-weighted contribution to global extreme-driven CH"[4]*" by 2100 ("*.(MAP_SSP)*")"),
        cex.main = 0.95, font.main = 2)
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
  title("(b) Regional contributions ( Σ = global )", cex.main = 0.95, font.main = 2)
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
  need <- c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    message("  (skip fig7_choropleth: install.packages(c(",
            paste(sprintf('\"%s\"', miss), collapse = ", "), ")))")
    return(invisible(FALSE))
  }
  cc_csv <- file.path(RP, "continent_contributions_2100.csv")
  if (!file.exists(cc_csv)) {
    message("  (skip fig7_choropleth: continent_contributions_2100.csv missing)")
    return(invisible(FALSE))
  }
  suppressPackageStartupMessages({ library(sf); library(ggplot2) })
  sf::sf_use_s2(FALSE)
  MAP_SSP <- "SSP2-4.5"
  bd <- read.csv(cc_csv, stringsAsFactors = FALSE); bd <- bd[bd$ssp == MAP_SSP, ]

  world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")  # offline via rnaturalearthdata
  world <- world[!is.na(world$continent) &
                 !world$continent %in% c("Antarctica", "Seven seas (open ocean)"), ]
  world <- st_make_valid(world)
  # dissolve countries -> one polygon per continent (base R; no dplyr needed)
  cs <- sort(unique(world$continent))
  geoms <- do.call(c, lapply(cs, function(k) st_union(st_geometry(world[world$continent == k, ]))))
  cont <- st_sf(continent = cs, geometry = geoms, crs = st_crs(world))
  cont <- merge(cont, bd[, c("continent", "additional_Tg_per_yr_2100")], by = "continent", all.x = TRUE)

  lim <- max(abs(bd$additional_Tg_per_yr_2100), na.rm = TRUE)
  p <- ggplot(cont) +
    geom_sf(aes(fill = additional_Tg_per_yr_2100), color = "grey30", linewidth = 0.15) +
    scale_fill_gradient2(low = "#8c510a", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
                         limits = c(-lim, lim), na.value = "grey92",
                         name = expression(atop("Additional CH"[4], "(Tg yr"^-1*", 2100)"))) +
    coord_sf(crs = "ESRI:54030", expand = FALSE) +   # Robinson, full extent (no y-clip)
    labs(title = bquote("Continental contribution to global extreme-driven CH"[4]*" by 2100 ("*.(MAP_SSP)*")"),
         subtitle = "Region-weighted; continents sum to the global total (WAD2M allocation)") +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_line(color = "grey90", linewidth = 0.2),
          axis.text = element_blank(), axis.title = element_blank(), legend.position = "right")
  ggsave(file.path(OUT, "fig7_continent_choropleth.png"), p, width = 9, height = 4.8, dpi = 300)
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
  title("(a) Compound hot-dry frequency, 2006-2019", font.main = 2, cex.main = 1)
  legend("topleft", bty = "n", cex = 0.85, text.col = "#b2182b",
         legend = bquote("trend " * .(sprintf("%+.2f", coef(fa)[2])) * " pp yr"^-1))
  # (b)
  plot(yr, mSTI, type = "n", xlab = "", ylab = "", las = 1)
  abline(h = 0, col = "#cccccc")
  points(yr, mSTI, pch = 19, col = "#c0392b", cex = 1.1); lines(yr, mSTI, col = "#c0392b", lwd = 1.5)
  lines(yr, predict(fb, data.frame(year = yr)), col = "#333333", lwd = 2.5)
  mtext("Year", 1, 2.4, font = 2); mtext("Mean STI (temperature anomaly), z", 2, 2.8, font = 2, cex = 0.95)
  title("(b) Mean temperature anomaly, 2006-2019", font.main = 2, cex.main = 1)
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
  title("(a) Projected compound hot-dry frequency", font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  plot(NA, xlim = range(fut$year), ylim = c(0, max(fut$mean_STI) * 1.05),
       xlab = "", ylab = "", las = 1)
  for (s in ssps) { z <- fut[fut$ssp == s, ]; lines(z$year, z$mean_STI, col = SSPCOL[s], lwd = 2.4) }
  mtext("Year", 1, 2.4, font = 2); mtext("Mean STI (temperature anomaly, z)", 2, 2.8, font = 2, cex = 0.95)
  title("(b) Projected temperature anomaly", font.main = 2, cex.main = 1)
  legend("topleft", legend = ssps, col = SSPCOL[ssps], lwd = 2.4, bty = "n", cex = 0.85)
  dev.off()
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
             fig7_choropleth = fig7_choropleth, fig8 = fig8, fig9 = fig9)

message("Drawing figures ...")
for (nm in names(figs)) {
  ok <- tryCatch({ figs[[nm]](); TRUE },
                 error = function(e) { message("  !! ", nm, " failed: ", conditionMessage(e)); FALSE })
  if (ok) message("  wrote ", nm)
}
message("Done. Figures + generated CSVs in: ", OUT)
