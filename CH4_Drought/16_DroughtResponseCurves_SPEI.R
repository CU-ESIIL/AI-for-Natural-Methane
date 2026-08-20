# 16_DroughtResponseCurves_SPEI.R
# ---------------------------------------------------------------------------
# CH4 response curves across SPEI, for FLUXNET and every registered model.
# Model-agnostic: models come from MODEL_REGISTRY (models.R), so adding a model
# requires no change here. Outputs -> outputs/drought_response_curves/ (+ server).
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "models.R"))

out_rel <- function(f) file.path("drought_response_curves", f)

load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
fluxnet_data <- fluxes.drought_normalized

spei_indices <- c("SPEI1", "SPEI3", "SPEI6", "SPEI12", "SPEI24", "SPEI36", "SPEI48")
spei_indices <- spei_indices[spei_indices %in% names(fluxnet_data)]
term_class <- ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "short_term", "long_term")
term_lookup <- data.frame(Drought.IDX = spei_indices, term_class = term_class, stringsAsFactors = FALSE)

make_long <- function(data, source, scenario, response_col) {
  if (!response_col %in% names(data)) return(NULL)
  out <- do.call(rbind, lapply(spei_indices, function(idx) {
    if (!idx %in% names(data)) return(NULL)
    o <- data.frame(source = source, scenario = scenario, Drought.IDX = idx,
                    SPEI = data[[idx]], normalized_CH4 = data[[response_col]],
                    SITE_ID = data$SITE_ID, stringsAsFactors = FALSE)
    o[!is.na(o$SPEI) & !is.na(o$normalized_CH4) & o$SPEI >= -4 & o$SPEI <= 4, ]
  }))
  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

bin_summary <- function(data, bin_width = 0.25) {
  breaks <- seq(-4, 4, by = bin_width)
  data$bin_mid <- breaks[as.integer(cut(data$SPEI, breaks, include.lowest = TRUE, right = FALSE))] + bin_width / 2
  data <- data[!is.na(data$bin_mid), ]
  aggregate(normalized_CH4 ~ source + scenario + Drought.IDX + term_class + bin_mid, data,
            function(x) c(n = length(x), mean = mean(x, na.rm = TRUE),
                          median = median(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(length(x))))
}
site_bin_summary <- function(data, bin_width = 0.25) {
  breaks <- seq(-4, 4, by = bin_width)
  data$bin_mid <- breaks[as.integer(cut(data$SPEI, breaks, include.lowest = TRUE, right = FALSE))] + bin_width / 2
  data <- data[!is.na(data$bin_mid), ]
  sb <- aggregate(normalized_CH4 ~ source + scenario + Drought.IDX + term_class + SITE_ID + bin_mid, data, mean, na.rm = TRUE)
  aggregate(normalized_CH4 ~ source + scenario + Drought.IDX + term_class + bin_mid, sb,
            function(x) c(n_sites = length(x), mean = mean(x, na.rm = TRUE),
                          median = median(x, na.rm = TRUE), se = sd(x, na.rm = TRUE) / sqrt(length(x))))
}
unpack_summary <- function(s) {
  v <- s$normalized_CH4; if (is.list(v) && !is.matrix(v)) v <- do.call(rbind, v)
  out <- cbind(s[names(s) != "normalized_CH4"], as.data.frame(v))
  for (col in intersect(c("bin_mid", "n", "n_sites", "mean", "median", "se"), names(out))) out[[col]] <- as.numeric(out[[col]])
  out
}

# ---- FLUXNET + every registered model/scenario ----
pieces <- list(make_long(fluxnet_data, "FLUXNET", "tower", "normalized_Fch4"))
for (model_name in names(MODEL_REGISTRY)) {
  m <- MODEL_REGISTRY[[model_name]]
  p <- m$daily_rdata(analysis_dir)
  if (!file.exists(p)) { message("Skipping ", model_name, " (missing daily file)"); next }
  load(p); md <- get(m$daily_object)
  for (scn in names(m$scenarios))
    pieces[[paste(model_name, scn)]] <- make_long(md, model_name, scn, m$scenarios[[scn]]$normalized)
}
all_long <- do.call(rbind, pieces[!vapply(pieces, is.null, logical(1))])

observation_bins <- unpack_summary(bin_summary(all_long))
site_bins <- unpack_summary(site_bin_summary(all_long))

save_output_csv(all_long, out_rel("long_normalized_ch4_by_source_spei.csv"), analysis_dir)
save_output_csv(observation_bins, out_rel("binned_observation_response_curves.csv"), analysis_dir)
save_output_csv(site_bins, out_rel("binned_site_mean_response_curves.csv"), analysis_dir)

# dynamic source list + colors (FLUXNET black; models from a palette)
groups <- unique(paste(all_long$source, all_long$scenario, sep = "_"))
groups <- c(groups[startsWith(groups, "FLUXNET")], sort(groups[!startsWith(groups, "FLUXNET")]))
palette <- c("#2b8cbe", "#f03b20", "#31a354", "#756bb1", "#e6550d", "#c51b8a")
gcol <- setNames(ifelse(startsWith(groups, "FLUXNET"), "black",
                        palette[((seq_along(groups) - 1) %% length(palette)) + 1]), groups)
glty <- setNames(ifelse(startsWith(groups, "FLUXNET"), 1,
                        rep(c(1, 2, 3), length.out = length(groups))), groups)

plot_curves <- function(summary_data, value_col, y_label, relpath, min_n_col, min_n) {
  draw <- function() {
    par(mfrow = c(2, 4), mar = c(4, 4, 3, 1))
    for (idx in spei_indices) {
      sub <- summary_data[summary_data$Drought.IDX == idx & summary_data[[min_n_col]] >= min_n, ]
      ylim <- range(sub[[value_col]], na.rm = TRUE); if (!all(is.finite(ylim))) ylim <- c(-1, 1)
      plot(c(-4, 4), ylim, type = "n", xlab = idx, ylab = y_label,
           main = paste0(idx, " (", term_lookup$term_class[match(idx, term_lookup$Drought.IDX)], ")"))
      abline(h = 0, col = "gray55", lty = 2); abline(v = c(-1, 1), col = "gray75", lty = 3)
      for (grp in groups) {
        parts <- strsplit(grp, "_", fixed = TRUE)[[1]]
        ld <- sub[sub$source == parts[1] & sub$scenario == paste(parts[-1], collapse = "_"), ]
        ld <- ld[order(ld$bin_mid), ]
        if (nrow(ld) > 0) lines(ld$bin_mid, ld[[value_col]], col = gcol[grp], lwd = 2, lty = glty[grp])
      }
      legend("topleft", legend = groups, col = gcol[groups], lty = glty[groups], lwd = 2, bty = "n", cex = 0.7)
    }
    plot.new()
  }
  save_output_figure(draw, relpath, analysis_dir, width = 2400, height = 1700, res = 180)
}

plot_curves(observation_bins, "mean",   "Mean normalized CH4",   out_rel("figures/mean_response_curves_by_spei_window.png"),   "n", 20)
plot_curves(observation_bins, "median", "Median normalized CH4", out_rel("figures/median_response_curves_by_spei_window.png"), "n", 20)
plot_curves(site_bins,        "mean",   "Site-mean normalized CH4", out_rel("figures/site_mean_response_curves_by_spei_window.png"), "n_sites", 3)

message("Wrote response curves for: ", paste(groups, collapse = ", "))
