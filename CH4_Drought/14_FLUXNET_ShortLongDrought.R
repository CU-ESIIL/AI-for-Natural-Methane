# Compare short- and long-term SPEI drought effects in FLUXNET-CH4 data.

rm(list = ls())

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file))
} else {
  getwd()
}

source(file.path(analysis_dir, "config.R"))

input_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
output_dir <- file.path(analysis_dir, "outputs", "fluxnet_short_long_drought")
figure_dir <- file.path(output_dir, "figures")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

load(input_file)
analysis_data <- fluxes.drought_normalized

spei_indices <- c("SPEI1", "SPEI3", "SPEI6", "SPEI12", "SPEI24", "SPEI36", "SPEI48")
spei_indices <- spei_indices[spei_indices %in% names(analysis_data)]

term_class <- ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "short_term", "long_term")
term_lookup <- data.frame(Drought.IDX = spei_indices, term_class = term_class, stringsAsFactors = FALSE)

mean_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

median_or_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

sd_or_na <- function(x) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  sd(x, na.rm = TRUE)
}

condition_for_index <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x <= DROUGHT_THRESHOLD, "drought", ifelse(x >= WET_THRESHOLD, "extreme_wet", "normal"))
  )
}

summarise_observations <- function(data, index_name) {
  condition <- condition_for_index(data[[index_name]])
  ok <- !is.na(condition) & !is.na(data$normalized_Fch4)
  split_values <- split(data$normalized_Fch4[ok], condition[ok])

  out <- do.call(rbind, lapply(names(split_values), function(condition_name) {
    values <- split_values[[condition_name]]
    data.frame(
      Drought.IDX = index_name,
      condition = condition_name,
      n_obs = length(values),
      n_sites = length(unique(data$SITE_ID[ok][condition[ok] == condition_name])),
      mean_normalized_Fch4 = mean_or_na(values),
      median_normalized_Fch4 = median_or_na(values),
      sd_normalized_Fch4 = sd_or_na(values),
      stringsAsFactors = FALSE
    )
  }))

  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

summarise_site_paired <- function(data, index_name) {
  data$condition_tmp <- condition_for_index(data[[index_name]])
  data <- data[!is.na(data$condition_tmp) & !is.na(data$normalized_Fch4), ]

  site_condition <- aggregate(
    normalized_Fch4 ~ SITE_ID + condition_tmp,
    data,
    mean,
    na.rm = TRUE
  )
  names(site_condition)[2] <- "condition"

  site_wide <- reshape(site_condition, idvar = "SITE_ID", timevar = "condition", direction = "wide")
  drought_col <- "normalized_Fch4.drought"
  normal_col <- "normalized_Fch4.normal"
  wet_col <- "normalized_Fch4.extreme_wet"

  if (!(drought_col %in% names(site_wide)) || !(normal_col %in% names(site_wide))) {
    drought_delta <- rep(NA_real_, nrow(site_wide))
  } else {
    drought_delta <- site_wide[[drought_col]] - site_wide[[normal_col]]
  }

  if (!(wet_col %in% names(site_wide)) || !(normal_col %in% names(site_wide))) {
    wet_delta <- rep(NA_real_, nrow(site_wide))
  } else {
    wet_delta <- site_wide[[wet_col]] - site_wide[[normal_col]]
  }

  data.frame(
    Drought.IDX = index_name,
    SITE_ID = site_wide$SITE_ID,
    drought_minus_normal = drought_delta,
    extreme_wet_minus_normal = wet_delta,
    stringsAsFactors = FALSE
  )
}

summarise_site_month_matched <- function(data, index_name) {
  data$condition_tmp <- condition_for_index(data[[index_name]])
  data <- data[!is.na(data$condition_tmp) & !is.na(data$normalized_Fch4) & !is.na(data$month), ]

  site_month_condition <- aggregate(
    cbind(normalized_Fch4, FCH4_F_ANNOPTLM, spei_value = data[[index_name]]) ~ SITE_ID + IGBP + month + condition_tmp,
    data,
    mean,
    na.rm = TRUE
  )
  names(site_month_condition)[4] <- "condition"

  normal <- site_month_condition[site_month_condition$condition == "normal",
                                 c("SITE_ID", "month", "normalized_Fch4", "FCH4_F_ANNOPTLM", "spei_value")]
  names(normal)[3:5] <- c("normal_normalized_Fch4", "normal_FCH4", "normal_spei")

  non_normal <- site_month_condition[site_month_condition$condition != "normal", ]
  matched <- merge(non_normal, normal, by = c("SITE_ID", "month"), all.x = FALSE, sort = FALSE)
  if (nrow(matched) == 0) {
    return(data.frame())
  }

  matched$delta_normalized_Fch4 <- matched$normalized_Fch4 - matched$normal_normalized_Fch4
  matched$delta_FCH4 <- matched$FCH4_F_ANNOPTLM - matched$normal_FCH4
  matched$delta_spei <- matched$spei_value - matched$normal_spei

  site_condition <- aggregate(
    cbind(delta_normalized_Fch4, delta_FCH4, delta_spei) ~ SITE_ID + IGBP + condition,
    matched,
    mean,
    na.rm = TRUE
  )
  site_condition$Drought.IDX <- index_name
  merge(site_condition, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

summarise_delta_tests <- function(delta_data, delta_col) {
  rows <- split(delta_data, paste(delta_data$Drought.IDX, delta_data$condition, sep = "__"))
  out <- do.call(rbind, lapply(rows, function(sub) {
    values <- sub[[delta_col]]
    values <- values[!is.na(values)]
    if (length(values) == 0) return(NULL)

    t_p <- if (length(values) > 1) t.test(values)$p.value else NA_real_
    w_p <- if (length(values) > 1) wilcox.test(values, mu = 0, exact = FALSE)$p.value else NA_real_

    data.frame(
      Drought.IDX = sub$Drought.IDX[1],
      term_class = sub$term_class[1],
      condition = sub$condition[1],
      response = delta_col,
      n_sites = length(values),
      mean_delta = mean_or_na(values),
      median_delta = median_or_na(values),
      sd_delta = sd_or_na(values),
      n_positive = sum(values > 0),
      n_negative = sum(values < 0),
      t_test_p = t_p,
      wilcox_p = w_p,
      stringsAsFactors = FALSE
    )
  }))
  out[order(match(out$Drought.IDX, spei_indices), out$condition), ]
}

summarise_site_slopes <- function(data, index_name) {
  rows <- split(data, data$SITE_ID)
  out <- do.call(rbind, lapply(rows, function(sub) {
    ok <- !is.na(sub[[index_name]]) & !is.na(sub$normalized_Fch4)
    sub <- sub[ok, ]
    if (nrow(sub) < 10 || length(unique(sub[[index_name]])) < 3) return(NULL)

    model <- lm(sub$normalized_Fch4 ~ sub[[index_name]])
    model_summary <- summary(model)
    data.frame(
      Drought.IDX = index_name,
      SITE_ID = sub$SITE_ID[1],
      n_obs = nrow(sub),
      slope = coef(model)[2],
      p_value = coef(model_summary)[2, 4],
      r_squared = model_summary$r.squared,
      stringsAsFactors = FALSE
    )
  }))
  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

observation_summary <- do.call(rbind, lapply(spei_indices, summarise_observations, data = analysis_data))
site_paired_deltas <- do.call(rbind, lapply(spei_indices, summarise_site_paired, data = analysis_data))
site_paired_deltas <- merge(site_paired_deltas, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)

site_month_deltas <- do.call(rbind, lapply(spei_indices, summarise_site_month_matched, data = analysis_data))

site_month_delta_tests <- summarise_delta_tests(site_month_deltas, "delta_normalized_Fch4")
site_paired_drought_tests <- data.frame(
  Drought.IDX = character(),
  term_class = character(),
  condition = character(),
  response = character(),
  n_sites = integer(),
  mean_delta = numeric(),
  median_delta = numeric(),
  sd_delta = numeric(),
  n_positive = integer(),
  n_negative = integer(),
  t_test_p = numeric(),
  wilcox_p = numeric(),
  stringsAsFactors = FALSE
)

for (index_name in spei_indices) {
  sub <- site_paired_deltas[site_paired_deltas$Drought.IDX == index_name, ]
  values <- sub$drought_minus_normal
  values <- values[!is.na(values)]
  if (length(values) == 0) next
  site_paired_drought_tests <- rbind(
    site_paired_drought_tests,
    data.frame(
      Drought.IDX = index_name,
      term_class = term_lookup$term_class[match(index_name, term_lookup$Drought.IDX)],
      condition = "drought",
      response = "drought_minus_normal",
      n_sites = length(values),
      mean_delta = mean_or_na(values),
      median_delta = median_or_na(values),
      sd_delta = sd_or_na(values),
      n_positive = sum(values > 0),
      n_negative = sum(values < 0),
      t_test_p = if (length(values) > 1) t.test(values)$p.value else NA_real_,
      wilcox_p = if (length(values) > 1) wilcox.test(values, mu = 0, exact = FALSE)$p.value else NA_real_,
      stringsAsFactors = FALSE
    )
  )
}

site_linear_slopes <- do.call(rbind, lapply(spei_indices, summarise_site_slopes, data = analysis_data))
slope_summary <- aggregate(
  cbind(slope, p_value, r_squared) ~ Drought.IDX + term_class,
  site_linear_slopes,
  function(x) c(n = length(x), mean = mean_or_na(x), median = median_or_na(x), sd = sd_or_na(x))
)

summarise_near_normal_baselines <- function(data, index_name) {
  near_normal <- data[
    !is.na(data[[index_name]]) &
      data[[index_name]] > NORMAL_LOWER &
      data[[index_name]] < NORMAL_UPPER &
      !is.na(data$FCH4_F_ANNOPTLM),
  ]

  baseline <- aggregate(
    FCH4_F_ANNOPTLM ~ SITE_ID,
    near_normal,
    function(x) c(
      n_near_normal = length(x),
      near_normal_mean_FCH4 = mean_or_na(x),
      near_normal_median_FCH4 = median_or_na(x),
      near_normal_sd_FCH4 = sd_or_na(x)
    )
  )

  values <- as.data.frame(baseline$FCH4_F_ANNOPTLM)
  names(values) <- c(
    "n_near_normal",
    "near_normal_mean_FCH4",
    "near_normal_median_FCH4",
    "near_normal_sd_FCH4"
  )

  out <- cbind(
    data.frame(Drought.IDX = index_name, SITE_ID = baseline$SITE_ID, stringsAsFactors = FALSE),
    values
  )
  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

near_normal_baselines <- do.call(
  rbind,
  lapply(spei_indices, summarise_near_normal_baselines, data = analysis_data)
)

near_normal_wide <- reshape(
  near_normal_baselines[, c("SITE_ID", "Drought.IDX", "near_normal_mean_FCH4")],
  idvar = "SITE_ID",
  timevar = "Drought.IDX",
  direction = "wide"
)

baseline_shift_rows <- lapply(spei_indices, function(index_name) {
  col_name <- paste0("near_normal_mean_FCH4.", index_name)
  data.frame(
    Drought.IDX = index_name,
    SITE_ID = near_normal_wide$SITE_ID,
    baseline_minus_SPEI1 = near_normal_wide[[col_name]] - near_normal_wide$near_normal_mean_FCH4.SPEI1,
    stringsAsFactors = FALSE
  )
})
near_normal_baseline_shifts <- do.call(rbind, baseline_shift_rows)
near_normal_baseline_shifts <- merge(near_normal_baseline_shifts, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)

write.csv(observation_summary, file.path(output_dir, "observation_condition_summary_by_spei_window.csv"), row.names = FALSE)
write.csv(site_paired_deltas, file.path(output_dir, "site_paired_condition_deltas_by_spei_window.csv"), row.names = FALSE)
write.csv(site_paired_drought_tests, file.path(output_dir, "site_paired_drought_tests_by_spei_window.csv"), row.names = FALSE)
write.csv(site_month_deltas, file.path(output_dir, "site_month_matched_condition_deltas_by_spei_window.csv"), row.names = FALSE)
write.csv(site_month_delta_tests, file.path(output_dir, "site_month_matched_delta_tests_by_spei_window.csv"), row.names = FALSE)
write.csv(site_linear_slopes, file.path(output_dir, "site_linear_slopes_by_spei_window.csv"), row.names = FALSE)
write.csv(slope_summary, file.path(output_dir, "site_linear_slope_summary_by_spei_window.csv"), row.names = FALSE)
write.csv(near_normal_baselines, file.path(output_dir, "site_near_normal_baselines_by_spei_window.csv"), row.names = FALSE)
write.csv(near_normal_baseline_shifts, file.path(output_dir, "site_near_normal_baseline_shifts_vs_spei1.csv"), row.names = FALSE)

png(file.path(figure_dir, "drought_delta_by_spei_window.png"), width = 1800, height = 1200, res = 180)
boxplot(
  drought_minus_normal ~ Drought.IDX,
  data = site_paired_deltas,
  col = ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "#9ecae1", "#fdae6b"),
  ylab = "Site mean drought - normal normalized CH4",
  xlab = "SPEI accumulation window",
  main = "FLUXNET site-paired drought effects by SPEI window"
)
abline(h = 0, col = "gray35", lty = 2)
dev.off()

png(file.path(figure_dir, "near_normal_baseline_by_spei_window.png"), width = 2400, height = 1400, res = 180)
layout(matrix(c(1, 2), nrow = 1), widths = c(1.15, 1))
palette_cols <- ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "#9ecae1", "#fdae6b")

baseline_matrix <- sapply(spei_indices, function(index_name) {
  near_normal_wide[[paste0("near_normal_mean_FCH4.", index_name)]]
})
rownames(baseline_matrix) <- near_normal_wide$SITE_ID

matplot(
  seq_along(spei_indices),
  t(baseline_matrix),
  type = "l",
  lty = 1,
  col = adjustcolor("gray45", alpha.f = 0.35),
  xaxt = "n",
  xlab = "SPEI accumulation window",
  ylab = "Near-normal mean CH4 flux",
  main = "Site near-normal baseline by SPEI window"
)
axis(1, at = seq_along(spei_indices), labels = spei_indices)
lines(seq_along(spei_indices), apply(baseline_matrix, 2, median, na.rm = TRUE), col = "black", lwd = 3)
lines(seq_along(spei_indices), apply(baseline_matrix, 2, mean, na.rm = TRUE), col = "#de2d26", lwd = 3)
legend(
  "topright",
  legend = c("Sites", "Median across sites", "Mean across sites"),
  col = c("gray45", "black", "#de2d26"),
  lty = 1,
  lwd = c(1, 3, 3),
  bty = "n"
)

shift_plot_data <- near_normal_baseline_shifts[near_normal_baseline_shifts$Drought.IDX != "SPEI1", ]
shift_plot_data$Drought.IDX <- factor(shift_plot_data$Drought.IDX, levels = spei_indices[spei_indices != "SPEI1"])
boxplot(
  baseline_minus_SPEI1 ~ Drought.IDX,
  data = shift_plot_data,
  col = palette_cols[spei_indices != "SPEI1"],
  ylab = "Near-normal baseline shift vs SPEI1",
  xlab = "SPEI accumulation window",
  main = "How much does the baseline move?"
)
abline(h = 0, col = "gray35", lty = 2)
stripchart(
  baseline_minus_SPEI1 ~ Drought.IDX,
  data = shift_plot_data,
  vertical = TRUE,
  method = "jitter",
  pch = 16,
  col = adjustcolor("black", alpha.f = 0.35),
  add = TRUE
)
dev.off()

png(file.path(figure_dir, "site_month_matched_drought_delta_by_spei_window.png"), width = 1800, height = 1200, res = 180)
drought_site_month <- site_month_deltas[site_month_deltas$condition == "drought", ]
boxplot(
  delta_normalized_Fch4 ~ Drought.IDX,
  data = drought_site_month,
  col = ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "#9ecae1", "#fdae6b"),
  ylab = "Site-month matched drought - normal normalized CH4",
  xlab = "SPEI accumulation window",
  main = "FLUXNET site-month matched drought effects"
)
abline(h = 0, col = "gray35", lty = 2)
dev.off()

png(file.path(figure_dir, "linear_slope_by_spei_window.png"), width = 1800, height = 1200, res = 180)
boxplot(
  slope ~ Drought.IDX,
  data = site_linear_slopes,
  col = ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "#9ecae1", "#fdae6b"),
  ylab = "Site-level slope: normalized CH4 ~ SPEI",
  xlab = "SPEI accumulation window",
  main = "FLUXNET linear CH4-SPEI slopes by window"
)
abline(h = 0, col = "gray35", lty = 2)
dev.off()

message("Wrote FLUXNET short/long drought comparison to: ", output_dir)
