# Evaluate climate and soil-moisture shifts across SPEI48 conditions and
# relate those shifts to methane-flux changes.

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(broom)
library(ggrepel)
library(patchwork)

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file))
} else {
  # Interactive run (no --file=): pick whichever candidate holds config.R, so this
  # works whether the working directory is the project root or the CH4_Drought folder.
  cand <- c(getwd(), file.path(getwd(), "CH4_Drought"),
            "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) hit[1] else stop(
    "Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
}
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "temperature_index.R"))

input_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
output_dir <- file.path(analysis_dir, "outputs", "condition_changes")
figure_dir <- file.path(output_dir, "figures")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

load(input_file)

condition_vars <- c("TA_F", "VPD_F", "P_F", "SVWC")
response_vars <- c("FCH4_F_ANNOPTLM", "normalized_Fch4")

condition_labels <- c("drought", "normal", "extreme_wet")

analysis_data <- fluxes.drought_normalized %>%
  add_thermal_season() %>%
  add_temp_anomaly_class() %>%
  mutate(
    condition = case_when(
      .data[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD ~ "drought",
      .data[[DROUGHT_INDEX]] >= WET_THRESHOLD ~ "extreme_wet",
      TRUE ~ "normal"
    ),
    condition = factor(condition, levels = condition_labels)
  ) %>%
  add_event_class(season_col = "thermal_season", out_col = "event_class") %>%
  add_event_class(season_col = "temp_class", out_col = "temp_event_class") %>%
  select(SITE_ID, IGBP, month, condition, thermal_season, event_class,
         temp_class, temp_event_class, STI, TA_site_z,
         all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)) %>%
  drop_na(SITE_ID, condition, thermal_season, all_of(condition_vars), all_of(response_vars))

condition_summary <- analysis_data %>%
  group_by(condition) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(SITE_ID),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

site_condition_summary <- analysis_data %>%
  group_by(SITE_ID, IGBP, condition) %>%
  summarise(
    n_obs = n(),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

site_month_condition_summary <- analysis_data %>%
  group_by(SITE_ID, IGBP, month, condition) %>%
  summarise(
    n_obs = n(),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

normal_baseline <- site_month_condition_summary %>%
  filter(condition == "normal") %>%
  select(
    SITE_ID, month,
    ends_with("_mean")
  ) %>%
  rename_with(~ paste0(.x, "_normal"), -c(SITE_ID, month))

site_month_condition_deltas <- site_month_condition_summary %>%
  filter(condition != "normal") %>%
  inner_join(normal_baseline, by = c("SITE_ID", "month")) %>%
  mutate(
    delta_spei = .data[[paste0(DROUGHT_INDEX, "_mean")]] - .data[[paste0(DROUGHT_INDEX, "_mean_normal")]],
    delta_TA_F = TA_F_mean - TA_F_mean_normal,
    delta_VPD_F = VPD_F_mean - VPD_F_mean_normal,
    delta_P_F = P_F_mean - P_F_mean_normal,
    delta_SVWC = SVWC_mean - SVWC_mean_normal,
    delta_FCH4 = FCH4_F_ANNOPTLM_mean - FCH4_F_ANNOPTLM_mean_normal,
    delta_normalized_Fch4 = normalized_Fch4_mean - normalized_Fch4_mean_normal
  ) %>%
  select(
    SITE_ID, IGBP, month, condition, n_obs,
    starts_with("delta_"),
    ends_with("_mean"),
    ends_with("_normal")
  )

site_condition_deltas <- site_month_condition_deltas %>%
  mutate(weight_obs = n_obs) %>%
  group_by(SITE_ID, IGBP, condition) %>%
  summarise(
    n_months = n_distinct(month),
    across(starts_with("delta_"), ~ weighted.mean(.x, w = .data$weight_obs, na.rm = TRUE)),
    n_obs = sum(weight_obs),
    .groups = "drop"
  )

drought_deltas <- site_condition_deltas %>%
  filter(condition == "drought")

wet_deltas <- site_condition_deltas %>%
  filter(condition == "extreme_wet")

delta_predictors <- c("delta_TA_F", "delta_VPD_F", "delta_P_F", "delta_SVWC")

correlation_table <- function(data, response) {
  map_dfr(delta_predictors, function(x) {
    test <- cor.test(data[[x]], data[[response]], method = "spearman", exact = FALSE)
    tibble(
      response = response,
      predictor = x,
      n_sites = sum(complete.cases(data[[x]], data[[response]])),
      spearman_rho = unname(test$estimate),
      p_value = test$p.value
    )
  })
}

drought_correlations <- bind_rows(
  correlation_table(drought_deltas, "delta_FCH4"),
  correlation_table(drought_deltas, "delta_normalized_Fch4")
) %>%
  mutate(condition = "drought", .before = 1)

wet_correlations <- bind_rows(
  correlation_table(wet_deltas, "delta_FCH4"),
  correlation_table(wet_deltas, "delta_normalized_Fch4")
) %>%
  mutate(condition = "extreme_wet", .before = 1)

fit_delta_model <- function(data, response) {
  formula <- as.formula(paste(response, "~", paste(delta_predictors, collapse = " + ")))
  model <- lm(formula, data = data)
  model_fit <- glance(model) %>%
    transmute(
      model_r_squared = r.squared,
      model_adj_r_squared = adj.r.squared,
      model_p_value = p.value,
      model_df_residual = df.residual
    )

  bind_cols(
    model_fit,
    tidy(model) %>% filter(term != "(Intercept)")
  ) %>%
    mutate(response = response, .before = 1)
}

drought_models <- bind_rows(
  fit_delta_model(drought_deltas, "delta_FCH4"),
  fit_delta_model(drought_deltas, "delta_normalized_Fch4")
) %>%
  mutate(condition = "drought", .before = 1)

wet_models <- bind_rows(
  fit_delta_model(wet_deltas, "delta_FCH4"),
  fit_delta_model(wet_deltas, "delta_normalized_Fch4")
) %>%
  mutate(condition = "extreme_wet", .before = 1)

write_csv(condition_summary, file.path(output_dir, "condition_summary.csv"))
write_csv(site_condition_summary, file.path(output_dir, "site_condition_summary.csv"))
write_csv(site_month_condition_summary, file.path(output_dir, "site_month_condition_summary.csv"))
write_csv(site_month_condition_deltas, file.path(output_dir, "site_month_condition_deltas_vs_normal.csv"))
write_csv(site_condition_deltas, file.path(output_dir, "site_condition_deltas_vs_normal.csv"))
write_csv(bind_rows(drought_correlations, wet_correlations), file.path(output_dir, "delta_spearman_correlations.csv"))
write_csv(bind_rows(drought_models, wet_models), file.path(output_dir, "delta_linear_models.csv"))

condition_long <- analysis_data %>%
  select(condition, all_of(condition_vars), normalized_Fch4) %>%
  pivot_longer(-condition, names_to = "variable", values_to = "value")

condition_boxplot <- ggplot(condition_long, aes(x = condition, y = value, fill = condition)) +
  geom_boxplot(outlier.alpha = 0.05) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c(drought = "#b35806", normal = "#5ab4ac", extreme_wet = "#2166ac")) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "condition_boxplots.png"),
  condition_boxplot,
  width = 9,
  height = 6,
  dpi = 300
)

condition_names <- c(
  drought = "Drought",
  normal = "Normal",
  extreme_wet = "Extreme wet"
)

variable_names <- c(
  TA_F = "Air temperature",
  VPD_F = "VPD",
  P_F = "Precipitation",
  SVWC = "Soil water",
  normalized_Fch4 = "Methane flux anomaly"
)

delta_names <- c(
  delta_TA_F = "Air temperature",
  delta_VPD_F = "VPD",
  delta_P_F = "Precipitation",
  delta_SVWC = "Soil water"
)

condition_palette <- c(
  drought = "#b35806",
  normal = "#5ab4ac",
  extreme_wet = "#2166ac"
)

site_mean_vars <- paste0(c(condition_vars, "normalized_Fch4"), "_mean")

site_condition_long <- site_condition_summary %>%
  select(SITE_ID, condition, all_of(site_mean_vars)) %>%
  pivot_longer(
    cols = all_of(site_mean_vars),
    names_to = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = sub("_mean$", "", variable)) %>%
  mutate(
    condition_label = factor(condition_names[as.character(condition)], levels = condition_names),
    variable_label = factor(variable_names[variable], levels = variable_names)
  )

condition_mean_figure <- site_condition_long %>%
  group_by(condition, condition_label, variable_label) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    se = sd(value, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = condition_label, y = mean, color = condition)) +
  geom_hline(yintercept = 0, color = "grey82", linewidth = 0.3) +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se), linewidth = 0.45) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 3) +
  scale_color_manual(values = condition_palette, guide = "none") +
  labs(x = NULL, y = "Site mean +/- SE") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "grey94", color = "grey80")
  )

ggsave(
  file.path(figure_dir, "finding_condition_means.png"),
  condition_mean_figure,
  width = 9,
  height = 5.5,
  dpi = 300
)

delta_long <- site_condition_deltas %>%
  mutate(
    condition_label = factor(condition_names[condition], levels = condition_names[c("drought", "extreme_wet")]),
    methane_response = if_else(delta_normalized_Fch4 >= 0, "Higher than normal", "Lower than normal")
  )

methane_delta_figure <- delta_long %>%
  group_by(condition) %>%
  mutate(SITE_ID_ordered = reorder(SITE_ID, delta_normalized_Fch4)) %>%
  ungroup() %>%
  ggplot(aes(x = delta_normalized_Fch4, y = SITE_ID_ordered, color = methane_response)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_segment(aes(x = 0, xend = delta_normalized_Fch4, yend = SITE_ID_ordered), linewidth = 0.55) +
  geom_point(size = 2) +
  facet_wrap(~ condition_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Higher than normal" = "#2166ac", "Lower than normal" = "#b35806")) +
  labs(
    x = expression(Delta ~ "normalized" ~ F[CH4] ~ "(condition - normal)"),
    y = "Site",
    color = NULL
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  file.path(figure_dir, "finding_site_methane_deltas.png"),
  methane_delta_figure,
  width = 9,
  height = 7,
  dpi = 300
)

delta_condition_figure <- site_condition_deltas %>%
  select(condition, all_of(delta_predictors)) %>%
  pivot_longer(-condition, names_to = "variable", values_to = "delta") %>%
  mutate(
    condition_label = factor(condition_names[condition], levels = condition_names[c("drought", "extreme_wet")]),
    variable_label = factor(delta_names[variable], levels = delta_names)
  ) %>%
  ggplot(aes(x = condition_label, y = delta, fill = condition)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.5) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = condition_palette, guide = "none") +
  labs(x = NULL, y = "Change from site normal") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "grey94", color = "grey80")
  )

ggsave(
  file.path(figure_dir, "finding_condition_deltas.png"),
  delta_condition_figure,
  width = 10,
  height = 4.5,
  dpi = 300
)

plot_delta_relationship <- function(data, x, response, title) {
ggplot(data, aes(x = .data[[x]], y = .data[[response]], color = IGBP)) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
    geom_point(size = 2, alpha = 0.8) +
    geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
    ggrepel::geom_text_repel(aes(label = SITE_ID), size = 2, max.overlaps = 8, show.legend = FALSE) +
    labs(x = x, y = response, title = title, color = "IGBP") +
    theme_bw()
}

strong_relationship_figure <- (
  plot_delta_relationship(drought_deltas, "delta_VPD_F", "delta_normalized_Fch4", "Drought response tracks VPD change") +
    labs(x = "Drought VPD change from normal", y = expression(Delta ~ "normalized" ~ F[CH4]))
) / (
  plot_delta_relationship(wet_deltas, "delta_SVWC", "delta_normalized_Fch4", "Extreme-wet response tracks soil-water change") +
    labs(x = "Extreme-wet soil-water change from normal", y = expression(Delta ~ "normalized" ~ F[CH4]))
)

ggsave(
  file.path(figure_dir, "finding_strongest_relationships.png"),
  strong_relationship_figure,
  width = 7.5,
  height = 8,
  dpi = 300
)

drought_relationships <- (
  plot_delta_relationship(drought_deltas, "delta_TA_F", "delta_normalized_Fch4", "Drought: air temperature") +
    plot_delta_relationship(drought_deltas, "delta_VPD_F", "delta_normalized_Fch4", "Drought: VPD")
) / (
  plot_delta_relationship(drought_deltas, "delta_P_F", "delta_normalized_Fch4", "Drought: precipitation") +
    plot_delta_relationship(drought_deltas, "delta_SVWC", "delta_normalized_Fch4", "Drought: soil water")
)

ggsave(
  file.path(figure_dir, "drought_delta_relationships.png"),
  drought_relationships,
  width = 11,
  height = 8,
  dpi = 300
)

wet_relationships <- (
  plot_delta_relationship(wet_deltas, "delta_TA_F", "delta_normalized_Fch4", "Extreme wet: air temperature") +
    plot_delta_relationship(wet_deltas, "delta_VPD_F", "delta_normalized_Fch4", "Extreme wet: VPD")
) / (
  plot_delta_relationship(wet_deltas, "delta_P_F", "delta_normalized_Fch4", "Extreme wet: precipitation") +
    plot_delta_relationship(wet_deltas, "delta_SVWC", "delta_normalized_Fch4", "Extreme wet: soil water")
)

ggsave(
  file.path(figure_dir, "extreme_wet_delta_relationships.png"),
  wet_relationships,
  width = 11,
  height = 8,
  dpi = 300
)

# ===========================================================================
# TEMPERATURE (THERMAL-SEASON) AXIS
# ---------------------------------------------------------------------------
# Separate patterns driven by HOT vs COLD conditions from those driven by
# WET vs DRY conditions. thermal_season classifies each observation as a
# winter (cold-season) or summer (warm-season) event RELATIVE TO ITS OWN SITE,
# independent of SPEI. We summarise methane and its drivers (1) by thermal
# season alone (regardless of moisture) and (2) across the full moisture x
# thermal cross-classification (summer-dry, winter-dry, summer-wet, ...).
# ===========================================================================

temp_output_dir <- file.path(analysis_dir, "outputs", "temperature_events")
temp_figure_dir <- file.path(temp_output_dir, "figures")
dir.create(temp_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(temp_figure_dir, recursive = TRUE, showWarnings = FALSE)

thermal_palette <- c(winter = "#2166ac", shoulder = "grey65", summer = "#b2182b")

# (1) Temperature-only summaries: methane + drivers by thermal season, pooled
#     across ALL moisture conditions (extreme-dry, normal, extreme-wet).
temp_summary <- analysis_data %>%
  group_by(thermal_season) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(SITE_ID),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

site_temp_summary <- analysis_data %>%
  group_by(SITE_ID, IGBP, thermal_season) %>%
  summarise(
    n_obs = n(),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

# Site-level warm-minus-cold methane contrast (regardless of moisture): a direct
# read on how much of each site's methane signal is a hot vs cold effect.
site_thermal_contrast <- site_temp_summary %>%
  select(SITE_ID, IGBP, thermal_season, normalized_Fch4_mean, TA_F_mean) %>%
  pivot_wider(names_from = thermal_season,
              values_from = c(normalized_Fch4_mean, TA_F_mean)) %>%
  mutate(
    methane_summer_minus_winter = normalized_Fch4_mean_summer - normalized_Fch4_mean_winter,
    TA_summer_minus_winter      = TA_F_mean_summer - TA_F_mean_winter
  )

# (2) Moisture x thermal cross-classification (the event grid).
event_summary <- analysis_data %>%
  group_by(condition, thermal_season, event_class) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(SITE_ID),
    across(
      c(all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

# (3) Two-way partition: does the methane anomaly depend on moisture, on thermal
#     season, and do the two interact? Sequential (Type-I) ANOVA + coefficients.
twoway_model <- lm(normalized_Fch4 ~ thermal_season * condition, data = analysis_data)
twoway_anova <- broom::tidy(anova(twoway_model))
twoway_terms <- broom::tidy(twoway_model)

# Marginal thermal effect on methane, ignoring moisture entirely.
thermal_only_model <- lm(normalized_Fch4 ~ thermal_season, data = analysis_data)
thermal_only_terms <- broom::tidy(thermal_only_model)

write_csv(temp_summary,            file.path(temp_output_dir, "temperature_only_summary.csv"))
write_csv(site_temp_summary,       file.path(temp_output_dir, "site_temperature_summary.csv"))
write_csv(site_thermal_contrast,   file.path(temp_output_dir, "site_summer_minus_winter_contrast.csv"))
write_csv(event_summary,           file.path(temp_output_dir, "moisture_by_thermal_summary.csv"))
write_csv(twoway_anova,            file.path(temp_output_dir, "twoway_anova_moisture_x_thermal.csv"))
write_csv(twoway_terms,            file.path(temp_output_dir, "twoway_terms_moisture_x_thermal.csv"))
write_csv(thermal_only_terms,      file.path(temp_output_dir, "thermal_only_methane_terms.csv"))

# --- Figures ---------------------------------------------------------------

# Methane anomaly by thermal season (regardless of moisture).
thermal_methane_figure <- analysis_data %>%
  ggplot(aes(x = thermal_season, y = normalized_Fch4, fill = thermal_season)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.04) +
  scale_fill_manual(values = thermal_palette, guide = "none") +
  coord_cartesian(ylim = quantile(analysis_data$normalized_Fch4, c(0.02, 0.98), na.rm = TRUE)) +
  labs(x = NULL, y = "Methane flux anomaly",
       title = "Methane anomaly by thermal season (all moisture conditions)") +
  theme_bw()

ggsave(file.path(temp_figure_dir, "thermal_methane_anomaly.png"),
       thermal_methane_figure, width = 6, height = 5, dpi = 300)

# Drivers by thermal season.
thermal_drivers_figure <- analysis_data %>%
  select(thermal_season, all_of(condition_vars)) %>%
  pivot_longer(-thermal_season, names_to = "variable", values_to = "value") %>%
  mutate(variable_label = factor(variable_names[variable], levels = variable_names)) %>%
  ggplot(aes(x = thermal_season, y = value, fill = thermal_season)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.04) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = thermal_palette, guide = "none") +
  labs(x = NULL, y = NULL, title = "Environmental drivers by thermal season") +
  theme_bw()

ggsave(file.path(temp_figure_dir, "thermal_drivers.png"),
       thermal_drivers_figure, width = 10, height = 4.5, dpi = 300)

# The event grid: mean methane anomaly across moisture x thermal season.
event_grid_figure <- event_summary %>%
  ggplot(aes(x = condition, y = thermal_season, fill = normalized_Fch4_mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f\n(n=%d)", normalized_Fch4_mean, n_obs)), size = 3) +
  scale_fill_gradient2(low = "#b35806", mid = "grey95", high = "#2166ac", midpoint = 0) +
  scale_x_discrete(labels = c(drought = "Extreme dry", normal = "Normal", extreme_wet = "Extreme wet")) +
  labs(x = "Moisture condition", y = "Thermal season",
       fill = "Mean methane\nanomaly",
       title = "Methane anomaly across the moisture x thermal-season grid") +
  theme_bw()

ggsave(file.path(temp_figure_dir, "event_grid_methane.png"),
       event_grid_figure, width = 7.5, height = 4.5, dpi = 300)

# Site-level summer-minus-winter methane contrast (hot vs cold effect per site).
site_thermal_figure <- site_thermal_contrast %>%
  filter(!is.na(methane_summer_minus_winter)) %>%
  mutate(SITE_ID_ordered = reorder(SITE_ID, methane_summer_minus_winter),
         direction = if_else(methane_summer_minus_winter >= 0,
                             "Higher in summer", "Higher in winter")) %>%
  ggplot(aes(x = methane_summer_minus_winter, y = SITE_ID_ordered, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_segment(aes(x = 0, xend = methane_summer_minus_winter, yend = SITE_ID_ordered),
               linewidth = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Higher in summer" = "#b2182b", "Higher in winter" = "#2166ac")) +
  labs(x = "Summer - winter methane anomaly", y = "Site", color = NULL,
       title = "Per-site hot vs cold methane contrast") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6))

ggsave(file.path(temp_figure_dir, "site_summer_minus_winter_methane.png"),
       site_thermal_figure, width = 7.5, height = 8, dpi = 300)

cat("Wrote temperature (thermal-season) outputs to:", temp_output_dir, "\n")

# ===========================================================================
# TEMPERATURE ANOMALY AXIS (HOT / NORMAL / COLD EVENTS)
# ---------------------------------------------------------------------------
# The true analog of the SPEI moisture axis: TA_F standardized WITHIN each
# site-month (STI), so hot = hotter than normal for that site and time of year,
# cold = colder than normal, and there is an explicit NORMAL class. This lets
# us ask which methane patterns are driven by temperature anomalies (heat vs
# cold events) regardless of whether the event is extreme-dry or extreme-wet,
# with a proper normal reference on both axes.
# ===========================================================================

anomaly_output_dir <- file.path(analysis_dir, "outputs", "temperature_anomaly_events")
anomaly_figure_dir <- file.path(anomaly_output_dir, "figures")
dir.create(anomaly_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(anomaly_figure_dir, recursive = TRUE, showWarnings = FALSE)

temp_palette <- c(cold = "#2166ac", normal = "grey70", hot = "#b2182b")

# Restrict to observations with a defined temperature anomaly class.
anomaly_data <- analysis_data %>% filter(!is.na(temp_class))

# (1) Temperature-anomaly-only summaries: methane + drivers by hot/normal/cold,
#     pooled across ALL moisture conditions.
temp_class_summary <- anomaly_data %>%
  group_by(temp_class) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(SITE_ID),
    across(
      c(STI, all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

site_temp_class_summary <- anomaly_data %>%
  group_by(SITE_ID, IGBP, temp_class) %>%
  summarise(
    n_obs = n(),
    across(
      c(STI, all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

# Site-level hot-minus-cold methane contrast (regardless of moisture).
site_hot_cold_contrast <- site_temp_class_summary %>%
  select(SITE_ID, IGBP, temp_class, normalized_Fch4_mean, TA_F_mean) %>%
  pivot_wider(names_from = temp_class,
              values_from = c(normalized_Fch4_mean, TA_F_mean)) %>%
  mutate(
    methane_hot_minus_cold = normalized_Fch4_mean_hot - normalized_Fch4_mean_cold,
    TA_hot_minus_cold      = TA_F_mean_hot - TA_F_mean_cold
  )

# (2) Moisture x temperature-anomaly cross-classification (the event grid, now
#     with a normal x normal reference cell).
temp_event_summary <- anomaly_data %>%
  group_by(condition, temp_class, temp_event_class) %>%
  summarise(
    n_obs = n(),
    n_sites = n_distinct(SITE_ID),
    across(
      c(STI, all_of(DROUGHT_INDEX), all_of(condition_vars), all_of(response_vars)),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    .groups = "drop"
  )

# (3) Two-way partition with NORMAL as the reference on both axes.
anomaly_model_data <- anomaly_data %>%
  mutate(
    temp_class = relevel(factor(temp_class), ref = "normal"),
    condition  = relevel(factor(condition), ref = "normal")
  )

temp_twoway_model <- lm(normalized_Fch4 ~ temp_class * condition, data = anomaly_model_data)
temp_twoway_anova <- broom::tidy(anova(temp_twoway_model))
temp_twoway_terms <- broom::tidy(temp_twoway_model)

# Marginal temperature-anomaly effect on methane, ignoring moisture (vs normal).
temp_only_model <- lm(normalized_Fch4 ~ temp_class, data = anomaly_model_data)
temp_only_terms <- broom::tidy(temp_only_model)

write_csv(temp_class_summary,     file.path(anomaly_output_dir, "temp_anomaly_only_summary.csv"))
write_csv(site_temp_class_summary, file.path(anomaly_output_dir, "site_temp_anomaly_summary.csv"))
write_csv(site_hot_cold_contrast, file.path(anomaly_output_dir, "site_hot_minus_cold_contrast.csv"))
write_csv(temp_event_summary,     file.path(anomaly_output_dir, "moisture_by_temp_anomaly_summary.csv"))
write_csv(temp_twoway_anova,      file.path(anomaly_output_dir, "twoway_anova_moisture_x_temp_anomaly.csv"))
write_csv(temp_twoway_terms,      file.path(anomaly_output_dir, "twoway_terms_moisture_x_temp_anomaly.csv"))
write_csv(temp_only_terms,        file.path(anomaly_output_dir, "temp_anomaly_only_methane_terms.csv"))

# --- Figures ---------------------------------------------------------------

temp_class_methane_figure <- anomaly_data %>%
  ggplot(aes(x = temp_class, y = normalized_Fch4, fill = temp_class)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.04) +
  scale_fill_manual(values = temp_palette, guide = "none") +
  coord_cartesian(ylim = quantile(anomaly_data$normalized_Fch4, c(0.02, 0.98), na.rm = TRUE)) +
  labs(x = NULL, y = "Methane flux anomaly",
       title = "Methane anomaly by temperature anomaly class (all moisture conditions)",
       subtitle = "hot / cold = hotter / colder than normal for that site and month") +
  theme_bw()

ggsave(file.path(anomaly_figure_dir, "temp_anomaly_methane.png"),
       temp_class_methane_figure, width = 6.5, height = 5, dpi = 300)

temp_drivers_figure <- anomaly_data %>%
  select(temp_class, all_of(condition_vars)) %>%
  pivot_longer(-temp_class, names_to = "variable", values_to = "value") %>%
  mutate(variable_label = factor(variable_names[variable], levels = variable_names)) %>%
  ggplot(aes(x = temp_class, y = value, fill = temp_class)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.04) +
  facet_wrap(~ variable_label, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = temp_palette, guide = "none") +
  labs(x = NULL, y = NULL, title = "Environmental drivers by temperature anomaly class") +
  theme_bw()

ggsave(file.path(anomaly_figure_dir, "temp_anomaly_drivers.png"),
       temp_drivers_figure, width = 10, height = 4.5, dpi = 300)

temp_event_grid_figure <- temp_event_summary %>%
  mutate(temp_class = factor(temp_class, levels = c("cold", "normal", "hot"))) %>%
  ggplot(aes(x = condition, y = temp_class, fill = normalized_Fch4_mean)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f\n(n=%d)", normalized_Fch4_mean, n_obs)), size = 3) +
  scale_fill_gradient2(low = "#b35806", mid = "grey95", high = "#2166ac", midpoint = 0) +
  scale_x_discrete(labels = c(drought = "Extreme dry", normal = "Normal", extreme_wet = "Extreme wet")) +
  labs(x = "Moisture condition", y = "Temperature anomaly",
       fill = "Mean methane\nanomaly",
       title = "Methane anomaly across the moisture x temperature-anomaly grid",
       subtitle = "normal x Normal is the joint reference cell") +
  theme_bw()

ggsave(file.path(anomaly_figure_dir, "temp_anomaly_event_grid.png"),
       temp_event_grid_figure, width = 7.5, height = 4.5, dpi = 300)

site_hot_cold_figure <- site_hot_cold_contrast %>%
  filter(!is.na(methane_hot_minus_cold)) %>%
  mutate(SITE_ID_ordered = reorder(SITE_ID, methane_hot_minus_cold),
         direction = if_else(methane_hot_minus_cold >= 0,
                             "Higher when hot", "Higher when cold")) %>%
  ggplot(aes(x = methane_hot_minus_cold, y = SITE_ID_ordered, color = direction)) +
  geom_vline(xintercept = 0, color = "grey55", linewidth = 0.35) +
  geom_segment(aes(x = 0, xend = methane_hot_minus_cold, yend = SITE_ID_ordered),
               linewidth = 0.5) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Higher when hot" = "#b2182b", "Higher when cold" = "#2166ac")) +
  labs(x = "Hot - cold methane anomaly (within site-month normal)", y = "Site", color = NULL,
       title = "Per-site methane response to temperature anomalies") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(size = 6))

ggsave(file.path(anomaly_figure_dir, "site_hot_minus_cold_methane.png"),
       site_hot_cold_figure, width = 7.5, height = 8, dpi = 300)

cat("Wrote temperature-anomaly (hot/normal/cold) outputs to:", anomaly_output_dir, "\n")

cat("Wrote condition-change analysis outputs to:", output_dir, "\n")
