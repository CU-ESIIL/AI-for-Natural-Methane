# Export variable importance from the refit random forest (run 07 first).
# Reads the stored model in DroughtAnalysis.RDATA and writes a ranking table
# + figure showing where thermal_season sits relative to SPEI and the others.
# No refit is performed.

library(tidyverse)
library(randomForest)

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else file.path(getwd(), "CH4_Drought")
source(file.path(analysis_dir, "config.R"))

load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))
rf <- Normalizex.spei48.model.rf

out_dir <- file.path(analysis_dir, "outputs", "temperature_events")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

pretty <- c(
  SPEI1 = "SPEI (wet-dry)", thermal_season = "Thermal season (winter-summer)",
  temp_class = "Temp anomaly (hot-cold)",
  SLOPE = "Slope", DI.SPEI1.MeanDuration = "Drought duration",
  month = "Month", VPD_F = "VPD", TA_F = "Air temperature", SVWC = "Soil water"
)
temp_axis_labels <- c("Thermal season (winter-summer)", "Temp anomaly (hot-cold)")

imp <- as.data.frame(randomForest::importance(rf)) %>%
  rownames_to_column("variable") %>%
  mutate(label = ifelse(variable %in% names(pretty), pretty[variable], variable)) %>%
  arrange(desc(`%IncMSE`))

write_csv(imp, file.path(out_dir, "rf_variable_importance.csv"))

oob_r2 <- tail(rf$rsq, 1)
cat("OOB pseudo-R2:", round(oob_r2, 3), "\n")
print(imp[, c("label", "%IncMSE", "IncNodePurity")], row.names = FALSE)

imp_long <- imp %>%
  select(label, `%IncMSE`, IncNodePurity) %>%
  pivot_longer(-label, names_to = "metric", values_to = "value")

rf_imp_fig <- imp_long %>%
  ggplot(aes(x = value, y = reorder(label, value),
             color = label %in% temp_axis_labels)) +
  geom_segment(aes(x = 0, xend = value, yend = reorder(label, value)), linewidth = 0.5) +
  geom_point(size = 3) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_color_manual(values = c(`FALSE` = "grey35", `TRUE` = "#b2182b"), guide = "none") +
  labs(x = NULL, y = NULL,
       title = "Random-forest importance for normalized methane flux",
       subtitle = "Temperature axes highlighted") +
  theme_bw()

ggsave(file.path(fig_dir, "rf_variable_importance.png"), rf_imp_fig,
       width = 10, height = 4.5, dpi = 300)

cat("Wrote:", file.path(out_dir, "rf_variable_importance.csv"), "\n")
