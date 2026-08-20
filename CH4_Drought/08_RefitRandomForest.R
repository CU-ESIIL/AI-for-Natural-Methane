# Refit the stored random-forest model using the configured drought index.

library(tidyverse)
library(randomForest)

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file))
} else {
  file.path(getwd(), "CH4_Drought")
}

source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "temperature_index.R"))

input_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
load(input_file)

if ("geometry" %in% names(fluxes.drought_normalized)) {
  fluxes.drought_normalized$geometry <- NULL
}

# Add the winter/summer (per-site thermal-season) axis so the random forest can
# show how much of the methane signal is a hot vs cold effect, alongside the
# SPEI moisture axis.
if (!"thermal_season" %in% names(fluxes.drought_normalized)) {
  fluxes.drought_normalized <- add_thermal_season(fluxes.drought_normalized)
}
fluxes.drought_normalized$thermal_season <- factor(fluxes.drought_normalized$thermal_season)

# Add the hot/normal/cold temperature-anomaly axis (STI standardized within
# site-month). Drop observations whose site-month baseline is too thin to define
# a class, so the random forest has no missing predictors.
if (!"temp_class" %in% names(fluxes.drought_normalized)) {
  fluxes.drought_normalized <- add_temp_anomaly_class(fluxes.drought_normalized)
}
fluxes.drought_normalized <- fluxes.drought_normalized %>% dplyr::filter(!is.na(temp_class))
fluxes.drought_normalized$temp_class <- factor(fluxes.drought_normalized$temp_class)

required_vars <- c(
  "normalized_Fch4",
  DROUGHT_INDEX,
  "SLOPE",
  DROUGHT_DURATION_VAR,
  "month",
  "VPD_F",
  "TA_F",
  "SVWC",
  "thermal_season",
  "temp_class"
)

missing_vars <- setdiff(required_vars, names(fluxes.drought_normalized))
if (length(missing_vars) > 0) {
  stop("Missing required model columns: ", paste(missing_vars, collapse = ", "))
}

set.seed(111)

fluxes.drought_normalized <- fluxes.drought_normalized %>%
  mutate(
    id = row_number(),
    month = as.factor(month)
  )

train <- fluxes.drought_normalized %>% dplyr::sample_frac(0.80)
test <- dplyr::anti_join(fluxes.drought_normalized, train, by = "id")

rf_formula <- as.formula(paste(
  "normalized_Fch4 ~",
  DROUGHT_INDEX,
  "+ SLOPE +",
  DROUGHT_DURATION_VAR,
  "+ month + VPD_F + TA_F + SVWC + thermal_season + temp_class"
))

Normalizex.spei48.model.rf <- randomForest::randomForest(
  rf_formula,
  data = train,
  importance = TRUE
)
Normalizex.spei48.model.rf$call$formula <- rf_formula

train$PRED <- predict(Normalizex.spei48.model.rf, train)
test$PRED <- predict(Normalizex.spei48.model.rf, test)

sensitivity.df <- function(model, dataframe, factors) {
  vars <- model$importance %>% as.data.frame() %>% row.names()
  vars.no.factors <- setdiff(vars, factors)

  sub.set.mean <- dataframe %>%
    select(all_of(vars.no.factors)) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

  sensitivity_parts <- lapply(vars.no.factors, function(i) {
    var_values <- dataframe[[i]]
    step <- max(abs(var_values), na.rm = TRUE) / 10
    if (!is.finite(step) || step == 0) step <- 1

    target <- tibble(!!i := seq(
      min(var_values, na.rm = TRUE),
      max(var_values, na.rm = TRUE),
      by = step
    ))

    sub.set.mean %>%
      select(-all_of(i)) %>%
      tidyr::crossing(target) %>%
      mutate(target = i)
  })

  sensitivity <- bind_rows(sensitivity_parts)

  for (f in factors) {
    factor_values <- tibble(!!f := unique(dataframe[[f]]))
    sensitivity <- sensitivity %>% tidyr::crossing(factor_values)
  }

  sensitivity$predictions <- predict(model, newdata = sensitivity)
  sensitivity
}

Normalizex.spei48.model.rf.SA.DF <- sensitivity.df(
  model = Normalizex.spei48.model.rf,
  dataframe = fluxes.drought_normalized,
  factors = c("month", "thermal_season", "temp_class")
)

save(
  Normalizex.spei48.model.rf,
  fluxes.drought_normalized,
  Normalizex.spei48.model.rf.SA.DF,
  train,
  test,
  file = input_file
)

cat("Refit random forest with formula:\n")
print(rf_formula)
cat("Saved:", input_file, "\n")
