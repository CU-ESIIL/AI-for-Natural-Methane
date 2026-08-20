# models.R
# ---------------------------------------------------------------------------
# Registry of process/ML methane models to compare against FLUXNET-CH4.
#
# ==> TO ADD A NEW MODEL: append one entry to MODEL_REGISTRY below. Nothing else
#     in the comparison (10) or projection (12) code needs to change.
#
# Each entry is a list with:
#   daily_rdata  function(analysis_dir) -> path to an .RDATA holding the model's
#                daily table (one row per SITE_ID x Date).
#   daily_object name of the data.frame inside that .RDATA.
#   temp_col     name of the air-temperature column (for the hot/cold anomaly axis).
#   scenarios    named list; each element gives the raw `flux` column and the
#                site-`normalized` flux column for that scenario/driver.
#
# The daily table MUST contain: SITE_ID, Date, month, the temp_col, the SPEI
# columns (or a `condition` column), and each scenario's flux + normalized flux.
# Builder scripts (e.g. 13_TEM_MDM_ModelOutputs.R) are responsible for producing
# a table in this shape and writing it to outputs/ + the server.
# ---------------------------------------------------------------------------

MODEL_REGISTRY <- list(

  `TEM-MDM` = list(
    daily_rdata  = function(analysis_dir) file.path(
      analysis_dir, "outputs", "tem_mdm_model_outputs",
      "TEM_MDM_daily_CH4_conditions.RDATA"),
    daily_object = "model_daily",
    temp_col     = "TAIR",
    scenarios    = list(
      modelssm = list(flux = "CH4EMI_modelssm", normalized = "normalized_CH4EMI_modelssm"),
      era5ssm  = list(flux = "CH4EMI_era5ssm",  normalized = "normalized_CH4EMI_era5ssm")
    )
  )

  # ---- Template for a future model (uncomment, edit, and provide a builder) ----
  # ,`LPJ-wsl` = list(
  #   daily_rdata  = function(analysis_dir) file.path(
  #     analysis_dir, "outputs", "lpj_wsl_model_outputs",
  #     "LPJ_WSL_daily_CH4_conditions.RDATA"),
  #   daily_object = "model_daily",
  #   temp_col     = "TA",
  #   scenarios    = list(
  #     default = list(flux = "CH4", normalized = "normalized_CH4")
  #   )
  # )
)

# Convenience: flatten the registry into one row per (model, scenario).
model_scenario_table <- function(registry = MODEL_REGISTRY) {
  rows <- list()
  for (model_name in names(registry)) {
    m <- registry[[model_name]]
    for (scenario_name in names(m$scenarios)) {
      s <- m$scenarios[[scenario_name]]
      rows[[length(rows) + 1]] <- data.frame(
        model = model_name, scenario = scenario_name,
        flux_col = s$flux, normalized_col = s$normalized,
        temp_col = m$temp_col, stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
