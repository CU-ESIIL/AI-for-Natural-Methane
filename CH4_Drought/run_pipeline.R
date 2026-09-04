# run_pipeline.R
# ===========================================================================
# Pipeline runner for the CH4 extreme-event workflow. Named for its headline
# block: the model-comparison + projection steps 13 -> 21, which run in order.
# It also knows the upstream data-build (01-05) and analysis/figures (06-12),
# and can run everything.
#
# WHAT EACH SCRIPT DOES (dependencies in parentheses):
#   -- data build (needs the lab server volume) --
#   01_CompileFluxnet.R      Compile raw FLUXNET-CH4 half-hourly/daily -> Fluxnet_Data.RDATA
#   02_Terra_SVWC.R           TerraClimate soil-water at sites -> TERRA_SWC_FLUXNETSITES.csv
#   03_Topography.R           Site terrain covariates (slope, TPI, ...) -> Fluxnet_Topography.csv
#   04_DroughtIndices_Sites.R Extract ECMWF SPEI/SPI at sites, join to daily fluxes
#   05_CompileData.R          Merge fluxes + drought + EVI + SVWC + topo; normalize CH4;
#                             add winter/summer season -> FinalDrought_Data.RDATA
#   -- analysis + figures --
#   06_BuildAnalysisTable.R   Build the canonical analysis table and write
#                             data/DroughtAnalysis.RDATA, the shared input for 07-21
#   07_ConditionChanges.R     Moisture x temperature (seasonal & hot/cold anomaly) condition
#                             analysis, event grids, two-way models, figures
#   08_RefitRandomForest.R    Refit RF for normalized CH4 (SPEI + season + temp anomaly ...)
#   09_RF_Importance.R       Export RF variable-importance table + figure
#   10_Figure_MAP.R           Site maps (drought/extreme-wet), SPEI distributions;
#                             saves SiteList.RDATA used by 12_Q10 (run before Q10)
#   11_Linear.R               Site FCH4~SPEI, stratified by season and hot/cold anomaly
#   12_Q10.R                  Bayesian Q10/Rref by site & period; Q10 vs SPEI by season/anomaly
#   -- models + projection (the 13-21 block) --
#   13_TEM_MDM_ModelOutputs.R Build TEM-MDM daily table (+ temp_class) -> daily conditions
#   14_FLUXNET_ShortLongDrought.R  FLUXNET short- vs long-term (SPEI window) drought responses
#   15_Compare_FLUXNET_Models.R    FLUXNET vs every registered model (models.R): moisture axis
#                             + compound moisture x temperature grid (Fig 6 built by 22_Figures.R)
#   16_DroughtResponseCurves_SPEI.R  CH4-vs-SPEI response curves, FLUXNET + all models
#   17_ScenarioFrequencies_CMIP6.R   Future hot/cold/dry/wet frequencies per SSP (CMIP6/AR6)
#   18_ExtremeEmissionsProjection.R  Additional wetland CH4 to 2100 (IPF joint + MC range)
#   19_RegionalProjection.R          Region-weighted global projection by latitude band
#   20_ContinentChoropleth.R        Continent choropleth from region-weighted contributions
#   21_SensitivityAndDiagnostics.R   Sensitivity checks and diagnostic flags
#   21b_AxisOrthogonality.R          SPEI (moisture) vs STI (temperature) axis
#                             (non-)orthogonality: correlation, chi-square/Cramer's V,
#                             observed-vs-independence joint occupancy, VIF
#
# Every output is written to outputs/ and mirrored to the server (io_helpers.R).
#
# USAGE:
#   Rscript run_pipeline.R              # default: the 13-21 model/projection block
#   Rscript run_pipeline.R analysis     # 06-12 analysis + figures (incl. 10 map)
#   Rscript run_pipeline.R data         # 01-05 upstream data build
#   Rscript run_pipeline.R regional     # 19 regional projection + continent map
#   Rscript run_pipeline.R all          # data -> analysis -> models (everything)
#   Rscript run_pipeline.R refresh      # 05 -> analysis -> models (rebuild after a
#                                       #   site-selection change; SKIPS the slow 01-04)
# ===========================================================================

# Locate the analysis directory (the folder holding config.R). Robust to being
# run via `Rscript`, `source()`, or the RStudio "Source" button — in the latter
# two cases there is no --file argument, so getwd() alone can point one level up.
locate_analysis_dir <- function() {
  cand <- character(0)
  args <- commandArgs(FALSE)
  f <- args[grepl("^--file=", args)]
  if (length(f)) cand <- c(cand, dirname(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE)))
  for (i in seq_len(sys.nframe())) {                 # sourced file: frame$ofile
    of <- sys.frame(i)$ofile
    if (!is.null(of)) cand <- c(cand, dirname(normalizePath(of, mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) cand <- c(cand, dirname(normalizePath(p, mustWork = FALSE)))
  }
  cand <- c(cand, getwd(), file.path(getwd(), "CH4_Drought"),
            "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) return(hit[1])
  stop("Could not find config.R. Set the working directory to the CH4_Drought folder, ",
       "or run: Rscript run_pipeline.R")
}
analysis_dir <- locate_analysis_dir()
message("analysis_dir: ", analysis_dir)
source(file.path(analysis_dir, "config.R"))

STOP_ON_ERROR <- FALSE   # TRUE aborts the whole run on the first failure

# one-line description per script (printed as each step runs)
STEP_DOC <- c(
  "06_BuildAnalysisTable.R"        = "build canonical analysis table -> data/DroughtAnalysis.RDATA",
  "01_CompileFluxnet.R"           = "compile raw FLUXNET-CH4 -> Fluxnet_Data.RDATA",
  "02_Terra_SVWC.R"                = "TerraClimate soil water at sites",
  "03_Topography.R"                = "site terrain covariates",
  "04_DroughtIndices_Sites.R"      = "extract SPEI/SPI at sites",
  "05_CompileData.R"               = "merge + normalize CH4 + add season -> FinalDrought_Data",
  "07_ConditionChanges.R"          = "moisture x temperature condition analysis + grids",
  "08_RefitRandomForest.R"         = "refit random forest for normalized CH4",
  "09_RF_Importance.R"            = "RF variable-importance table + figure",
  "10_Figure_MAP.R"                = "site maps + SPEI distributions; writes SiteList.RDATA",
  "11_Linear.R"                    = "site FCH4~SPEI by season and hot/cold anomaly",
  "12_Q10.R"                       = "Bayesian Q10/Rref; Q10 vs SPEI by season/anomaly",
  "13_TEM_MDM_ModelOutputs.R"      = "build TEM-MDM daily conditions (+ temp_class)",
  "14_FLUXNET_ShortLongDrought.R"  = "FLUXNET short- vs long-term drought responses",
  "15_Compare_FLUXNET_Models.R"    = "FLUXNET vs all models: moisture + compound grid",
  "16_DroughtResponseCurves_SPEI.R"= "CH4-vs-SPEI response curves, FLUXNET + all models",
  "17_ScenarioFrequencies_CMIP6.R" = "future hot/cold/dry/wet frequencies per SSP",
  "18_ExtremeEmissionsProjection.R"= "additional wetland CH4 to 2100 (IPF joint + MC range)",
  "19_RegionalProjection.R"        = "region-weighted global projection, FIXED area (comparison)",
  "19c_RegionalProjection_InundationVarying.R" = "region-weighted projection, VARIABLE inundation (headline)",
  "20_ContinentChoropleth.R"      = "continent choropleth from region-weighted contributions",
  "21_SensitivityAndDiagnostics.R" = "projection sensitivities and diagnostic flags",
  "21b_AxisOrthogonality.R"        = "SPEI vs STI axis (non-)orthogonality: corr, Cramer's V, joint vs independence, VIF",
  "21d_DurationSensitivity.R"      = "projection sensitivity to drought duration (SPEI1 vs SPEI48 response)",
  "22_Figures.R"                   = "rebuild manuscript figures from outputs")

PIPELINE <- list(
  data     = c("01_CompileFluxnet.R", "02_Terra_SVWC.R", "03_Topography.R",
               "04_DroughtIndices_Sites.R", "05_CompileData.R"),
  analysis = c("06_BuildAnalysisTable.R",
               "07_ConditionChanges.R", "08_RefitRandomForest.R", "09_RF_Importance.R",
               "10_Figure_MAP.R", "11_Linear.R", "12_Q10.R", "21b_AxisOrthogonality.R"),
  # 19c (variable inundation) is the headline projection and runs after 19 (fixed,
  # kept for comparison). 19b (elasticity calibration) is intentionally EXCLUDED
  # from the automated run because it is slow (WAD2M x SPEI + bootstrap); 19c reads
  # its saved data/inundation_elasticities.csv, or falls back to config defaults.
  models   = c("13_TEM_MDM_ModelOutputs.R", "14_FLUXNET_ShortLongDrought.R",
               "15_Compare_FLUXNET_Models.R",
               "16_DroughtResponseCurves_SPEI.R",
               "17_ScenarioFrequencies_CMIP6.R", "18_ExtremeEmissionsProjection.R",
               "19_RegionalProjection.R", "19c_RegionalProjection_InundationVarying.R",
               "20_ContinentChoropleth.R",
               "21_SensitivityAndDiagnostics.R", "21b_AxisOrthogonality.R",
               "21d_DurationSensitivity.R"),
  regional = c("19_RegionalProjection.R", "19c_RegionalProjection_InundationVarying.R",
               "20_ContinentChoropleth.R",
               "21_SensitivityAndDiagnostics.R"))
STEP_GROUPS <- list(
  default  = PIPELINE$models,                                   # the 13-21 block
  models   = PIPELINE$models,
  regional = PIPELINE$regional,
  analysis = PIPELINE$analysis,
  data     = PIPELINE$data,
  # Rebuild after a site-selection change (EXCLUDE_IGBP / EXCLUDE_SITES): re-run 05
  # (FinalDrought) through the analysis and model/projection blocks, but skip the slow
  # upstream 01-04, which do not depend on which sites are kept. (steps are de-duped.)
  refresh  = c("05_CompileData.R", PIPELINE$analysis, PIPELINE$models, "22_Figures.R"),
  all      = c(PIPELINE$data, PIPELINE$analysis, PIPELINE$models))

args  <- commandArgs(trailingOnly = TRUE)
group <- if (length(args) >= 1 && args[1] %in% names(STEP_GROUPS)) args[1] else "default"
# De-duplicate while preserving order: 21b_AxisOrthogonality.R appears in both the
# analysis and models blocks, so "all" would otherwise run it twice.
steps <- unique(STEP_GROUPS[[group]])

rscript <- file.path(R.home("bin"), "Rscript")
log_dir <- file.path(analysis_dir, "outputs", "pipeline_log")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
stamp    <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path(log_dir, paste0("run_", group, "_", stamp, ".log"))

record <- data.frame(step = character(), status = character(), seconds = numeric(), stringsAsFactors = FALSE)
cat("Pipeline run:", group, " at", format(Sys.time()), "\n", file = log_file)
message("=== run_pipeline.R : group '", group, "' (", length(steps), " steps) ===")

old_wd <- getwd()
setwd(analysis_dir)
on.exit(setwd(old_wd), add = TRUE)

for (step in steps) {
  path <- file.path(analysis_dir, step)
  desc <- if (step %in% names(STEP_DOC)) STEP_DOC[[step]] else ""
  if (!file.exists(path)) {
    message("  SKIP  ", step, " (not found)")
    record <- rbind(record, data.frame(step = step, status = "missing", seconds = NA)); next
  }
  message(sprintf("  RUN   %-32s %s", step, desc))
  t0 <- Sys.time()
  # Per-step log so a failure's output is not overwritten by the next step.
  step_log <- file.path(log_dir, paste0("run_", group, "_", stamp, "__", sub("\\.R$", "", step), ".log"))
  # Use relative script names from analysis_dir. Passing a quoted absolute path
  # through Rscript can corrupt spaces in this Dropbox path on macOS.
  rc <- system2(rscript, step, stdout = step_log, stderr = step_log, env = character(), wait = TRUE)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  status <- if (rc == 0) "ok" else "FAILED"
  message(sprintf("  %-6s %s (%.1fs)%s", status, step, secs,
                  if (rc != 0) paste0("  -> ", step_log) else ""))
  record <- rbind(record, data.frame(step = step, status = status, seconds = round(secs, 1)))
  if (rc != 0 && STOP_ON_ERROR) { message("Aborting: STOP_ON_ERROR=TRUE"); break }
}

utils::write.csv(record, file.path(log_dir, paste0("run_", group, "_", stamp, "_summary.csv")), row.names = FALSE)
message("\n=== summary (details: ", log_file, ") ===")
print(record, row.names = FALSE)
if (any(record$status == "FAILED")) message("Some steps FAILED - see the log.") else message("All steps completed.")
