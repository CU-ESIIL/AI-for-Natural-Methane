# 06_BuildAnalysisTable.R
# ---------------------------------------------------------------------------
# Build the canonical analysis table used by every downstream script (07-21).
# Loads the compiled FinalDrought_Data.RDATA (from 05_CompileData.R), drops the
# non-target IGBP classes, and writes data/DroughtAnalysis.RDATA with the single
# object fluxes.drought_normalized.
#
# The random forest itself is NOT fit here - it is fit and saved back into
# DroughtAnalysis.RDATA by 08_RefitRandomForest.R, and its importance exported by
# 09_RF_Importance.R. This script only exists to produce the shared table.
# ---------------------------------------------------------------------------

library(dplyr)

config_file <- file.path(getwd(), "CH4_Drought", "config.R")
if (!file.exists(config_file)) config_file <- "config.R"
source(config_file)
analysis_dir <- dirname(normalizePath(config_file))

# FinalDrought_Data.RDATA holds fluxes.drought_normalized (built in 05_CompileData.R).
# Prefer a local data/ copy; fall back to the lab server.
project.data.dir <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
final_local  <- file.path(analysis_dir, "data", "FinalDrought_Data.RDATA")
final_server <- file.path(project.data.dir, "FinalDrought_Data.RDATA")
final_file <- if (file.exists(final_local)) final_local else final_server
if (!file.exists(final_file))
  stop("FinalDrought_Data.RDATA not found locally or on the server. Run 05_CompileData.R first.")
load(final_file)

# Drop non-target IGBP classes (correct set-membership test, not vectorized !=).
rm.INGBP <- c("CRO", "URB", "SNO")
fluxes.drought_normalized <- fluxes.drought_normalized %>% filter(!IGBP %in% rm.INGBP)

out_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
save(fluxes.drought_normalized, file = out_file)

message("Wrote canonical analysis table: ", out_file,
        " (", nrow(fluxes.drought_normalized), " rows, ",
        length(unique(fluxes.drought_normalized$SITE_ID)), " sites)")
