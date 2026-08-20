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

# Resolve the folder holding config.R robustly (see 05_CompileData.R).
.a <- commandArgs(FALSE); .f <- .a[grepl("^--file=", .a)]
.cand <- if (length(.f)) dirname(normalizePath(sub("^--file=", "", .f[1]), mustWork = FALSE)) else character(0)
.cand <- c(.cand, getwd(), file.path(getwd(), "CH4_Drought"),
           "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
.hit <- .cand[file.exists(file.path(.cand, "config.R"))]
if (!length(.hit)) stop("Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
analysis_dir <- .hit[1]
source(file.path(analysis_dir, "config.R"))

# FinalDrought_Data.RDATA holds fluxes.drought_normalized (built in 05_CompileData.R).
# Prefer a local data/ copy; fall back to the lab server.
project.data.dir <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
final_local  <- file.path(analysis_dir, "data", "FinalDrought_Data.RDATA")
final_server <- file.path(project.data.dir, "FinalDrought_Data.RDATA")
final_file <- if (file.exists(final_local)) final_local else final_server
if (!file.exists(final_file))
  stop("FinalDrought_Data.RDATA not found locally or on the server. Run 05_CompileData.R first.")
load(final_file)

# Drop the sf geometry column if present: this script does not load {sf}, so a
# leftover sfc list-column makes dplyr::filter() error ("must be a vector, not a
# <sfc_POINT>") under recent dplyr/vctrs. Removing it keeps DroughtAnalysis clean
# for all downstream steps (07/11/21/21b) that likewise do not load {sf}.
fluxes.drought_normalized <- as.data.frame(fluxes.drought_normalized)
fluxes.drought_normalized[["geometry"]] <- NULL

# Drop non-target IGBP classes (correct set-membership test, not vectorized !=).
# Class list is centralized in config.R (EXCLUDE_IGBP) so 05/06/10 stay consistent.
rm.INGBP <- EXCLUDE_IGBP
fluxes.drought_normalized <- fluxes.drought_normalized %>%
  filter(!IGBP %in% rm.INGBP, !SITE_ID %in% EXCLUDE_SITES)

out_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
save(fluxes.drought_normalized, file = out_file)

message("Wrote canonical analysis table: ", out_file,
        " (", nrow(fluxes.drought_normalized), " rows, ",
        length(unique(fluxes.drought_normalized$SITE_ID)), " sites)")
