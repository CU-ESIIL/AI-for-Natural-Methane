# RENAMED -> use run_pipeline.R (documented pipeline runner; default runs the
# 08-14b model/projection block, with data / analysis / regional / all groups available).
# This file forwards for convenience and is safe to delete.
message("run_all.R is renamed to run_pipeline.R -- forwarding.")
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "run_pipeline.R"))
