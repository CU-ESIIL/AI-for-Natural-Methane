# io_helpers.R
# ---------------------------------------------------------------------------
# Shared output helpers. Every analysis output is written to the project
# outputs/ folder and, when WRITE_TO_SERVER is TRUE and the server is mounted,
# mirrored to SERVER_OUTPUTS (see config.R). This guarantees the "correct files
# are created and added to the server" for downstream scripts and collaborators.
#
# `relpath` is the path *relative to* outputs/, e.g.
#   "tem_mdm_model_outputs/TEM_MDM_daily_CH4_conditions.csv".
# ---------------------------------------------------------------------------

.server_enabled <- function() {
  isTRUE(get0("WRITE_TO_SERVER", ifnotfound = FALSE)) &&
    nzchar(get0("SERVER_DIR", ifnotfound = "")) &&
    dir.exists(get0("SERVER_DIR", ifnotfound = ""))
}

output_targets <- function(relpath, analysis_dir) {
  targets <- file.path(analysis_dir, "outputs", relpath)
  if (.server_enabled()) {
    targets <- c(targets, file.path(get0("SERVER_OUTPUTS"), relpath))
  }
  targets
}

save_output_csv <- function(df, relpath, analysis_dir, row.names = FALSE) {
  for (path in output_targets(relpath, analysis_dir)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(df, path, row.names = row.names)
    message("Wrote: ", path)
  }
  invisible(file.path(analysis_dir, "outputs", relpath))
}

save_output_rdata <- function(..., relpath, analysis_dir) {
  objs <- list(...)
  nms <- names(objs)
  for (path in output_targets(relpath, analysis_dir)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    e <- new.env()
    for (i in seq_along(objs)) assign(nms[i], objs[[i]], envir = e)
    save(list = nms, file = path, envir = e)
    message("Wrote: ", path)
  }
  invisible(file.path(analysis_dir, "outputs", relpath))
}

# `draw_fun` is a zero-argument function that issues base-graphics plotting
# calls; it is re-invoked once per output device (project + server).
save_output_figure <- function(draw_fun, relpath, analysis_dir,
                                width = 2200, height = 1400, res = 180) {
  for (path in output_targets(relpath, analysis_dir)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    grDevices::png(path, width = width, height = height, res = res)
    draw_fun()
    grDevices::dev.off()
  }
  invisible(file.path(analysis_dir, "outputs", relpath))
}

# For ggplot objects: save the same plot to every target path.
save_output_ggplot <- function(plot, relpath, analysis_dir,
                                width = 9, height = 6, dpi = 300) {
  for (path in output_targets(relpath, analysis_dir)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = dpi)
    message("Wrote: ", path)
  }
  invisible(file.path(analysis_dir, "outputs", relpath))
}
