# 19b_CalibrateInundationElasticities.R
# ---------------------------------------------------------------------------
# CALIBRATE the inundation-area response used by 19c from OBSERVATIONS, instead
# of assuming it. Regresses interannual/seasonal wetland-area anomalies (WAD2M)
# on the standardized moisture anomaly (ECMWF SPEI, the same product 04 uses),
# separately for the DRY and WET halves of the SPEI axis, per latitude band. The
# fitted slopes ARE the elasticities 19c needs (e_dry, e_wet), and a year-block
# bootstrap gives their real uncertainty. An optional temperature term is fit as
# a diagnostic if a gridded temperature file is supplied.
#
# WHY THIS MAPS TO 19c
# --------------------
# 19c models the log area factor (baseline = 0) as
#     log A = e_wet * (wet SPEI forcing) - e_dry * (dry SPEI forcing)
# i.e. a piecewise-linear function of the SPEI anomaly x, through the origin:
#     log A = e_wet * max(x, 0) + e_dry * min(x, 0)
# So fitting  y = log(Fw / Fw_clim)  on  [max(x,0), min(x,0)]  with NO intercept
# yields  b_pos = e_wet  and  b_neg = e_dry  directly, both expected > 0. If the
# data say e_dry > e_wet, the assumed asymmetry (drainage easier than reflooding)
# is confirmed empirically; if not, 19c inherits the observed relationship.
#
# METHOD
# ------
#  * Anomalies: SPEI is already a standardized anomaly, used as x directly. Area
#    anomaly y = log(Fw / per-cell per-calendar-month climatology), so the fit is
#    through the origin and seasonality is removed.
#  * Cells with tiny climatological wetland area (< INUND_CAL_MIN_FW) are dropped.
#  * Observations are area-weighted (long-term wetland area per cell), so large
#    wetlands dominate -- consistent with the area-weighted budget in 19/19c.
#  * Fit is a weighted least squares through the origin; solved from per-year
#    sufficient statistics (X'WX, X'Wy) so the YEAR-BLOCK BOOTSTRAP (resample
#    whole years with replacement) is exact and fast. Blocking by year gives
#    honest uncertainty despite spatial+temporal autocorrelation.
#
# Writes:
#   data/inundation_elasticities.csv    region, e_dry(+se,ci), e_wet(+se,ci), ...
#   outputs/regional_projection_inundation/figures/inundation_calibration.png
#   outputs/regional_projection_inundation/inundation_calibration_fit.csv
#
# Then re-run 19c: it auto-detects data/inundation_elasticities.csv and uses the
# fitted per-band elasticities (drawn from their sampling distribution in the MC)
# in place of the hard-coded defaults.
#
# Requires: terra. Server volume with WAD2M_FILE (config.R) and the ECMWF SPEI
# directory must be mounted.
# ---------------------------------------------------------------------------

rm(list = ls())
locate_analysis_dir <- function() {
  cand <- character(0)
  args <- commandArgs(FALSE)
  f <- args[grepl("^--file=", args)]
  if (length(f)) cand <- c(cand, dirname(normalizePath(sub("^--file=", "", f[1]), mustWork = FALSE)))
  for (i in seq_len(sys.nframe())) {
    of <- sys.frame(i)$ofile
    if (!is.null(of)) cand <- c(cand, dirname(normalizePath(of, mustWork = FALSE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) cand <- c(cand, dirname(normalizePath(p, mustWork = FALSE)))
  }
  cand <- c(cand, getwd(), file.path(getwd(), "CH4_Drought"))
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) return(hit[1])
  stop("Could not find config.R. Set the working directory to the CH4_Drought folder.")
}
analysis_dir <- locate_analysis_dir()
message("analysis_dir: ", analysis_dir)
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
suppressPackageStartupMessages(library(terra))
set.seed(7)
out_rel <- function(f) file.path("regional_projection_inundation", f)

# ---- calibration settings (editable; get0 fallbacks -> no config.R edit) ----
ECMWF_DROUGHT_DIR <- get0("ECMWF_DROUGHT_DIR",
  ifnotfound = "/Volumes/MaloneLab/Research/Natural_CH4_CO2/Drought/ECMWF_DroughtIndices_Global")
INUND_CAL_INDEX   <- get0("INUND_CAL_INDEX",  ifnotfound = get0("DROUGHT_INDEX", ifnotfound = "SPEI1"))
INUND_CAL_MIN_FW  <- get0("INUND_CAL_MIN_FW", ifnotfound = 0.01)  # min clim. wetland fraction to keep a cell
INUND_CAL_BOOT    <- get0("INUND_CAL_BOOT",   ifnotfound = 500)   # year-block bootstrap reps
INUND_CAL_TEMP_FILE <- get0("INUND_CAL_TEMP_FILE", ifnotfound = "") # optional gridded temperature nc (diagnostic)
INUND_ELASTICITY_FILE <- get0("INUND_ELASTICITY_FILE", ifnotfound = "data/inundation_elasticities.csv")
# Fallback start (YYYY-MM) used to CONSTRUCT a monthly time axis when a file's own
# time metadata is missing/undecoded. WAD2M is monthly from 2000-01. Leave SPEI
# empty unless its time axis fails to decode (the diagnostics below will say so).
WAD2M_START <- get0("WAD2M_START", ifnotfound = "2000-01")
SPEI_START  <- get0("SPEI_START",  ifnotfound = "")

reg_cfg <- read.csv(file.path(analysis_dir, REGION_CONFIG), stringsAsFactors = FALSE)
band_of <- function(lat) {
  a <- abs(lat)
  reg_cfg$region[vapply(a, function(x) {
    j <- which(x > reg_cfg$abs_lat_min - 1e-9 & x <= reg_cfg$abs_lat_max + 1e-9)[1]
    if (is.na(j)) NA_integer_ else j
  }, integer(1))]
}

# Robustly derive one "YYYY-MM" per layer. Tries (1) the raster time axis, (2) a
# date token in the layer names, (3) a constructed monthly sequence from `start`.
# Prints a diagnostic so mismatches are visible if the overlap is still empty.
guess_ym <- function(r, label, start = NULL) {
  n <- terra::nlyr(r)
  tt <- tryCatch(terra::time(r), error = function(e) NULL)
  ym <- NULL
  if (!is.null(tt) && length(tt) == n) {
    d <- suppressWarnings(as.Date(tt))
    if (sum(!is.na(d)) == n) {
      yrs <- as.integer(format(d, "%Y"))
      if (is.finite(diff(range(yrs))) && diff(range(yrs)) >= 1) ym <- format(d, "%Y-%m")
    }
  }
  if (is.null(ym)) {                       # try a date token in layer names
    nm <- names(r)
    tok <- regmatches(nm, regexpr("(1[89]|20)[0-9]{2}[-_.]?(0[1-9]|1[0-2])", nm))
    if (length(tok) == n) { mm <- gsub("[-_.]", "", tok)
      ym <- paste0(substr(mm, 1, 4), "-", substr(mm, 5, 6)) }
  }
  if (is.null(ym) && !is.null(start) && nzchar(start)) {   # construct monthly seq
    d <- seq(as.Date(paste0(start, "-01")), by = "month", length.out = n)
    ym <- format(d, "%Y-%m")
  }
  message(sprintf("  [%s] nlyr=%d | time head: %s | names head: %s | Y-M: %s..%s (%s unique)",
    label, n,
    paste(utils::head(as.character(tt), 3), collapse = ","),
    paste(utils::head(names(r), 2), collapse = ","),
    if (!is.null(ym)) min(ym) else "NA",
    if (!is.null(ym)) max(ym) else "NA",
    if (!is.null(ym)) length(unique(ym)) else 0))
  if (is.null(ym)) stop("Could not derive year-months for ", label,
    " -- set ", if (grepl("WAD2M", label)) "WAD2M_START" else "SPEI_START",
    " (YYYY-MM) or check the file's time axis.")
  ym
}

# ---- load WAD2M wetland fraction (Fw) as a time-stamped raster ----
if (!file.exists(WAD2M_FILE)) stop("WAD2M_FILE not found (mount the server volume): ", WAD2M_FILE)
fw <- terra::rast(WAD2M_FILE, subds = "Fw")
message("Deriving month labels:")
ym_fw <- guess_ym(fw, "WAD2M", start = WAD2M_START)

# ---- load ECMWF SPEI: each .nc is one month -> stack the index across files ----
nc_files <- list.files(ECMWF_DROUGHT_DIR, pattern = "\\.nc$", full.names = TRUE)
if (!length(nc_files)) stop("No .nc files in ECMWF_DROUGHT_DIR: ", ECMWF_DROUGHT_DIR)
# Pick the layer whose name matches the index EXACTLY (so "SPEI1" never grabs
# "SPEI12"). Handles: index as a sub-dataset, as one of several variables, or a
# single-layer file.
exact_layer <- function(r, index) {
  nm <- names(r)
  sel <- which(nm == index); if (!length(sel)) sel <- which(tolower(nm) == tolower(index))
  if (length(sel)) return(r[[sel[1]]])
  if (terra::nlyr(r) == 1) return(r)
  NULL
}
build_index_series <- function(files, index) {
  lst <- list()
  for (f in files) {
    r <- tryCatch(terra::rast(f, subds = index), error = function(e) NULL)
    if (is.null(r)) r <- tryCatch(terra::rast(f), error = function(e) NULL)
    if (is.null(r)) next
    li <- exact_layer(r, index)
    if (!is.null(li)) lst[[length(lst) + 1]] <- li
  }
  if (!length(lst)) stop("No '", index, "' layers found across ", length(files), " ECMWF files.")
  s <- if (length(lst) == 1) lst[[1]] else do.call(c, lst)
  tt <- tryCatch(terra::time(s), error = function(e) NULL)     # sort chronologically
  if (!is.null(tt) && sum(!is.na(tt)) == terra::nlyr(s)) s <- s[[order(tt)]]
  s
}
spei <- build_index_series(nc_files, INUND_CAL_INDEX)
message("Assembled SPEI series: ", terra::nlyr(spei), " layers from ", length(nc_files), " files.")
ym_sp <- guess_ym(spei, paste0("SPEI/", INUND_CAL_INDEX), start = SPEI_START)

# common months, in WAD2M order
common <- intersect(ym_fw, ym_sp)
common <- common[order(common)]
if (length(common) < 24) stop("Too few overlapping months between WAD2M and SPEI: ", length(common))
fw  <- fw[[match(common, ym_fw)]]
spei <- spei[[match(common, ym_sp)]]
mon <- as.integer(substr(common, 6, 7))
yr  <- as.integer(substr(common, 1, 4))
message("Overlap: ", length(common), " months (", min(yr), "-", max(yr), ")")

# ---- restrict to wetland cells (SPEI is SAMPLED at these cells, not regridded) ----
fw_mean <- terra::app(fw, fun = function(v) mean(v, na.rm = TRUE))   # per-cell long-term mean
area_km <- terra::cellSize(fw, unit = "km")                          # per-cell area
keep <- which(is.finite(terra::values(fw_mean)[, 1]) & terra::values(fw_mean)[, 1] > INUND_CAL_MIN_FW)
if (!length(keep)) stop("No cells exceed INUND_CAL_MIN_FW = ", INUND_CAL_MIN_FW)
xy   <- terra::xyFromCell(fw, keep)
band <- band_of(xy[, 2])
w_cell <- terra::values(fw_mean)[keep, 1] * terra::values(area_km)[keep, 1]  # area weight (km2)

FW <- terra::values(fw)[keep, , drop = FALSE]    # cells x months (WAD2M is a single file)
# Sample SPEI ONLY at the wetland-cell coordinates -- ~|keep| points x months
# instead of resampling the whole globe. Both grids are lon/lat, but some NetCDFs
# carry NO CRS, so we sample with the raw coordinates and only reproject when BOTH
# grids have a valid, differing CRS. Also handle a 0..360 longitude convention.
crs_fw <- terra::crs(fw)
if (terra::xmax(spei) > 180.001) spei <- tryCatch(terra::rotate(spei), error = function(e) spei)
xy_sp <- xy
crs_sp <- terra::crs(spei)
if (nzchar(crs_fw) && nzchar(crs_sp) && !terra::same.crs(fw, spei))
  xy_sp <- terra::crds(terra::project(terra::vect(xy, crs = crs_fw), crs_sp))
SP <- terra::extract(spei, xy_sp)                # matrix input -> data.frame of layer values
if ("ID" %in% names(SP)) SP$ID <- NULL
SP <- as.matrix(SP)                              # cells x months
message("Sampled SPEI at ", length(keep), " wetland cells x ", ncol(SP), " months.")

# Per-cell per-calendar-month climatology in LOG space -> log ANOMALY.
# Centering in log space (mean of log Fw, NOT log of the arithmetic mean) removes
# the Jensen offset so E[y | SPEI=0] ~ 0, which the through-origin elasticity fit
# requires. A small floor keeps dry-down months (Fw -> 0) finite rather than
# censoring exactly the strong-drought signal we care about.
FW_FLOOR <- get0("INUND_CAL_FW_FLOOR", ifnotfound = 1e-4)
LFW  <- log(pmax(FW, FW_FLOOR))
CLIM <- matrix(NA_real_, nrow(LFW), 12)
for (m in 1:12) { cols <- which(mon == m); if (length(cols)) CLIM[, m] <- rowMeans(LFW[, cols, drop = FALSE], na.rm = TRUE) }
Y <- LFW - CLIM[, mon]                            # cells x months (log area anomaly, ~zero per-cell-month mean)

# optional temperature anomaly (standardized per cell-month) as a diagnostic term
have_temp <- nzchar(INUND_CAL_TEMP_FILE) && file.exists(INUND_CAL_TEMP_FILE)
if (have_temp) {
  tr <- terra::rast(INUND_CAL_TEMP_FILE)
  ym_t <- guess_ym(tr, "TEMP", start = get0("TEMP_START", ifnotfound = ""))
  if (length(intersect(common, ym_t)) < length(common))
    stop("Temperature file does not cover all overlap months.")
  tr <- tr[[match(common, ym_t)]]
  if (terra::xmax(tr) > 180.001) tr <- tryCatch(terra::rotate(tr), error = function(e) tr)
  xy_t <- xy; crs_t <- terra::crs(tr)
  if (nzchar(crs_fw) && nzchar(crs_t) && !terra::same.crs(fw, tr))
    xy_t <- terra::crds(terra::project(terra::vect(xy, crs = crs_fw), crs_t))
  TT <- terra::extract(tr, xy_t)
  if ("ID" %in% names(TT)) TT$ID <- NULL
  TT <- as.matrix(TT)                            # cells x months
  TCL <- matrix(NA_real_, nrow(TT), 12); TSD <- TCL
  for (m in 1:12) { cols <- which(mon == m)
    if (length(cols)) { TCL[, m] <- rowMeans(TT[, cols, drop = FALSE], na.rm = TRUE)
                        TSD[, m] <- apply(TT[, cols, drop = FALSE], 1, sd, na.rm = TRUE) } }
  XT <- (TT - TCL[, mon]) / TSD[, mon]           # standardized temperature anomaly
} else XT <- NULL

# ---- assemble long observation table ----
ncell <- nrow(FW); nmon <- ncol(FW)
long <- data.frame(
  band = rep(band, times = nmon),
  year = rep(yr, each = ncell),
  w    = rep(w_cell, times = nmon),
  x    = as.vector(SP),
  y    = as.vector(Y))
if (have_temp) long$xt <- as.vector(XT)
# Drop implausible SPEI: standardized indices essentially never exceed ~|3.5|, so
# larger magnitudes are fill/sentinel values (e.g. -9999 or a NetCDF _FillValue
# read as a real number). Being finite AND negative, such fills otherwise survive
# and, via huge leverage, flatten the dry-side slope to ~0 and zero out R^2.
SPEI_MAX <- get0("INUND_CAL_SPEI_MAX", ifnotfound = 3.5)
ok <- is.finite(long$x) & is.finite(long$y) & is.finite(long$w) & long$w > 0 &
      !is.na(long$band) & abs(long$x) <= SPEI_MAX
if (have_temp) ok <- ok & is.finite(long$xt)
nbad <- sum(is.finite(long$x) & abs(long$x) > SPEI_MAX, na.rm = TRUE)
long <- long[ok, ]
message("Calibration observations: ", format(nrow(long), big.mark = ","),
        " cell-months across ", length(unique(long$year)), " years",
        if (nbad > 0) paste0(" (dropped ", format(nbad, big.mark = ","),
                             " |SPEI|>", SPEI_MAX, " fill/outliers)") else "", ".")

# ---- weighted linear fit  y = a + b*x  (slope b = elasticity), per band --------
# Fit WITH an intercept (absorbs any residual mean offset) and take the SLOPE as
# the elasticity; 19c applies it through the origin (A = 1 at baseline). The dry
# (x<0) and wet (x>=0) halves are fit SEPARATELY, each with its OWN intercept, so
# neither side is forced through 0 -- that constraint was collapsing the dry-side
# slope to exactly zero. Everything is solved from per-year weighted sums so the
# year-block bootstrap is exact and fast.
wsuff <- function(x, y, w) c(Sw = sum(w), Swx = sum(w * x), Swy = sum(w * y),
                             Swxx = sum(w * x * x), Swxy = sum(w * x * y),
                             Swyy = sum(w * y * y), n = length(x))
slope_from <- function(S) {                    # weighted OLS slope + R^2 (with intercept)
  den <- S["Sw"] * S["Swxx"] - S["Swx"]^2
  if (!is.finite(den) || den <= 0 || S["n"] < 3) return(c(b = NA_real_, r2 = NA_real_))
  b <- (S["Sw"] * S["Swxy"] - S["Swx"] * S["Swy"]) / den
  a <- (S["Swy"] - b * S["Swx"]) / S["Sw"]
  ybar <- S["Swy"] / S["Sw"]; sst <- S["Swyy"] - S["Sw"] * ybar^2
  sse <- S["Swyy"] - 2 * (a * S["Swy"] + b * S["Swxy"]) + a^2 * S["Sw"] +
         2 * a * b * S["Swx"] + b^2 * S["Swxx"]
  c(b = as.numeric(b), r2 = as.numeric(1 - sse / sst))
}
suff_year <- function(df) {                    # per-year sums for all / dry / wet subsets
  ys <- split(seq_len(nrow(df)), df$year)
  lapply(ys, function(idx) {
    x <- df$x[idx]; y <- df$y[idx]; w <- df$w[idx]; neg <- x < 0
    list(all = wsuff(x, y, w),
         dry = wsuff(x[neg],  y[neg],  w[neg]),
         wet = wsuff(x[!neg], y[!neg], w[!neg]))
  })
}
combine <- function(stats, years, key) Reduce(`+`, lapply(years, function(y) stats[[y]][[key]]))

bands <- reg_cfg$region
res <- list(); fitlines <- list()
for (bd in bands) {
  sub <- long[long$band == bd, ]
  if (nrow(sub) < 100 || length(unique(sub$year)) < 3) {
    message("  band ", bd, ": too few observations (", nrow(sub), ") -- skipped."); next }
  message(sprintf("  band %-9s obs=%s (neg=%s, pos=%s) years=%d  mean(y)=%.4f",
    bd, format(nrow(sub), big.mark = ","),
    format(sum(sub$x < 0), big.mark = ","), format(sum(sub$x > 0), big.mark = ","),
    length(unique(sub$year)),
    sum(sub$w * sub$y) / sum(sub$w)))
  stats <- suff_year(sub); yrs <- names(stats)
  fitb <- function(years) c(
    wet = as.numeric(slope_from(combine(stats, years, "wet"))["b"]),
    dry = as.numeric(slope_from(combine(stats, years, "dry"))["b"]),
    all = as.numeric(slope_from(combine(stats, years, "all"))["b"]))
  pt     <- fitb(yrs)
  r2_all <- as.numeric(slope_from(combine(stats, yrs, "all"))["r2"])
  # year-block bootstrap of the dry/wet slopes
  boot <- t(vapply(seq_len(INUND_CAL_BOOT),
                   function(i) fitb(sample(yrs, length(yrs), replace = TRUE)), numeric(3)))
  colnames(boot) <- c("wet", "dry", "all")
  ew <- pt["wet"]; ed <- pt["dry"]; ea <- pt["all"]   # fall back to pooled slope if a half is unidentified
  if (!is.finite(ew)) ew <- ea
  if (!is.finite(ed)) ed <- ea
  qw <- quantile(boot[, "wet"], c(.05, .95), na.rm = TRUE)
  qd <- quantile(boot[, "dry"], c(.05, .95), na.rm = TRUE)
  row <- data.frame(region = bd,
    e_wet = as.numeric(ew), e_wet_se = sd(boot[, "wet"], na.rm = TRUE), e_wet_lo = qw[1], e_wet_hi = qw[2],
    e_dry = as.numeric(ed), e_dry_se = sd(boot[, "dry"], na.rm = TRUE), e_dry_lo = qd[1], e_dry_hi = qd[2],
    n_obs = nrow(sub), n_years = length(yrs), r2 = r2_all)
  res[[bd]] <- row

  # binned fit line for the diagnostic figure (weighted mean y per SPEI bin).
  # Clamp values beyond +/-3 into the end bins so bin indices stay in 1..12.
  br <- seq(-3, 3, by = 0.5); mid <- head(br, -1) + 0.25
  gi <- findInterval(sub$x, br, rightmost.closed = TRUE)
  gi <- pmin(pmax(gi, 1L), length(mid))
  ybar <- tapply(sub$y * sub$w, gi, sum) / tapply(sub$w, gi, sum)
  idx  <- as.integer(names(ybar))
  fitlines[[bd]] <- data.frame(band = bd, xmid = mid[idx], ybar = as.numeric(ybar))
}
if (!length(res)) stop("No bands could be calibrated.")
elas <- do.call(rbind, res)
elas[, sapply(elas, is.numeric)] <- round(elas[, sapply(elas, is.numeric)], 5)

# canonical table where 19c looks for it (data/), plus a record copy in outputs
data_path <- file.path(analysis_dir, INUND_ELASTICITY_FILE)
dir.create(dirname(data_path), recursive = TRUE, showWarnings = FALSE)
write.csv(elas, data_path, row.names = FALSE)
message("Wrote elasticity table: ", data_path)
save_output_csv(elas, out_rel("inundation_elasticities.csv"), analysis_dir)
save_output_csv(do.call(rbind, fitlines), out_rel("inundation_calibration_fit.csv"), analysis_dir)

# NOTE: the calibration diagnostic figure is no longer drawn here. Because this
# script is slow (WAD2M x SPEI calibration + bootstrap), the plotting was moved to
# 19c_RegionalProjection_InundationVarying.R, which rebuilds it from the two CSVs
# written just above (inundation_elasticities.csv + inundation_calibration_fit.csv).
# Re-run 19c to (re)generate outputs/.../figures/inundation_calibration.png without
# repeating this calibration.

message("\nCalibrated inundation elasticities (per band):")
print(elas, row.names = FALSE)
message("\nInterpretation: e_dry > e_wet means area drains faster than it re-floods.")
message("Re-run 19c -- it will auto-detect ", INUND_ELASTICITY_FILE, " and use these values.")
