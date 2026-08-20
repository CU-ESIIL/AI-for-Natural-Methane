# Build daily TEM-MDM model outputs with drought-condition information.

rm(list = ls())

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file))
} else {
  getwd()
}

source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))

tem_dir <- file.path(analysis_dir, "data", "TEM_MDM_SIMULATIONdata")
metadata_file <- file.path(tem_dir, "FLX_AA-Flx_CH4-META_20201112135337801132.csv")
output_dir <- file.path(analysis_dir, "outputs", "tem_mdm_model_outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

ecmwf_drought_dir <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/Drought/ECMWF_DroughtIndices_Global"
gdal_location_info <- "/Applications/QGIS-final-4_0_3.app/Contents/MacOS/gdallocationinfo"
server_final_drought <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/FinalDrought_Data.RDATA"
local_fluxes_drought <- file.path(analysis_dir, "data", "fluxes_drought.csv")
local_drought_analysis <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
# Pinned cache of the NetCDF-extracted (model-grid) SPEI lookup. build_spei_lookup()
# reads this FIRST so the drought/normal labels are identical on any machine,
# whether or not the ECMWF rasters and QGIS gdallocationinfo are reachable.
local_spei_cache <- file.path(analysis_dir, "data", "TEM_MDM_site_month_spei_lookup.csv")

trim <- function(x) trimws(x, whitespace = "[ \t\r\n\uFEFF]")

days_in_month <- function(year, month) {
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_month <- if (month == 12) {
    as.Date(sprintf("%04d-01-01", year + 1))
  } else {
    as.Date(sprintf("%04d-%02d-01", year, month + 1))
  }
  as.integer(next_month - start)
}

read_tem_daily_file <- function(path, value_name, family) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) return(data.frame())

  rows <- vector("list", length(lines))
  for (i in seq_along(lines)) {
    parts <- trim(strsplit(lines[[i]], ",", fixed = TRUE)[[1]])

    if (family == "model_output") {
      lon <- as.numeric(parts[1])
      lat <- as.numeric(parts[2])
      area <- as.numeric(parts[8])
      year <- as.integer(parts[9])
      month <- as.integer(parts[10])
      total <- as.numeric(parts[11])
      max_value <- as.numeric(parts[12])
      mean_value <- as.numeric(parts[13])
      min_value <- as.numeric(parts[14])
      day_start <- 15
    } else if (family == "driver_daily") {
      lon <- as.numeric(parts[1])
      lat <- as.numeric(parts[2])
      area <- as.numeric(parts[4])
      year <- as.integer(parts[5])
      month <- as.integer(parts[6])
      total <- as.numeric(parts[7])
      max_value <- as.numeric(parts[8])
      mean_value <- as.numeric(parts[9])
      min_value <- as.numeric(parts[10])
      day_start <- 11
    } else {
      stop("Unknown TEM daily family: ", family)
    }

    n_days <- days_in_month(year, month)
    site_id <- trim(parts[length(parts)])
    values <- as.numeric(parts[day_start:(day_start + n_days - 1)])
    dates <- as.Date(sprintf("%04d-%02d-%02d", year, month, seq_len(n_days)))

    rows[[i]] <- data.frame(
      SITE_ID = site_id,
      Date = dates,
      YearMon = format(dates, "%Y-%m"),
      year = year,
      month = month,
      lon_model_grid = lon,
      lat_model_grid = lat,
      area = area,
      value = values,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  names(out)[names(out) == "value"] <- value_name
  out
}

merge_daily <- function(x, y) {
  if (nrow(x) == 0) return(y)
  merge(
    x,
    y,
    by = c("SITE_ID", "Date", "YearMon", "year", "month", "lon_model_grid", "lat_model_grid", "area"),
    all = TRUE,
    sort = FALSE
  )
}

read_daily_family <- function(file_stub, value_name, family, required = TRUE) {
  files <- list.files(
    tem_dir,
    pattern = paste0("^", file_stub, "-[0-9]+$"),
    recursive = TRUE,
    full.names = TRUE
  )
  files <- files[order(as.integer(sub(".*-", "", files)))]
  message("Reading ", length(files), " files for ", value_name)
  if (length(files) == 0) {
    if (required) stop("No files found for required daily family: ", file_stub)
    message("Skipping optional daily family: ", value_name)
    return(NULL)
  }
  do.call(rbind, lapply(files, read_tem_daily_file, value_name = value_name, family = family))
}

extract_spei_lookup_from_netcdf <- function(site_locations) {
  if (!dir.exists(ecmwf_drought_dir) || !file.exists(gdal_location_info)) {
    return(NULL)
  }

  spei_indices <- c("SPEI1", "SPEI3", "SPEI6", "SPEI12", "SPEI24", "SPEI36", "SPEI48")
  year_months <- format(seq(as.Date("2006-01-01"), as.Date("2019-12-01"), by = "month"), "%Y%m")
  site_locations <- site_locations[!is.na(site_locations$LAT) & !is.na(site_locations$LON), ]

  # ECMWF files are 0.25-degree rasters with this grid definition.
  site_locations$pixel_x <- floor((site_locations$LON + 180.125) / 0.25)
  site_locations$pixel_y <- floor((90.125 - site_locations$LAT) / 0.25)
  pixel_input <- paste(site_locations$pixel_x, site_locations$pixel_y)

  lookup <- expand.grid(
    SITE_ID = site_locations$SITE_ID,
    YearMon = format(seq(as.Date("2006-01-01"), as.Date("2019-12-01"), by = "month"), "%Y-%m"),
    stringsAsFactors = FALSE
  )
  lookup <- lookup[order(lookup$SITE_ID, lookup$YearMon), ]

  message("Extracting SPEI directly from NetCDF files in ", ecmwf_drought_dir)
  for (spei_index in spei_indices) {
    lookup[[spei_index]] <- NA_real_

    for (year_month in year_months) {
      file_pattern <- paste0("^", spei_index, "_.*_", year_month, "\\.nc$")
      nc_file <- list.files(ecmwf_drought_dir, pattern = file_pattern, full.names = TRUE)
      if (length(nc_file) == 0) next
      nc_file <- nc_file[1]

      values <- suppressWarnings(system2(
        gdal_location_info,
        args = c("-valonly", nc_file),
        input = pixel_input,
        stdout = TRUE,
        stderr = TRUE
      ))
      values <- suppressWarnings(as.numeric(values[seq_along(pixel_input)]))
      values[!is.finite(values) | values > 1e20] <- NA_real_

      year_mon <- paste0(substr(year_month, 1, 4), "-", substr(year_month, 5, 6))
      row_index <- match(
        paste(site_locations$SITE_ID, year_mon),
        paste(lookup$SITE_ID, lookup$YearMon)
      )
      lookup[[spei_index]][row_index] <- values
    }
  }

  lookup
}

build_spei_lookup <- function(site_locations = NULL) {
  # 1. Pinned cache: a SPEI lookup previously extracted from the ECMWF NetCDFs at
  #    the model grid cells and cached to data/. Reading it first makes the
  #    drought/normal labels reproducible regardless of whether the ECMWF raster
  #    directory and QGIS gdallocationinfo are reachable on this machine.
  #    Delete data/TEM_MDM_site_month_spei_lookup.csv to force a fresh extraction.
  if (file.exists(local_spei_cache)) {
    message("Using cached SPEI lookup: ", local_spei_cache)
    cached <- read.csv(local_spei_cache, stringsAsFactors = FALSE, check.names = FALSE)
    if (!is.null(site_locations)) {
      missing <- setdiff(unique(site_locations$SITE_ID), unique(cached$SITE_ID))
      if (length(missing))
        warning("Cached SPEI lookup is missing ", length(missing), " model site(s): ",
                paste(missing, collapse = ", "),
                ". Delete ", local_spei_cache, " to re-extract from NetCDF.")
    }
    return(cached)
  }

  # 2. Fresh extraction from the ECMWF NetCDFs at each model grid cell; cache it
  #    to data/ so every subsequent run (anywhere) reuses the identical labels.
  if (!is.null(site_locations)) {
    netcdf_lookup <- extract_spei_lookup_from_netcdf(site_locations)
    if (!is.null(netcdf_lookup)) {
      dir.create(dirname(local_spei_cache), recursive = TRUE, showWarnings = FALSE)
      write.csv(netcdf_lookup, local_spei_cache, row.names = FALSE)
      message("Cached NetCDF SPEI lookup to: ", local_spei_cache)
      return(netcdf_lookup)
    }
  }

  # 3. Fallback sources (used only when no cache exists AND NetCDF is unreachable).
  spei_cols <- c("SPEI1", "SPEI3", "SPEI6", "SPEI12", "SPEI24", "SPEI36", "SPEI48",
                 "SPI1", "SPI3", "SPI6", "SPI12", "SPI24", "SPI36", "SPI48")

  if (file.exists(server_final_drought)) {
    message("Using SPEI lookup from server FinalDrought_Data.RDATA")
    load(server_final_drought)
    source_data <- if (exists("Drought.DF.final")) Drought.DF.final else fluxes.drought
  } else if (file.exists(local_fluxes_drought)) {
    message("Using SPEI lookup from local fluxes_drought.csv")
    source_data <- read.csv(local_fluxes_drought, stringsAsFactors = FALSE)
  } else if (file.exists(local_drought_analysis)) {
    message("Using SPEI lookup from local DroughtAnalysis.RDATA")
    load(local_drought_analysis)
    source_data <- fluxes.drought_normalized
  } else {
    warning("No SPEI lookup source found; output will have model data without SPEI.")
    return(data.frame(SITE_ID = character(), YearMon = character()))
  }

  keep_cols <- intersect(c("SITE_ID", "YearMon", spei_cols), names(source_data))
  source_data <- source_data[keep_cols]
  source_data <- source_data[!is.na(source_data$SITE_ID) & !is.na(source_data$YearMon), , drop = FALSE]
  source_data <- source_data[!duplicated(source_data[c("SITE_ID", "YearMon")]), , drop = FALSE]
  source_data
}

add_site_normalized_flux <- function(data, flux_col, output_col) {
  index <- data[[DROUGHT_INDEX]]
  normal_rows <- !is.na(index) & index > NORMAL_LOWER & index < NORMAL_UPPER
  baselines <- aggregate(data[[flux_col]][normal_rows], by = list(SITE_ID = data$SITE_ID[normal_rows]), FUN = mean, na.rm = TRUE)
  names(baselines)[2] <- paste0(flux_col, "_normal")
  data <- merge(data, baselines, by = "SITE_ID", all.x = TRUE, sort = FALSE)
  data[[output_col]] <- data[[flux_col]] - data[[paste0(flux_col, "_normal")]]
  data
}

summarise_condition <- function(data, scenario_name, flux_col, normalized_col) {
  ok <- !is.na(data[[flux_col]]) & !is.na(data$condition)
  aggregate(
    data.frame(
      n_days = rep(1, sum(ok)),
      flux = data[[flux_col]][ok],
      normalized_flux = data[[normalized_col]][ok],
      spei = data[[DROUGHT_INDEX]][ok]
    ),
    by = list(scenario = rep(scenario_name, sum(ok)), condition = data$condition[ok]),
    FUN = function(x) c(n = length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
  )
}

summarise_site_condition <- function(data, scenario_name, flux_col, normalized_col) {
  ok <- !is.na(data[[flux_col]]) & !is.na(data$condition)
  summary_input <- data.frame(
    SITE_ID = data$SITE_ID[ok],
    scenario = rep(scenario_name, sum(ok)),
    condition = data$condition[ok],
    flux = data[[flux_col]][ok],
    normalized_flux = data[[normalized_col]][ok],
    stringsAsFactors = FALSE
  )

  by_cols <- c("SITE_ID", "scenario", "condition")
  n_days <- aggregate(summary_input$flux, by = summary_input[by_cols], FUN = length)
  names(n_days)[4] <- "n_days"
  flux_mean <- aggregate(summary_input$flux, by = summary_input[by_cols], FUN = mean, na.rm = TRUE)
  names(flux_mean)[4] <- "flux_mean"
  flux_sd <- aggregate(summary_input$flux, by = summary_input[by_cols], FUN = sd, na.rm = TRUE)
  names(flux_sd)[4] <- "flux_sd"
  normalized_mean <- aggregate(summary_input$normalized_flux, by = summary_input[by_cols], FUN = mean, na.rm = TRUE)
  names(normalized_mean)[4] <- "normalized_flux_mean"
  normalized_sd <- aggregate(summary_input$normalized_flux, by = summary_input[by_cols], FUN = sd, na.rm = TRUE)
  names(normalized_sd)[4] <- "normalized_flux_sd"

  out <- Reduce(function(x, y) merge(x, y, by = by_cols, all = TRUE, sort = FALSE),
                list(n_days, flux_mean, flux_sd, normalized_mean, normalized_sd))
  out[order(out$SITE_ID, out$scenario, out$condition), ]
}

metadata <- read.csv(metadata_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
metadata_keep <- intersect(
  c("SITE_ID", "SITE_NAME", "COUNTRY", "LAT", "LON", "SITE_CLASSIFICATION", "UPLAND_CLASS",
    "IGBP", "KOPPEN", "YEAR_START", "YEAR_END", "UTC_OFFSET", "DOM_VEG"),
  names(metadata)
)
metadata <- metadata[metadata_keep]

daily_tables <- Filter(Negate(is.null), list(
  read_daily_family("modelssm_ch4emi.day", "CH4EMI_modelssm", "model_output"),
  read_daily_family("era5ssm_ch4emi.day", "CH4EMI_era5ssm", "model_output"),
  read_daily_family("modelssm.day", "VSM1_modelssm", "model_output"),
  read_daily_family("FNdPREC2006-2019.tem", "PREC", "driver_daily"),
  read_daily_family("FNdTAIR2006-2019.tem", "TAIR", "driver_daily"),
  read_daily_family("FNdVAPR2006-2019.tem", "VAPR", "driver_daily"),
  read_daily_family("FNdSSM2006-2019.tem", "SSM_era5", "driver_daily"),
  # SMAP L4 starts in 2015, so it is optional for this 2006-2019 workflow.
  # When SPL4SMGP files are absent, the ERA5 soil-moisture driver remains active.
  read_daily_family("SPL4SMGP2006-2019.tem", "SSM_smap_l4", "driver_daily", required = FALSE),
  read_daily_family("FNdSOLR2006-2019.tem", "SOLR", "driver_daily")
))

model_daily <- Reduce(merge_daily, daily_tables)
model_daily <- merge(model_daily, metadata, by = "SITE_ID", all.x = TRUE, sort = FALSE)

site_locations <- unique(model_daily[c("SITE_ID", "LAT", "LON")])
spei_lookup <- build_spei_lookup(site_locations)

# --- Belt-and-suspenders SPEI1 guard -------------------------------------------
# The drought/normal labels and the normalized baselines are all keyed on
# DROUGHT_INDEX; this workflow (and the FLUXNET comparison) assume SPEI1. Fail
# loudly rather than silently producing a mislabeled comparison.
if (!identical(DROUGHT_INDEX, "SPEI1"))
  stop("DROUGHT_INDEX is '", DROUGHT_INDEX, "' but this script requires 'SPEI1' ",
       "(set DROUGHT_INDEX <- \"SPEI1\" in config.R).")
if (!"SPEI1" %in% names(spei_lookup))
  stop("SPEI lookup has no SPEI1 column - check the cache/NetCDF source (columns: ",
       paste(names(spei_lookup), collapse = ", "), ").")
if (sum(!is.na(spei_lookup[["SPEI1"]])) == 0)
  stop("SPEI1 column is empty (all NA). The NetCDF extraction or cached lookup ",
       "returned no values; delete data/TEM_MDM_site_month_spei_lookup.csv and re-run ",
       "with the ECMWF rasters mounted.")

save_output_csv(spei_lookup, "tem_mdm_model_outputs/TEM_MDM_site_month_spei_lookup.csv", analysis_dir)

if (nrow(spei_lookup) > 0) {
  model_daily <- merge(model_daily, spei_lookup, by = c("SITE_ID", "YearMon"), all.x = TRUE, sort = FALSE)
}

model_daily$condition <- ifelse(
  is.na(model_daily[[DROUGHT_INDEX]]),
  NA_character_,
  ifelse(
    model_daily[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD,
    "drought",
    ifelse(model_daily[[DROUGHT_INDEX]] >= WET_THRESHOLD, "extreme_wet", "normal")
  )
)

model_daily <- add_site_normalized_flux(model_daily, "CH4EMI_modelssm", "normalized_CH4EMI_modelssm")
model_daily <- add_site_normalized_flux(model_daily, "CH4EMI_era5ssm", "normalized_CH4EMI_era5ssm")

# Add the hot/normal/cold temperature-anomaly axis (and winter/summer season)
# from the model's own air temperature, so the compound moisture x temperature
# comparison in 15_Compare_FLUXNET_Models.R can be run on the model output too.
if ("TAIR" %in% names(model_daily)) {
  model_daily <- add_thermal_season(model_daily, ta_col = "TAIR")
  model_daily <- add_temp_anomaly_class(model_daily, ta_col = "TAIR")
}

model_daily <- model_daily[order(model_daily$SITE_ID, model_daily$Date), ]

save_output_csv(model_daily, "tem_mdm_model_outputs/TEM_MDM_daily_CH4_conditions.csv", analysis_dir)
save_output_rdata(model_daily = model_daily,
                  relpath = "tem_mdm_model_outputs/TEM_MDM_daily_CH4_conditions.RDATA",
                  analysis_dir = analysis_dir)

condition_summary <- rbind(
  summarise_site_condition(model_daily, "modelssm", "CH4EMI_modelssm", "normalized_CH4EMI_modelssm"),
  summarise_site_condition(model_daily, "era5ssm", "CH4EMI_era5ssm", "normalized_CH4EMI_era5ssm")
)
save_output_csv(condition_summary, "tem_mdm_model_outputs/TEM_MDM_site_condition_summary.csv", analysis_dir)

site_normal <- condition_summary[condition_summary$condition == "normal", ]
site_normal <- site_normal[, c("SITE_ID", "scenario", "flux_mean", "normalized_flux_mean")]
names(site_normal)[3:4] <- c("normal_flux_mean", "normal_normalized_flux_mean")

site_deltas <- merge(
  condition_summary[condition_summary$condition != "normal", ],
  site_normal,
  by = c("SITE_ID", "scenario"),
  all.x = TRUE,
  sort = FALSE
)
site_deltas$delta_flux_vs_normal <- site_deltas$flux_mean - site_deltas$normal_flux_mean
site_deltas$delta_normalized_flux_vs_normal <- site_deltas$normalized_flux_mean - site_deltas$normal_normalized_flux_mean
save_output_csv(site_deltas, "tem_mdm_model_outputs/TEM_MDM_site_condition_deltas_vs_normal.csv", analysis_dir)

message("Wrote TEM-MDM daily outputs + summaries to project outputs",
        if (.server_enabled()) " and server" else "")
message("Rows: ", nrow(model_daily), "; sites: ", length(unique(model_daily$SITE_ID)))
message("Rows with ", DROUGHT_INDEX, ": ", sum(!is.na(model_daily[[DROUGHT_INDEX]])))
