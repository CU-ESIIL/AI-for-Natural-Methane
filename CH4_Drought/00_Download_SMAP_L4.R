# Download SMAP L4 point samples through AppEEARS and convert them to the
# TEM-MDM daily driver-file format expected by 13_TEM_MDM_ModelOutputs.R.
#
# Usage:
#   EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R submit
#   EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R status <task_id>
#   EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R download <task_id>
#   Rscript 00_Download_SMAP_L4.R convert <path/to/appeears-point-results.csv>
#   Rscript 00_Download_SMAP_L4.R all

rm(list = ls())

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

api_base <- "https://appeears.earthdatacloud.nasa.gov/api"
product <- "SPL4SMGP.008"
layer <- "Geophysical_Data_sm_surface"
start_date <- "03-31-2015"
end_date <- "12-31-2019"

tem_dir <- file.path(analysis_dir, "data", "TEM_MDM_SIMULATIONdata")
metadata_file <- file.path(tem_dir, "FLX_AA-Flx_CH4-META_20201112135337801132.csv")
download_dir <- file.path(analysis_dir, "data", "SMAP_L4_AppEEARS")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
command <- if (length(args) >= 1) args[1] else "help"

stop_for_response <- function(response) {
  if (status_code(response) >= 300) {
    body <- tryCatch(content(response, as = "text", encoding = "UTF-8"), error = function(e) "")
    stop("HTTP ", status_code(response), ": ", body, call. = FALSE)
  }
  response
}

appeears_token <- function() {
  username <- Sys.getenv("EARTHDATA_USERNAME")
  password <- Sys.getenv("EARTHDATA_PASSWORD")
  if (!nzchar(username) || !nzchar(password)) {
    stop("Set EARTHDATA_USERNAME and EARTHDATA_PASSWORD before submitting/downloading AppEEARS tasks.", call. = FALSE)
  }
  response <- POST(file.path(api_base, "login"), authenticate(username, password))
  parsed <- content(stop_for_response(response), as = "parsed", type = "application/json")
  parsed$token
}

auth_header <- function(token) {
  add_headers(Authorization = paste("Bearer", token))
}

read_sites <- function() {
  metadata <- read.csv(metadata_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
  metadata <- metadata[!is.na(metadata$LAT) & !is.na(metadata$LON), ]
  metadata <- metadata[!duplicated(metadata$SITE_ID), ]
  lapply(seq_len(nrow(metadata)), function(i) {
    list(
      id = as.character(metadata$SITE_ID[i]),
      category = "FLUXNET_CH4",
      latitude = metadata$LAT[i],
      longitude = metadata$LON[i]
    )
  })
}

submit_task <- function() {
  token <- appeears_token()
  task <- list(
    task_type = "point",
    task_name = paste0("CH4_Drought_SMAP_L4_surface_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    params = list(
      dates = list(list(startDate = start_date, endDate = end_date, recurring = FALSE)),
      layers = list(list(product = product, layer = layer)),
      output = list(format = list(type = "csv")),
      coordinates = read_sites()
    )
  )
  request_file <- file.path(download_dir, paste0(task$task_name, "_request.json"))
  write(toJSON(task, auto_unbox = TRUE, pretty = TRUE), request_file)
  response <- POST(file.path(api_base, "task"), auth_header(token), body = task, encode = "json")
  parsed <- content(stop_for_response(response), as = "parsed", type = "application/json")
  message("Submitted AppEEARS task: ", parsed$task_id)
  message("Request JSON: ", request_file)
  invisible(parsed)
}

task_status <- function(task_id) {
  token <- appeears_token()
  response <- GET(file.path(api_base, "task", task_id), auth_header(token))
  parsed <- content(stop_for_response(response), as = "parsed", type = "application/json")
  print(parsed)
  invisible(parsed)
}

download_bundle <- function(task_id) {
  token <- appeears_token()
  response <- GET(file.path(api_base, "bundle", task_id), auth_header(token))
  bundle <- content(stop_for_response(response), as = "parsed", type = "application/json")
  task_dir <- file.path(download_dir, task_id)
  dir.create(task_dir, recursive = TRUE, showWarnings = FALSE)
  write(toJSON(bundle, auto_unbox = TRUE, pretty = TRUE), file.path(task_dir, "bundle.json"))

  for (item in bundle$files) {
    file_name <- item$file_name
    target <- file.path(task_dir, basename(file_name))
    message("Downloading ", file_name)
    response <- GET(file.path(api_base, "bundle", task_id, item$file_id),
                    auth_header(token), write_disk(target, overwrite = TRUE))
    stop_for_response(response)
  }
  message("Downloaded bundle to: ", task_dir)
  invisible(task_dir)
}

find_value_col <- function(df) {
  candidates <- c(layer, "sm_surface", "value", "Value")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, c("latitude", "Latitude", "longitude", "Longitude", "ID", "id"))
  if (length(numeric_cols) == 1) return(numeric_cols)
  stop("Could not identify SMAP value column. Columns: ", paste(names(df), collapse = ", "), call. = FALSE)
}

find_date_col <- function(df) {
  candidates <- c("Date", "date", "time", "Time", "datetime", "Date_Time")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  stop("Could not identify date/time column. Columns: ", paste(names(df), collapse = ", "), call. = FALSE)
}

find_site_col <- function(df) {
  candidates <- c("ID", "id", "SITE_ID", "site_id")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) > 0) return(hit[1])
  stop("Could not identify site ID column. Columns: ", paste(names(df), collapse = ", "), call. = FALSE)
}

parse_template_line <- function(line) {
  parts <- trimws(strsplit(line, ",", fixed = TRUE)[[1]])
  data.frame(
    lon = as.numeric(parts[1]),
    lat = as.numeric(parts[2]),
    area = as.numeric(parts[4]),
    year = as.integer(parts[5]),
    month = as.integer(parts[6]),
    SITE_ID = parts[length(parts)],
    stringsAsFactors = FALSE
  )
}

read_template <- function() {
  files <- list.files(tem_dir, pattern = "^FNdSSM2006-2019\\.tem-[0-9]+$",
                      recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) stop("No FNdSSM template files found in ", tem_dir, call. = FALSE)
  rows <- do.call(rbind, lapply(files, function(path) {
    lines <- readLines(path, warn = FALSE)
    parsed <- do.call(rbind, lapply(lines, parse_template_line))
    parsed$part <- sub(".*-", "", basename(path))
    parsed$path <- path
    parsed
  }))
  rows[!duplicated(rows[c("part", "SITE_ID", "year", "month")]), ]
}

days_in_month <- function(year, month) {
  start <- as.Date(sprintf("%04d-%02d-01", year, month))
  next_month <- if (month == 12) as.Date(sprintf("%04d-01-01", year + 1)) else as.Date(sprintf("%04d-%02d-01", year, month + 1))
  as.integer(next_month - start)
}

format_num <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.4f", x))
}

convert_appeears_csv <- function(csv_file) {
  smap <- read.csv(csv_file, stringsAsFactors = FALSE)
  site_col <- find_site_col(smap)
  date_col <- find_date_col(smap)
  value_col <- find_value_col(smap)

  smap$SITE_ID <- as.character(smap[[site_col]])
  smap$Date <- as.Date(substr(as.character(smap[[date_col]]), 1, 10))
  smap$value <- as.numeric(smap[[value_col]])
  smap$value[smap$value <= -9990] <- NA_real_
  daily <- aggregate(value ~ SITE_ID + Date, data = smap, FUN = function(x) mean(x, na.rm = TRUE))
  daily$value[is.nan(daily$value)] <- NA_real_
  daily$key <- paste(daily$SITE_ID, daily$Date)

  template <- read_template()
  template <- template[template$year >= 2015 & template$year <= 2019, ]
  template <- template[as.Date(sprintf("%04d-%02d-01", template$year, template$month)) >= as.Date("2015-03-01"), ]

  by_part <- split(template, template$part)
  for (part in names(by_part)) {
    part_rows <- by_part[[part]]
    out_file <- file.path(tem_dir, paste0("part-", part), paste0("SPL4SMGP2006-2019.tem-", part))
    lines <- character(nrow(part_rows))
    for (i in seq_len(nrow(part_rows))) {
      row <- part_rows[i, ]
      n_days <- days_in_month(row$year, row$month)
      dates <- as.Date(sprintf("%04d-%02d-%02d", row$year, row$month, seq_len(n_days)))
      values <- daily$value[match(paste(row$SITE_ID, dates), daily$key)]
      monthly <- c(sum(values, na.rm = TRUE), max(values, na.rm = TRUE), mean(values, na.rm = TRUE), min(values, na.rm = TRUE))
      if (all(is.na(values))) monthly[] <- NA_real_
      lines[i] <- paste(c(
        sprintf("%.6f", row$lon),
        sprintf("%.6f", row$lat),
        " SMAP_L4_SSM ",
        as.character(row$area),
        as.character(row$year),
        as.character(row$month),
        format_num(monthly),
        format_num(values),
        row$SITE_ID
      ), collapse = ",")
    }
    writeLines(lines, out_file)
    message("Wrote ", out_file)
  }
}

if (command == "submit") {
  submit_task()
} else if (command == "status") {
  if (length(args) < 2) stop("Usage: Rscript 00_Download_SMAP_L4.R status <task_id>", call. = FALSE)
  task_status(args[2])
} else if (command == "download") {
  if (length(args) < 2) stop("Usage: Rscript 00_Download_SMAP_L4.R download <task_id>", call. = FALSE)
  download_bundle(args[2])
} else if (command == "convert") {
  if (length(args) < 2) stop("Usage: Rscript 00_Download_SMAP_L4.R convert <appeears_csv>", call. = FALSE)
  convert_appeears_csv(args[2])
} else if (command == "all") {
  task <- submit_task()
  message("Task submitted. AppEEARS processing is asynchronous; rerun status/download/convert when it is done.")
} else {
  message("Usage:")
  message("  EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R submit")
  message("  EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R status <task_id>")
  message("  EARTHDATA_USERNAME=... EARTHDATA_PASSWORD=... Rscript 00_Download_SMAP_L4.R download <task_id>")
  message("  Rscript 00_Download_SMAP_L4.R convert <path/to/appeears-point-results.csv>")
}
