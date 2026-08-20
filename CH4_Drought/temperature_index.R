# temperature_index.R
# ---------------------------------------------------------------------------
# Shared helper for the thermal-season (temperature) axis of the CH4
# extreme-event workflow. It classifies each observation as a WINTER
# (cold-season) or SUMMER (warm-season) event RELATIVE TO ITS OWN SITE, so the
# axis is comparable across sites in different climates and is independent of
# the SPEI moisture axis. Crossing thermal_season with the moisture condition
# separates summer-dry from winter-dry events and summer-wet from winter-wet
# events, letting us see which methane patterns are driven by hot vs cold
# conditions regardless of whether the event is extreme-dry or extreme-wet.
#
# Columns added by add_thermal_season():
#   thermal_season      factor: winter / shoulder / summer (shoulder only if
#                       use_shoulder = TRUE)
#   TA_site_month_mean  site x calendar-month climatological mean of TA_F
#   TA_site_mean        site mean of TA_F
#   TA_site_sd          site sd of TA_F
#   TA_site_anom        TA_F - TA_site_month_mean  (within-month temperature anomaly)
#   TA_site_z           (TA_F - TA_site_mean) / TA_site_sd  (site-standardized temperature)
#
# Implemented in base R so it preserves row order and is safe on plain
# data.frames, tibbles, and sf objects (it never summarises the geometry).
# Thresholds/behaviour come from config.R; if config.R has not been sourced,
# sensible defaults are used.
# ---------------------------------------------------------------------------

add_thermal_season <- function(data,
                               ta_col       = "TA_F",
                               site_col     = "SITE_ID",
                               month_col    = "month",
                               basis        = if (exists("THERMAL_BASIS")) THERMAL_BASIS else "site_month_clim",
                               lower_prob   = if (exists("THERMAL_LOWER_PROB")) THERMAL_LOWER_PROB else 1/3,
                               upper_prob   = if (exists("THERMAL_UPPER_PROB")) THERMAL_UPPER_PROB else 2/3,
                               use_shoulder = if (exists("THERMAL_USE_SHOULDER")) THERMAL_USE_SHOULDER else TRUE) {

  stopifnot(all(c(ta_col, site_col, month_col) %in% names(data)))

  # Make the helper idempotent: drop any columns it may have added previously.
  out_cols <- c("thermal_season", "TA_site_month_mean", "TA_site_mean",
                "TA_site_sd", "TA_site_anom", "TA_site_z")
  keep <- setdiff(names(data), out_cols)
  data <- data[, keep, drop = FALSE]

  ta   <- as.numeric(data[[ta_col]])
  site <- as.character(data[[site_col]])
  mon  <- as.character(data[[month_col]])

  # Site-level temperature climate (for the continuous, site-standardized index).
  site_mean_map <- tapply(ta, site, mean, na.rm = TRUE)
  site_sd_map   <- tapply(ta, site, stats::sd, na.rm = TRUE)

  # Site x calendar-month climatology (defines each site's seasonal cycle).
  # The separator must not appear in SITE_ID values, so cold/warm months are
  # keyed unambiguously.
  sm_key      <- paste(site, mon, sep = "@@")
  sm_mean_map <- tapply(ta, sm_key, mean, na.rm = TRUE)

  TA_site_mean       <- as.numeric(site_mean_map[site])
  TA_site_sd         <- as.numeric(site_sd_map[site])
  TA_site_month_mean <- as.numeric(sm_mean_map[sm_key])

  # Per-site thresholds that split the year into cold vs warm season.
  q_lower <- if (use_shoulder) lower_prob else 0.5
  q_upper <- if (use_shoulder) upper_prob else 0.5

  sites <- unique(site)
  lower_map <- stats::setNames(numeric(length(sites)), sites)
  upper_map <- stats::setNames(numeric(length(sites)), sites)
  for (s in sites) {
    if (identical(basis, "observation")) {
      v <- ta[site == s]                                        # season = temperature level
    } else {
      v <- as.numeric(sm_mean_map[unique(sm_key[site == s])])   # season = timing (monthly climatology)
    }
    lower_map[s] <- as.numeric(stats::quantile(v, q_lower, na.rm = TRUE))
    upper_map[s] <- as.numeric(stats::quantile(v, q_upper, na.rm = TRUE))
  }
  lower <- as.numeric(lower_map[site])
  upper <- as.numeric(upper_map[site])

  class_value <- if (identical(basis, "observation")) ta else TA_site_month_mean
  season <- ifelse(class_value <= lower, "winter",
            ifelse(class_value >= upper, "summer", "shoulder"))
  lvls <- if (use_shoulder) c("winter", "shoulder", "summer") else c("winter", "summer")

  data$thermal_season     <- factor(season, levels = lvls)
  data$TA_site_month_mean <- TA_site_month_mean
  data$TA_site_mean       <- TA_site_mean
  data$TA_site_sd         <- TA_site_sd
  data$TA_site_anom       <- ta - TA_site_month_mean
  data$TA_site_z          <- ifelse(TA_site_sd > 0, (ta - TA_site_mean) / TA_site_sd, NA_real_)
  data
}

# ---------------------------------------------------------------------------
# add_temp_anomaly_class(): the true analog of the SPEI moisture axis, but for
# temperature. TA_F is standardized WITHIN each site-month to a Standardized
# Temperature Index (STI = (TA_F - mean_site_month) / sd_site_month), so a "hot"
# event is hotter than normal for that site and time of year and a "cold" event
# is colder than normal. Includes an explicit NORMAL class.
#
# Columns added:
#   temp_class     factor: cold / normal / hot  (NA where the baseline is too thin)
#   STI            site-month standardized temperature index
#   TA_month_mean  site x calendar-month mean of TA_F
#   TA_month_sd    site x calendar-month sd of TA_F
#   TA_month_n     number of observations in the site-month
# ---------------------------------------------------------------------------
add_temp_anomaly_class <- function(data,
                                   ta_col    = "TA_F",
                                   site_col  = "SITE_ID",
                                   month_col = "month",
                                   hot       = if (exists("HOT_THRESHOLD")) HOT_THRESHOLD else 1,
                                   cold      = if (exists("COLD_THRESHOLD")) COLD_THRESHOLD else -1,
                                   min_obs   = if (exists("TEMP_MIN_MONTH_OBS")) TEMP_MIN_MONTH_OBS else 5) {

  stopifnot(all(c(ta_col, site_col, month_col) %in% names(data)))

  out_cols <- c("temp_class", "STI", "TA_month_mean", "TA_month_sd", "TA_month_n")
  data <- data[, setdiff(names(data), out_cols), drop = FALSE]

  ta   <- as.numeric(data[[ta_col]])
  site <- as.character(data[[site_col]])
  mon  <- as.character(data[[month_col]])
  sm_key <- paste(site, mon, sep = "@@")   # separator must not occur in SITE_ID

  mean_map <- tapply(ta, sm_key, mean, na.rm = TRUE)
  sd_map   <- tapply(ta, sm_key, stats::sd, na.rm = TRUE)
  n_map    <- tapply(ta, sm_key, function(x) sum(is.finite(x)))

  TA_month_mean <- as.numeric(mean_map[sm_key])
  TA_month_sd   <- as.numeric(sd_map[sm_key])
  TA_month_n    <- as.numeric(n_map[sm_key])

  # STI is undefined where the site-month baseline is too thin or has no spread.
  ok  <- is.finite(TA_month_sd) & TA_month_sd > 0 & TA_month_n >= min_obs
  STI <- ifelse(ok, (ta - TA_month_mean) / TA_month_sd, NA_real_)

  temp_class <- ifelse(is.na(STI), NA_character_,
                ifelse(STI <= cold, "cold",
                ifelse(STI >= hot,  "hot", "normal")))

  data$temp_class    <- factor(temp_class, levels = c("cold", "normal", "hot"))
  data$STI           <- STI
  data$TA_month_mean <- TA_month_mean
  data$TA_month_sd   <- TA_month_sd
  data$TA_month_n    <- TA_month_n
  data
}

# Collapse the moisture condition and a temperature class into a single labelled
# cross-classification. Works for either the seasonal axis (season_col =
# "thermal_season" -> "summer_dry", "winter_wet") or the anomaly axis
# (season_col = "temp_class" -> "hot_dry", "cold_wet", "normal_normal").
# `condition` is expected to use the labels drought / normal / extreme_wet.
add_event_class <- function(data,
                            condition_col = "condition",
                            season_col    = "thermal_season",
                            out_col       = "event_class") {
  moisture_label <- c(drought = "dry", normal = "normal", extreme_wet = "wet")
  season_chr <- as.character(data[[season_col]])
  moist_chr  <- moisture_label[as.character(data[[condition_col]])]
  data[[out_col]] <- factor(paste(season_chr, moist_chr, sep = "_"))
  data
}
