# 15_Compare_FLUXNET_Models.R
# ---------------------------------------------------------------------------
# Compare FLUXNET-CH4 extreme-event responses against one or more models.
# Models are declared in models.R (MODEL_REGISTRY) — adding a model requires NO
# change here. Two comparisons are produced for every model x scenario:
#   (A) moisture axis  : drought/wet response vs FLUXNET (paired & site-month).
#   (B) compound axis  : the 3x3 moisture x temperature-anomaly grid vs FLUXNET.
# All outputs are written to outputs/fluxnet_model_comparison/ and mirrored to
# the server (see io_helpers.R / config.R).
# ---------------------------------------------------------------------------

rm(list = ls())

script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()

source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "models.R"))
source(file.path(analysis_dir, "temperature_index.R"))

out_rel <- function(f) file.path("fluxnet_model_comparison", f)

fluxnet_file <- file.path(analysis_dir, "data", "DroughtAnalysis.RDATA")
load(fluxnet_file)
fluxnet_data <- fluxes.drought_normalized
if (!"temp_class" %in% names(fluxnet_data))
  fluxnet_data <- add_temp_anomaly_class(fluxnet_data, ta_col = "TA_F")

spei_indices <- c("SPEI1", "SPEI3", "SPEI6", "SPEI12", "SPEI24", "SPEI36", "SPEI48")
spei_indices <- spei_indices[spei_indices %in% names(fluxnet_data)]
term_class <- ifelse(spei_indices %in% c("SPEI1", "SPEI3", "SPEI6"), "short_term", "long_term")
term_lookup <- data.frame(Drought.IDX = spei_indices, term_class = term_class, stringsAsFactors = FALSE)

# ---- shared helpers (generic in source/scenario/response) -----------------
condition_for_index <- function(x)
  ifelse(is.na(x), NA_character_,
         ifelse(x <= DROUGHT_THRESHOLD, "drought",
                ifelse(x >= WET_THRESHOLD, "extreme_wet", "normal")))
mean_or_na   <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
median_or_na <- function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)

site_paired_effects <- function(index_name, data, source, scenario, response_col) {
  data$condition_tmp <- condition_for_index(data[[index_name]])
  data <- data[!is.na(data$condition_tmp) & !is.na(data[[response_col]]), ]
  sc <- aggregate(data[[response_col]],
                  by = list(SITE_ID = data$SITE_ID, condition = data$condition_tmp),
                  FUN = mean, na.rm = TRUE)
  names(sc)[3] <- "response"
  w <- reshape(sc, idvar = "SITE_ID", timevar = "condition", direction = "wide")
  gv <- function(col) if (col %in% names(w)) w[[col]] else NA_real_
  out <- data.frame(source = source, scenario = scenario, Drought.IDX = index_name,
                    SITE_ID = w$SITE_ID,
                    drought_minus_normal = gv("response.drought") - gv("response.normal"),
                    extreme_wet_minus_normal = gv("response.extreme_wet") - gv("response.normal"),
                    stringsAsFactors = FALSE)
  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

site_month_matched_effects <- function(index_name, data, source, scenario, response_col) {
  data$condition_tmp <- condition_for_index(data[[index_name]])
  data <- data[!is.na(data$condition_tmp) & !is.na(data[[response_col]]) & !is.na(data$month), ]
  smc <- aggregate(data[[response_col]],
                   by = list(SITE_ID = data$SITE_ID, month = data$month, condition = data$condition_tmp),
                   FUN = mean, na.rm = TRUE)
  names(smc)[4] <- "response"
  normal <- smc[smc$condition == "normal", c("SITE_ID", "month", "response")]
  names(normal)[3] <- "normal_response"
  nn <- smc[smc$condition != "normal", ]
  matched <- merge(nn, normal, by = c("SITE_ID", "month"), all.x = FALSE, sort = FALSE)
  if (nrow(matched) == 0) return(data.frame())
  matched$delta_response <- matched$response - matched$normal_response
  agg <- aggregate(delta_response ~ SITE_ID + condition, matched, FUN = mean, na.rm = TRUE)
  out <- data.frame(source = source, scenario = scenario, Drought.IDX = index_name,
                    SITE_ID = agg$SITE_ID, condition = agg$condition,
                    delta_response = agg$delta_response, stringsAsFactors = FALSE)
  merge(out, term_lookup, by = "Drought.IDX", all.x = TRUE, sort = FALSE)
}

summarise_effects <- function(effects, value_col, condition_col = NULL, condition_value = NULL) {
  if (!is.null(condition_col)) effects <- effects[effects[[condition_col]] == condition_value, ]
  rows <- split(effects, paste(effects$source, effects$scenario, effects$Drought.IDX, sep = "__"))
  out <- do.call(rbind, lapply(rows, function(sub) {
    v <- sub[[value_col]]; v <- v[!is.na(v)]
    if (length(v) == 0) return(NULL)
    data.frame(source = sub$source[1], scenario = sub$scenario[1], Drought.IDX = sub$Drought.IDX[1],
               term_class = sub$term_class[1], n_sites = length(v),
               mean_delta = mean(v), median_delta = median(v),
               n_positive = sum(v > 0), n_negative = sum(v < 0), stringsAsFactors = FALSE)
  }))
  out[order(out$source, out$scenario, match(out$Drought.IDX, spei_indices)), ]
}

compare_overlap <- function(fluxnet_effects, model_effects, value_col, effect_type,
                            model_name, condition = "drought") {
  rows <- list(); k <- 1
  for (index_name in spei_indices) {
    f <- fluxnet_effects[fluxnet_effects$Drought.IDX == index_name, c("SITE_ID", value_col)]
    names(f)[2] <- "fluxnet_delta"
    for (scn in unique(model_effects$scenario)) {
      t <- model_effects[model_effects$Drought.IDX == index_name & model_effects$scenario == scn,
                         c("SITE_ID", value_col)]
      names(t)[2] <- "model_delta"
      p <- merge(f, t, by = "SITE_ID"); p <- p[!is.na(p$fluxnet_delta) & !is.na(p$model_delta), ]
      if (nrow(p) == 0) next
      cr <- if (nrow(p) >= 3) suppressWarnings(cor.test(p$fluxnet_delta, p$model_delta, method = "spearman", exact = FALSE)) else NULL
      rows[[k]] <- data.frame(model = model_name, effect_type = effect_type, condition = condition,
                              scenario = scn, Drought.IDX = index_name,
                              term_class = term_lookup$term_class[match(index_name, term_lookup$Drought.IDX)],
                              n_overlap_sites = nrow(p),
                              fluxnet_mean_delta = mean(p$fluxnet_delta), model_mean_delta = mean(p$model_delta),
                              mean_difference_model_minus_fluxnet = mean(p$model_delta - p$fluxnet_delta),
                              sign_agreement_pct = 100 * mean(sign(p$fluxnet_delta) == sign(p$model_delta)),
                              spearman_rho = if (is.null(cr)) NA_real_ else unname(cr$estimate),
                              spearman_p = if (is.null(cr)) NA_real_ else cr$p.value,
                              stringsAsFactors = FALSE)
      k <- k + 1
    }
  }
  do.call(rbind, rows)
}

# ---- compound grid (moisture x temperature-anomaly) -----------------------
compound_grid <- function(data, source, scenario, response_col) {
  if (!"temp_class" %in% names(data) || !"condition" %in% names(data)) return(NULL)
  ok <- !is.na(data$condition) & !is.na(data$temp_class) & !is.na(data[[response_col]])
  d <- data[ok, ]
  if (nrow(d) == 0) return(NULL)
  g <- list(condition = d$condition, temp_class = d$temp_class)
  m <- aggregate(d[[response_col]], by = g, FUN = mean, na.rm = TRUE); names(m)[3] <- "mean_normalized"
  n <- aggregate(d[[response_col]], by = g, FUN = length);            names(n)[3] <- "n"
  out <- merge(m, n, by = c("condition", "temp_class"))
  cbind(source = source, scenario = scenario, out)
}

grid_agreement <- function(fluxnet_grid, model_grid, model_name, scenario) {
  j <- merge(fluxnet_grid[c("condition", "temp_class", "mean_normalized")],
             model_grid[c("condition", "temp_class", "mean_normalized")],
             by = c("condition", "temp_class"), suffixes = c("_fluxnet", "_model"))
  if (nrow(j) < 3) return(NULL)
  cr <- suppressWarnings(cor(j$mean_normalized_fluxnet, j$mean_normalized_model, method = "pearson"))
  data.frame(model = model_name, scenario = scenario, n_cells = nrow(j),
             bias_model_minus_fluxnet = mean(j$mean_normalized_model - j$mean_normalized_fluxnet),
             rmse = sqrt(mean((j$mean_normalized_model - j$mean_normalized_fluxnet)^2)),
             sign_agreement_pct = 100 * mean(sign(j$mean_normalized_model) == sign(j$mean_normalized_fluxnet)),
             pearson_r_across_cells = cr, stringsAsFactors = FALSE)
}

# ---- FLUXNET reference effects & grid -------------------------------------
fluxnet_data$condition <- condition_for_index(fluxnet_data[[DROUGHT_INDEX]])
fluxnet_paired <- do.call(rbind, lapply(spei_indices, site_paired_effects,
  data = fluxnet_data, source = "FLUXNET", scenario = "tower", response_col = "normalized_Fch4"))
fluxnet_month <- do.call(rbind, lapply(spei_indices, site_month_matched_effects,
  data = fluxnet_data, source = "FLUXNET", scenario = "tower", response_col = "normalized_Fch4"))
fluxnet_grid <- compound_grid(fluxnet_data, "FLUXNET", "tower", "normalized_Fch4")

# ---- loop over every registered model -------------------------------------
paired_all  <- list(fluxnet_paired)
month_all   <- list(fluxnet_month)
grid_all    <- list(fluxnet_grid)
overlap_all <- list()
grid_eval_all <- list()

for (model_name in names(MODEL_REGISTRY)) {
  m <- MODEL_REGISTRY[[model_name]]
  rdata_path <- m$daily_rdata(analysis_dir)
  if (!file.exists(rdata_path)) { message("Skipping ", model_name, " (missing ", rdata_path, ")"); next }
  load(rdata_path)
  md <- get(m$daily_object)
  if (!"temp_class" %in% names(md) && m$temp_col %in% names(md))
    md <- add_temp_anomaly_class(md, ta_col = m$temp_col)
  if (!"condition" %in% names(md) && DROUGHT_INDEX %in% names(md))
    md$condition <- condition_for_index(md[[DROUGHT_INDEX]])

  model_paired <- list(); model_month <- list()
  for (scn in names(m$scenarios)) {
    resp <- m$scenarios[[scn]]$normalized
    if (!resp %in% names(md)) { message("  ", model_name, "/", scn, ": missing ", resp); next }
    model_paired[[scn]] <- do.call(rbind, lapply(spei_indices, site_paired_effects,
      data = md, source = model_name, scenario = scn, response_col = resp))
    model_month[[scn]] <- do.call(rbind, lapply(spei_indices, site_month_matched_effects,
      data = md, source = model_name, scenario = scn, response_col = resp))
    g <- compound_grid(md, model_name, scn, resp)
    if (!is.null(g)) {
      grid_all[[paste(model_name, scn)]] <- g
      ge <- grid_agreement(fluxnet_grid, g, model_name, scn)
      if (!is.null(ge)) grid_eval_all[[paste(model_name, scn)]] <- ge
    }
  }
  model_paired <- do.call(rbind, model_paired); model_month <- do.call(rbind, model_month)
  paired_all[[model_name]] <- model_paired; month_all[[model_name]] <- model_month

  overlap_all[[paste0(model_name, "_paired")]] <- compare_overlap(
    fluxnet_paired, model_paired, "drought_minus_normal", "site_paired", model_name)
  overlap_all[[paste0(model_name, "_month")]] <- compare_overlap(
    fluxnet_month[fluxnet_month$condition == "drought", ],
    model_month[model_month$condition == "drought", ],
    "delta_response", "site_month_matched", model_name)
}

# ---- write combined, model-agnostic outputs -------------------------------
save_output_csv(do.call(rbind, paired_all), out_rel("site_paired_deltas_by_spei.csv"), analysis_dir)
save_output_csv(do.call(rbind, month_all),  out_rel("site_month_matched_deltas_by_spei.csv"), analysis_dir)
save_output_csv(rbind(summarise_effects(do.call(rbind, paired_all), "drought_minus_normal")),
                out_rel("source_site_paired_summary_by_spei.csv"), analysis_dir)
save_output_csv(summarise_effects(do.call(rbind, month_all), "delta_response", "condition", "drought"),
                out_rel("source_site_month_matched_summary_by_spei.csv"), analysis_dir)
save_output_csv(do.call(rbind, overlap_all), out_rel("fluxnet_model_overlap_comparison_by_spei.csv"), analysis_dir)

# Compound-grid outputs (the headline model evaluation in the compound frame)
save_output_csv(do.call(rbind, grid_all), out_rel("compound_grid_by_source.csv"), analysis_dir)
if (length(grid_eval_all))
  save_output_csv(do.call(rbind, grid_eval_all), out_rel("compound_grid_model_agreement.csv"), analysis_dir)

overlap_sites <- data.frame(SITE_ID = sort(intersect(
  unique(fluxnet_data$SITE_ID),
  unique(unlist(lapply(names(MODEL_REGISTRY), function(mn) {
    p <- MODEL_REGISTRY[[mn]]$daily_rdata(analysis_dir)
    if (file.exists(p)) { load(p); unique(get(MODEL_REGISTRY[[mn]]$daily_object)$SITE_ID) } else character(0)
  }))))), stringsAsFactors = FALSE)
save_output_csv(overlap_sites, out_rel("overlap_sites.csv"), analysis_dir)

message("Compared FLUXNET with ", length(MODEL_REGISTRY), " model(s): ",
        paste(names(MODEL_REGISTRY), collapse = ", "))
