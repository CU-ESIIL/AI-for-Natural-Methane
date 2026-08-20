# 21b_AxisOrthogonality.R
# ---------------------------------------------------------------------------
# Quantify how (non-)orthogonal the two axes of the workflow really are:
#   - the SPEI moisture axis (DROUGHT_INDEX, e.g. SPEI1), and
#   - the Standardized Temperature Index (STI, site-month z-score of TA_F).
#
# WHY THIS MATTERS. SPEI is a standardized P - PET water balance, and PET is a
# function of temperature, so temperature enters the moisture index by
# construction; hot and dry also co-occur physically (compound events / soil
# moisture-temperature feedback). The two axes are therefore NOT guaranteed to
# be independent. This script measures the residual dependence directly so the
# "separate moisture vs temperature axes" design can be defended with numbers
# rather than asserted. It also confirms that the compound-grid projection is
# right to preserve the OBSERVED hot-dry joint occupancy (via IPF in
# 18/19/21) instead of assuming independence.
#
# WHAT IT COMPUTES (all on the canonical analysis table, data/DroughtAnalysis.RDATA):
#   1. Continuous association SPEI vs STI:
#        - pooled Pearson & Spearman (with test, CI, n) and r^2 / shared variance
#        - site-blocked: distribution of within-site Pearson r (guards against
#          pseudoreplication from many obs per site)
#        - site-demeaned pooled ("within") correlation
#        - the 2-predictor variance-inflation factor VIF = 1 / (1 - r^2)
#   2. Categorical association on the 3x3 grid (condition x temp_class):
#        - contingency counts and proportions
#        - chi-square (asymptotic + Monte-Carlo p) and Cramer's V effect size
#   3. Joint occupancy vs independence:
#        - observed joint probabilities, the product-of-marginals expectation,
#          their difference, and the lift (obs/exp) - the hot-dry cell is the
#          one the IPF step is designed to preserve.
#
# Outputs -> outputs/axis_orthogonality/ (+ server mirror via io_helpers.R).
# Read alongside 21_SensitivityAndDiagnostics.R; this step only needs
# DroughtAnalysis.RDATA (from 06_BuildAnalysisTable.R), nothing downstream.
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file))
} else {
  # Interactive run: pick whichever candidate holds config.R (project root or CH4_Drought).
  cand <- c(getwd(), file.path(getwd(), "CH4_Drought"),
            "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
  hit <- cand[file.exists(file.path(cand, "config.R"))]
  if (length(hit)) hit[1] else stop(
    "Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
}
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
source(file.path(analysis_dir, "temperature_index.R"))

out_rel <- function(f) file.path("axis_orthogonality", f)
MO <- c("drought", "normal", "extreme_wet")   # moisture (SPEI) classes
TE <- c("cold", "normal", "hot")              # temperature (STI) classes
MIN_SITE_OBS <- 30                            # min obs for a within-site correlation

# ---- data: canonical analysis table + STI/temp_class + moisture condition -----
load(file.path(analysis_dir, "data", "DroughtAnalysis.RDATA"))   # fluxes.drought_normalized
d <- add_temp_anomaly_class(fluxes.drought_normalized, ta_col = "TA_F")
d$condition <- ifelse(d[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD, "drought",
               ifelse(d[[DROUGHT_INDEX]] >= WET_THRESHOLD,    "extreme_wet", "normal"))

spei_v <- as.numeric(d[[DROUGHT_INDEX]])
sti_v  <- as.numeric(d[[TEMP_INDEX_VAR]])          # "STI"
ok     <- is.finite(spei_v) & is.finite(sti_v)
site   <- as.character(d$SITE_ID)

dd <- data.frame(SITE_ID = site[ok], SPEI = spei_v[ok], STI = sti_v[ok],
                 condition = d$condition[ok],
                 temp_class = as.character(d$temp_class)[ok],
                 stringsAsFactors = FALSE)
n_all <- nrow(dd)
message(sprintf("Axis orthogonality on %d obs, %d sites (index = %s vs %s).",
                n_all, length(unique(dd$SITE_ID)), DROUGHT_INDEX, TEMP_INDEX_VAR))

# ---- 1. continuous association ------------------------------------------------
pear <- stats::cor.test(dd$SPEI, dd$STI, method = "pearson")
spear <- suppressWarnings(stats::cor.test(dd$SPEI, dd$STI, method = "spearman"))
r  <- unname(pear$estimate)
r2 <- r^2
vif <- 1 / (1 - r2)

# site-blocked within-site correlations (guards pseudoreplication)
site_r <- sapply(split(dd[, c("SPEI", "STI")], dd$SITE_ID), function(s) {
  if (nrow(s) < MIN_SITE_OBS || stats::sd(s$SPEI) == 0 || stats::sd(s$STI) == 0) return(NA_real_)
  stats::cor(s$SPEI, s$STI)
})
site_r <- site_r[is.finite(site_r)]

# site-demeaned pooled ("within") correlation
site_mean <- function(x, g) ave(x, g, FUN = function(v) mean(v, na.rm = TRUE))
within_r <- stats::cor(dd$SPEI - site_mean(dd$SPEI, dd$SITE_ID),
                       dd$STI  - site_mean(dd$STI,  dd$SITE_ID))

corr_summary <- data.frame(
  metric = c("pearson_r", "pearson_p", "pearson_ci_lo", "pearson_ci_hi",
             "spearman_rho", "spearman_p",
             "r_squared", "shared_variance_pct", "vif_2predictor",
             "within_site_pooled_r",
             "within_site_mean_r", "within_site_median_r", "within_site_sd_r",
             "within_site_min_r", "within_site_max_r", "n_sites_within",
             "n_obs"),
  value = c(r, pear$p.value, pear$conf.int[1], pear$conf.int[2],
            unname(spear$estimate), spear$p.value,
            r2, 100 * r2, vif,
            within_r,
            mean(site_r), stats::median(site_r), stats::sd(site_r),
            min(site_r), max(site_r), length(site_r),
            n_all))
save_output_csv(corr_summary, out_rel("spei_sti_correlation_summary.csv"), analysis_dir)

within_tbl <- data.frame(SITE_ID = names(site_r), within_site_r = as.numeric(site_r),
                         row.names = NULL)
within_tbl <- within_tbl[order(within_tbl$within_site_r), ]
save_output_csv(within_tbl, out_rel("spei_sti_within_site_correlations.csv"), analysis_dir)

# ---- 2. categorical association on the 3x3 grid -------------------------------
dd$condition  <- factor(dd$condition,  levels = MO)
dd$temp_class <- factor(dd$temp_class, levels = TE)
tab <- table(condition = dd$condition, temp_class = dd$temp_class)

chi     <- suppressWarnings(stats::chisq.test(tab))
chi_mc  <- suppressWarnings(stats::chisq.test(tab, simulate.p.value = TRUE, B = 5000))
N       <- sum(tab)
cramers_v <- sqrt(as.numeric(chi$statistic) / (N * (min(dim(tab)) - 1)))

counts_df <- as.data.frame.matrix(tab)
counts_df <- cbind(condition = rownames(counts_df), counts_df)
save_output_csv(counts_df, out_rel("condition_tempclass_contingency_counts.csv"), analysis_dir)

prop_df <- as.data.frame.matrix(tab / N)
prop_df <- cbind(condition = rownames(prop_df), prop_df)
save_output_csv(prop_df, out_rel("condition_tempclass_contingency_proportions.csv"), analysis_dir)

assoc <- data.frame(
  test = c("chisq_statistic", "chisq_df", "chisq_p_asymptotic",
           "chisq_p_montecarlo_B5000", "cramers_v", "n_obs"),
  value = c(as.numeric(chi$statistic), as.numeric(chi$parameter),
            chi$p.value, chi_mc$p.value, cramers_v, N))
save_output_csv(assoc, out_rel("categorical_association_tests.csv"), analysis_dir)

# ---- 3. joint occupancy: observed vs independence ----------------------------
obs  <- tab / N
rowm <- rowSums(tab) / N
colm <- colSums(tab) / N
exp_indep <- outer(as.numeric(rowm), as.numeric(colm))
dimnames(exp_indep) <- dimnames(tab)

joint <- expand.grid(condition = MO, temp_class = TE, stringsAsFactors = FALSE)
joint$observed_prob     <- mapply(function(m, t) obs[m, t],        joint$condition, joint$temp_class)
joint$expected_indep    <- mapply(function(m, t) exp_indep[m, t],  joint$condition, joint$temp_class)
joint$obs_minus_exp     <- joint$observed_prob - joint$expected_indep
joint$lift_obs_over_exp <- joint$observed_prob / joint$expected_indep
joint$observed_count    <- mapply(function(m, t) tab[m, t],        joint$condition, joint$temp_class)
save_output_csv(joint, out_rel("joint_observed_vs_independence.csv"), analysis_dir)

# ---- figures -----------------------------------------------------------------
# (a) SPEI vs STI density scatter with the fitted line and r annotation.
save_output_figure(function() {
  op <- par(mar = c(4.5, 4.5, 3, 1))
  drawn <- tryCatch({
    graphics::smoothScatter(dd$SPEI, dd$STI,
                            xlab = paste0(DROUGHT_INDEX, "  (moisture axis)"),
                            ylab = "STI  (temperature axis)",
                            main = "Moisture vs temperature axis"); TRUE
  }, error = function(e) FALSE)
  if (!drawn) {
    graphics::plot(dd$SPEI, dd$STI, pch = 16, cex = 0.3,
                   col = grDevices::adjustcolor("steelblue", 0.15),
                   xlab = paste0(DROUGHT_INDEX, "  (moisture axis)"),
                   ylab = "STI  (temperature axis)",
                   main = "Moisture vs temperature axis")
  }
  graphics::abline(stats::lm(STI ~ SPEI, data = dd), col = "red", lwd = 2)
  graphics::abline(h = c(COLD_THRESHOLD, HOT_THRESHOLD),
                   v = c(DROUGHT_THRESHOLD, WET_THRESHOLD), lty = 3, col = "grey40")
  graphics::legend("topright", bty = "n",
                   legend = sprintf("Pearson r = %.3f  (r%s = %.3f)\nwithin-site r = %.3f\nn = %s",
                                    r, "²", r2, within_r, format(n_all, big.mark = ",")))
  par(op)
}, out_rel(file.path("figures", "spei_vs_sti_scatter.png")), analysis_dir)

# (b) heatmap of observed - expected(independence), % points; hot-dry highlighted.
save_output_figure(function() {
  m <- (obs - exp_indep) * 100
  op <- par(mar = c(4.5, 6, 3, 2))
  lim <- max(abs(m))
  cols <- grDevices::colorRampPalette(c("#2166ac", "white", "#b2182b"))(101)
  graphics::image(x = seq_len(ncol(m)), y = seq_len(nrow(m)), z = t(m[nrow(m):1, , drop = FALSE]),
                  zlim = c(-lim, lim), col = cols, axes = FALSE,
                  xlab = "temperature class (STI)", ylab = "",
                  main = "Observed - independence (percentage points)")
  graphics::axis(1, at = seq_len(ncol(m)), labels = colnames(m))
  graphics::axis(2, at = seq_len(nrow(m)), labels = rev(rownames(m)), las = 1)
  graphics::mtext("moisture class (SPEI)", side = 2, line = 4.5)
  for (i in seq_len(nrow(m))) for (j in seq_len(ncol(m)))
    graphics::text(j, nrow(m) - i + 1, sprintf("%+.2f", m[i, j]), cex = 0.9)
  par(op)
}, out_rel(file.path("figures", "joint_obs_minus_independence_heatmap.png")), analysis_dir)

# ---- console summary ---------------------------------------------------------
message(sprintf("Pearson r(%s, %s) = %.3f  (r^2 = %.3f, %.1f%% shared var); VIF = %.3f",
                DROUGHT_INDEX, TEMP_INDEX_VAR, r, r2, 100 * r2, vif))
message(sprintf("Within-site r: mean %.3f, median %.3f (n_sites = %d)",
                mean(site_r), stats::median(site_r), length(site_r)))
message(sprintf("Chi-square = %.1f (df %d), Cramer's V = %.3f",
                as.numeric(chi$statistic), as.numeric(chi$parameter), cramers_v))
hd <- joint[joint$condition == "drought" & joint$temp_class == "hot", ]
message(sprintf("hot-dry occupancy: observed %.4f vs independence %.4f (lift %.2f)",
                hd$observed_prob, hd$expected_indep, hd$lift_obs_over_exp))
message("Axis-orthogonality diagnostics complete -> outputs/axis_orthogonality/")
