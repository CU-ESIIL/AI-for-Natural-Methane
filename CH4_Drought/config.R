# Shared analysis settings for the CH4 extreme-event workflow.

# --- Moisture axis (SPEI): extreme-dry, normal, extreme-wet ---------------------
DROUGHT_INDEX <- "SPEI1"
DROUGHT_DURATION_VAR <- paste0("DI.", DROUGHT_INDEX, ".MeanDuration")

DROUGHT_THRESHOLD <- -1   # SPEI <= this  -> extreme dry ("drought")
WET_THRESHOLD <- 1        # SPEI >= this  -> extreme wet
NORMAL_LOWER <- -0.5
NORMAL_UPPER <- 0.5

# --- Thermal-season axis (temperature) ------------------------------------------
# A second, moisture-independent axis that classifies each observation by where it
# falls in ITS OWN site's annual temperature cycle. "winter" = cold season for that
# site, "summer" = warm season for that site (hemisphere-agnostic). Crossing this
# with the moisture axis separates, e.g., summer-dry from winter-dry events and
# summer-wet from winter-wet events, so patterns driven by hot vs cold conditions
# can be distinguished from those driven by wet vs dry conditions.
THERMAL_SEASON_VAR   <- "thermal_season"  # categorical: winter / shoulder / summer
THERMAL_BASIS        <- "site_month_clim" # "site_month_clim" (season by monthly climatology)
                                          #   or "observation" (classify each obs by its own TA_F)
THERMAL_LOWER_PROB   <- 1/3               # <= this within-site quantile -> winter (cold season)
THERMAL_UPPER_PROB   <- 2/3               # >= this within-site quantile -> summer (warm season)
THERMAL_USE_SHOULDER <- TRUE              # FALSE -> binary winter/summer split at the site median
THERMAL_CONT_VAR     <- "TA_site_z"       # continuous site-standardized temperature for models

# --- Temperature-anomaly axis (hot / normal / cold EVENTS) ----------------------
# The true analog to the SPEI moisture axis, but for temperature. TA_F is
# standardized WITHIN each site-month (a Standardized Temperature Index, STI),
# which removes both the site's climate and its seasonal cycle, so a "hot" event
# is hotter than normal *for that site and time of year* and a "cold" event is
# colder than normal. Unlike the seasonal winter/summer axis, this one has an
# explicit NORMAL class, mirroring drought / normal / extreme-wet.
TEMP_CLASS_VAR  <- "temp_class"  # categorical: cold / normal / hot
TEMP_INDEX_VAR  <- "STI"         # standardized temperature index (site-month z-score of TA_F)
HOT_THRESHOLD   <- 1             # STI >= this  -> hot event
COLD_THRESHOLD  <- -1            # STI <= this  -> cold event  (between -> normal)
TEMP_MIN_MONTH_OBS <- 5          # min obs in a site-month to estimate the STI baseline

# --- Site selection: non-target IGBP classes to exclude -------------------------
# Single source of truth for the land-cover classes dropped from the analysis:
# CRO = cropland, URB = urban, SNO = snow/ice, WAT = open water (e.g. DE-Dgw lake).
# Referenced by 05_CompileData.R (FinalDrought table), 06_BuildAnalysisTable.R
# (canonical DroughtAnalysis table), and 10_Figure_MAP.R (site maps / SiteList) so
# the three stay consistent. Edit here to add/remove an excluded class everywhere.
EXCLUDE_IGBP <- c("CRO", "URB", "SNO", "WAT")

# Additional site-level exclusions (by SITE_ID), for towers whose IGBP class is kept
# but which are upland sites with no wetland in or adjacent to the footprint:
#   AT-Neu (montane hay meadow), CH-Cha (lowland grassland), CH-Dav (subalpine
#   conifer forest), FI-Hyy (boreal pine forest; Siikaneva fen is ~5-6 km away).
# US-Ho1 is retained (scattered wetland patches within the footprint). Applied
# alongside EXCLUDE_IGBP in 05_CompileData.R, 06_BuildAnalysisTable.R, 10_Figure_MAP.R.
EXCLUDE_SITES <- c("AT-Neu", "CH-Cha", "CH-Dav", "FI-Hyy")

# --- Server / output locations --------------------------------------------------
# Canonical outputs are written to the project outputs/ folder AND (when
# reachable) mirrored to the lab server so downstream scripts and collaborators
# always see the latest files. Edit SERVER_DIR for your mount.
SERVER_DIR      <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2"
SERVER_OUTPUTS  <- file.path(SERVER_DIR, "CH4_Drought", "outputs")
WRITE_TO_SERVER <- TRUE           # set FALSE to skip the server mirror

# --- Climate-projection settings (see 18_ExtremeEmissionsProjection.R) -----------
PROJECTION_SSPS       <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
PROJECTION_BASE_YEAR  <- 2020      # reference period the frequency table departs from
PROJECTION_END_YEAR   <- 2100
PROJECTION_STEP_YEARS <- 10        # decadal steps
GLOBAL_WETLAND_BUDGET_TG <- 150    # global wetland CH4 source, Tg/yr (Saunois et al., 2025)
# Editable frequency table (marginal probabilities of hot/cold/dry/wet by decade
# and SSP). Produced by 17_ScenarioFrequencies_CMIP6.R from SSP warming pathways.
PROJECTION_FREQ_TABLE <- "data/projection_frequency_scenarios.csv"

# --- Scenario frequency model (17_ScenarioFrequencies_CMIP6.R) ------------------
# Future hot/cold/dry/wet frequencies are derived by shifting the observed
# standardized distributions under each SSP's global-warming pathway (CMIP6/AR6).
SSP_GWL_TABLE      <- "data/ssp_global_warming_levels.csv"  # year, ssp, gwl (deg C vs preindustrial)
GWL_BASELINE       <- 1.15   # warming of the observational baseline period (2006-2019)
LAND_AMPLIFICATION <- 1.5    # local site warming per unit global warming (land/latitude)
MOISTURE_DRY_SENS  <- 0.15   # SPEI drying (units) per deg C global warming (PET-driven)
MOISTURE_WET_SENS  <- 0.05   # wet-tail intensification per deg C global warming
# Optional: point these at raw CMIP6 tas/pr extracted at site locations to bypass
# the distribution-shift model entirely (see 17_ScenarioFrequencies_CMIP6.R).
CMIP6_SITE_DIR     <- file.path(SERVER_DIR, "CMIP6", "site_extractions")

# --- Projection uncertainty -----------------------------------------------------
PROJECTION_MC_DRAWS <- 3000  # Monte Carlo draws propagating response-function SE

# --- Region-weighted global projection (19_RegionalProjection.R) -----------------
# Instead of scaling the site response by one global budget, weight each latitude
# band by its share of the global wetland CH4 source and its own warming and
# response. Editable band definitions / shares / land amplification:
REGION_CONFIG <- "data/wetland_region_config.csv"
# Continent x latitude-band budget allocation (fractions of the global budget;
# columns are the bands, rows continents; band column sums MUST equal the band
# shares in REGION_CONFIG so continents sum exactly to the band-weighted global).
CONTINENT_ALLOC <- "data/continent_band_allocation.csv"
# Continent polygons for the choropleth (Figure 7); Natural Earth 50 m.
CONTINENT_SHAPEFILE <- "data/shapefiles/natural_earth/ne_50m_continents/ne_50m_continents.shp"
# WAD2M wetland dataset — the band shares and continent x band allocation above
# are DERIVED from it by make_wetland_allocation.R (run once / when WAD2M updates).
WAD2M_FILE <- file.path("/Volumes/MaloneLab/Research/FluxGradient/METHANE",
                        "Upscaling_Monthly/DATA/wad2m",
                        "WAD2M_wetlands_2000-2020_05deg_Ver2.0.nc")
