# CH4 Drought Analysis

This folder contains the working analysis for drought effects on FLUXNET-CH4 methane fluxes. The current workflow is script-based and writes intermediate `.RDATA`, `.csv`, and figure outputs to a mix of external lab storage and this repository.

## Project Roots

Most scripts currently assume these roots:

```r
project_repo_dir <- "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane"
analysis_dir <- file.path(project_repo_dir, "CH4_Drought")
project_data_dir <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/data"
ecmwf_drought_dir <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/Drought/ECMWF_DroughtIndices_Global"
figure_dir <- file.path(analysis_dir, "FIGURES")
local_data_dir <- file.path(analysis_dir, "data")
```

Several scripts use hard-coded absolute paths. If the analysis is moved to a new machine, update those roots before rerunning.

Shared settings live in `config.R`. The workflow currently uses `DROUGHT_INDEX <- "SPEI1"` for the moisture axis (extreme-dry / normal / extreme-wet), normalization windows, linear models, random-forest predictors, Q10 analyses, and condition-change analyses.

## Temperature axes (seasonal timing and anomaly)

`config.R` defines two moisture-independent temperature axes, both computed in `temperature_index.R` and woven through the workflow (`05_CompileData`, `07_ConditionChanges`, `08_RefitRandomForest`, `11_Linear`, `12_Q10`):

1. **Thermal season** (`add_thermal_season()` -> `thermal_season`): labels each observation as a `winter` (cold-season), `summer` (warm-season), or `shoulder` event *relative to that site's own annual temperature cycle*. This is a **timing** axis — winter/summer are the expected states for those times of year, so it has no "normal" class. Controlled by `THERMAL_LOWER_PROB`/`THERMAL_UPPER_PROB` (default terciles), `THERMAL_BASIS`, and `THERMAL_USE_SHOULDER`. Outputs: `CH4_Drought/outputs/temperature_events`.

2. **Temperature anomaly** (`add_temp_anomaly_class()` -> `temp_class`, `STI`): the direct analog of the SPEI moisture axis. `TA_F` is standardized *within each site-month* to a Standardized Temperature Index (STI), then classified as `hot` (STI >= `HOT_THRESHOLD`, default +1), `cold` (STI <= `COLD_THRESHOLD`, default -1), or `normal` (in between). A `hot` event is hotter than normal *for that site and time of year*, so this axis **has an explicit normal class** and cleanly separates a heatwave from an ordinary warm month. Outputs: `CH4_Drought/outputs/temperature_anomaly_events`.

Crossing either axis with the SPEI condition via `add_event_class()` builds the event grid (`summer_dry`/`winter_wet` for the seasonal axis; `hot_dry`/`cold_wet`/`normal_normal` for the anomaly axis), separating methane patterns driven by temperature from those driven by wet vs dry conditions. Use the **seasonal** axis for the winter-vs-summer contrast and the **anomaly** axis (with its normal reference) for hot-vs-cold *event* effects.

## Run Order

Run the scripts in this order when rebuilding the analysis from raw/source data.

Scripts are numbered in execution order. Steps 01–12 (data build + analysis) are the table below; steps 13–22 (model comparison, projection, figures) are described in the sections that follow. The pipeline runner `run_pipeline.R` executes these in dependency order.

| Step | Script | Main inputs | Main outputs |
| --- | --- | --- | --- |
| 01 | `01_CompileFluxnet.R` | FLUXNET zip/csv files and site metadata in `project_data_dir` | `Fluxnet_Data.RDATA` (`ch4.sites`, `ch4.sites.shp`, `CH4.Flux.HH.units`, `CH4.Flux.DD`) |
| 02 | `02_Terra_SVWC.R` | compiled flux/site data | `TERRA_SWC_FLUXNETSITES.csv` |
| 03 | `03_Topography.R` | `Fluxnet_Data.RDATA` | `Fluxnet_Topography.csv` |
| 04 | `04_DroughtIndices_Sites.R` | `Fluxnet_Data.RDATA`, ECMWF drought-index NetCDF files in `ecmwf_drought_dir` | `ECMWF_FLUXNET_CH4.RDATA`, merged daily/monthly drought-index flux table |
| 05 | `05_CompileData.R` | `Fluxnet_Data.RDATA`, `TERRA_SWC_FLUXNETSITES.csv`, `Fluxnet_Topography.csv`, `fluxes_drought_with_EVI.csv` | `FinalDrought_Data.RDATA` (`Drought.DF.final`, `fluxes.drought`, `fluxes.drought_normalized`) |
| 06 | `06_BuildAnalysisTable.R` | `FinalDrought_Data.RDATA` | **canonical** `data/DroughtAnalysis.RDATA` (`fluxes.drought_normalized`, with non-target IGBP classes dropped) — the shared input for steps 07–21 |
| 07 | `07_ConditionChanges.R` | `data/DroughtAnalysis.RDATA` | condition summaries, site-month-matched deltas, event grids, two-way models, figures in `outputs/condition_changes` and `outputs/temperature_anomaly_events` |
| 08 | `08_RefitRandomForest.R` | `data/DroughtAnalysis.RDATA`, `config.R` | overwrites `DroughtAnalysis.RDATA`, adding the fitted random forest for normalized CH4 |
| 09 | `09_RF_Importance.R` | `data/DroughtAnalysis.RDATA` (RF from step 08) | `rf_variable_importance.csv` + importance figure |
| 10 | `10_Figure_MAP.R` | `Fluxnet_Data.RDATA`, `FinalDrought_Data.RDATA` | site maps, SPEI-range figures, `SiteList.RDATA` (used by step 12) |
| 11 | `11_Linear.R` | `data/DroughtAnalysis.RDATA` | site-blocked FCH4~SPEI terms stratified by season and hot/cold anomaly, `FinalDrought_Data_LineaModel.RDATA` |
| 12 | `12_Q10.R` | `data/DroughtAnalysis.RDATA`, `Fluxnet_Data.RDATA`, `SiteList.RDATA`, `calc_Q10.R` | Q10/Rref outputs and figures |

Steps 06, 11, and 12 all read the same canonical `DroughtAnalysis.RDATA`, so the linear and Q10 analyses use the identical site set to the condition grid and projections.

Older exploratory scripts (`04_DroughtAnalysis.R`, `04_Exploratory Analysis.R`) live in `depreciated/` and are not part of the pipeline.

`temperature_index.R` is a shared helper (not a numbered step). It is sourced by `05_CompileData`, `07_ConditionChanges`, `08_RefitRandomForest`, `11_Linear`, and `12_Q10`, and adds the thermal-season columns (`thermal_season`, `TA_site_z`, `TA_site_anom`, plus site/site-month climatology) on demand, so any script can be rerun without a specific ordering as long as the flux table carries `SITE_ID`, `month`, and `TA_F`.

## Model comparison (multi-model ready)

Model evaluation is model-agnostic. Every model to compare against FLUXNET-CH4 is declared in `models.R` (`MODEL_REGISTRY`); **to add a model, append one entry** giving its daily-conditions `.RDATA`, temperature column, and per-scenario flux columns — no other code changes. `13_TEM_MDM_ModelOutputs.R` builds the TEM-MDM daily table (now including the `temp_class` axis) and is the template for other model builders. `15_Compare_FLUXNET_Models.R` then loops over the registry and compares each model to the towers along both (a) the SPEI moisture axis and (b) the compound moisture × temperature-anomaly grid, writing per-model and combined outputs to `outputs/fluxnet_model_comparison/`. The manuscript TEM-MDM evaluation figure (Fig 6) is built from those CSVs by `22_Figures.R` (`fig5`).

## Server outputs

`config.R` defines `SERVER_DIR` / `SERVER_OUTPUTS` and `WRITE_TO_SERVER`. The helpers in `io_helpers.R` (`save_output_csv`, `save_output_rdata`, `save_output_figure`, `save_output_ggplot`) write every output to the project `outputs/` folder **and** mirror it to the lab server when mounted, so downstream scripts and collaborators always see the latest files.

## Extreme-emissions projection (to 2100)

The projection runs in two steps. `17_ScenarioFrequencies_CMIP6.R` builds the future frequency of hot/cold/dry/wet events for every SSP by shifting the observed standardized temperature (STI) and moisture (SPEI) distributions under each SSP's global-warming pathway (`data/ssp_global_warming_levels.csv`, CMIP6/AR6): local warming = `LAND_AMPLIFICATION` × global warming, rescaling the ±1 SD class tails, with moisture drying/intensification via `MOISTURE_DRY_SENS`/`MOISTURE_WET_SENS`. It writes `data/projection_frequency_scenarios.csv`. A `cmip6-direct` mode reads raw per-site CMIP6 `tas`/`pr` from `CMIP6_SITE_DIR` when staged, bypassing the shift model.

`18_ExtremeEmissionsProjection.R` then projects additional wetland methane for all SSPs. It (1) computes each compound class's site-level mean response **and between-site SE** from the flux data; (2) turns the marginal frequencies into the **joint** 3×3 occupancy by Iterative Proportional Fitting of the *observed* joint table to the scenario marginals, which **preserves the observed hot-dry co-occurrence** rather than assuming independence; and (3) propagates the response SE with a Monte-Carlo (`PROJECTION_MC_DRAWS`) to report 5–95% ranges. Additional emission = (ΔE/F0) × global wetland budget (`GLOBAL_WETLAND_BUDGET_TG`). Outputs (median + 5–95% by decade, and a summary) go to `outputs/extreme_emissions_projection/`.

`19_RegionalProjection.R` is the fixed-area regional/global upscaling step. It applies latitude-band wetland budget shares and band-specific responses from `data/wetland_region_config.csv`, then writes `outputs/regional_projection/`. This fixed-area table (`regional_global_projection.csv`) is now retained as a **comparison**; the headline projection is the variable-inundation result from step 19c (see the next section). The single global site-scaled projection from step 18 is also kept for comparison. `20_ContinentChoropleth.R` maps the headline continent-contribution table (variable-inundation from 19c, with fixed-area 19 as fallback) using the local Natural Earth 1:50m continent shapefile at `data/shapefiles/natural_earth/ne_50m_continents_no_antarctica/ne_50m_continents_no_antarctica.shp`, writing `outputs/regional_projection/figures/continent_choropleth.png`.

### Inundation-varying projection (19b calibrate → 19c project)

The headline projection lets the **inundated wetland area** itself respond to warming, rather than holding it fixed as step 19 does. It runs in two lettered steps whose ascending order matches the dependency (calibrate, then project):

- **`19b_CalibrateInundationElasticities.R`** calibrates the dry and wet area elasticities (`e_dry`, `e_wet`) from observations: it regresses log wetland-area anomaly (WAD2M `Fw`) on the standardized moisture anomaly (ECMWF SPEI), separately for the dry and wet halves of the SPEI axis, per latitude band, as an area-weighted least-squares fit through the origin with a year-block bootstrap for uncertainty. Uses the `terra` package for raster extraction. It writes `data/inundation_elasticities.csv` and `outputs/regional_projection_inundation/inundation_calibration_fit.csv`. **This step is slow** (WAD2M × SPEI extraction + bootstrap) and needs the lab server volume, so it is **run manually when recalibrating** and is **excluded from the automated pipeline**.

- **`19c_RegionalProjection_InundationVarying.R`** is the headline projection. It extends step 19 by scaling each band's baseline inundated area by an asymmetric, warming-driven factor `A_r(t) = clamp(exp(e_wet·wet_forcing − e_dry·dry_forcing))`, with the moisture forcing set by each SSP's global-warming level and `MOISTURE_DRY_SENS`/`MOISTURE_WET_SENS`. When `A ≡ 1` it collapses exactly to step 19, so the difference is purely the inundation effect. A Monte-Carlo propagates both the flux-response SE and the elasticity uncertainty. It auto-detects `data/inundation_elasticities.csv` (from 19b) and falls back to the `INUND_*` config defaults if absent. Outputs go to `outputs/regional_projection_inundation/`: `regional_global_projection_inundation.csv` (the **headline** table), `regional_breakdown_2100.csv`, `continent_contributions_2100_inundation.csv`, `regional_vs_fixed_area_2100.csv`, and both diagnostic figures (the varying-vs-fixed projection and the calibration diagnostic, the latter rebuilt from 19b's CSVs so it does not require rerunning the slow calibration).

Steps 20, 21, and Figures 6–7 (`22_Figures.R`) use these variable-inundation (19c) outputs as the headline, with the fixed-area (19) tables as the comparison/fallback.

The local continent shapefiles are derived from the downloaded Natural Earth 1:50m Admin 0 countries dataset (`data/shapefiles/natural_earth/ne_50m_admin_0_countries.zip`) by dissolving country polygons on the Natural Earth continent field. Both an all-continent shapefile and a no-Antarctica shapefile are stored under `data/shapefiles/natural_earth/`.

`21_SensitivityAndDiagnostics.R` writes `outputs/diagnostics/`, including global-vs-regional projection comparisons, model-agreement flags, sparse-bin flags, regional uncertainty flags, deterministic assumption sensitivity, and leave-one-site-out influence checks. These diagnostics are intended to make the limitations visible rather than buried in intermediate tables.

### Axis orthogonality (SPEI moisture vs STI temperature)

`21b_AxisOrthogonality.R` quantifies how independent the two axes of the workflow actually are — the SPEI moisture axis (`DROUGHT_INDEX`) and the Standardized Temperature Index (`STI`, the site-month z-score of `TA_F`). This is not automatic: SPEI is a standardized P − PET water balance and PET depends on temperature, so temperature enters the moisture index by construction, and hot/dry conditions also co-occur physically (compound events, soil-moisture–temperature feedback). The script reads only the canonical `data/DroughtAnalysis.RDATA` and writes `outputs/axis_orthogonality/`: the SPEI–STI Pearson/Spearman correlation (pooled, site-blocked, and site-demeaned) with r² and the two-predictor VIF; the 3×3 `condition × temp_class` contingency table with a chi-square test and Cramér's V; and the observed joint occupancy versus the product-of-marginals expectation (with the hot–dry lift called out), plus two figures. In the current data the axes are near-orthogonal after the within-site-month standardization (|r| ≈ 0.03, Cramér's V ≈ 0.02, hot–dry enrichment only ~10% over independence), which supports treating moisture and temperature as separate axes while still using IPF (steps 18/19/21) to preserve the small observed hot–dry dependence rather than assuming independence. It runs at the end of the model/projection block in `run_pipeline.R`.

## Automation

`run_pipeline.R` runs the pipeline in dependency order in fresh Rscript processes, logging status/timing to `outputs/pipeline_log/`, and printing a one-line description of each step. By default it runs the model/projection block **13 → 21**: 13 TEM outputs, 14 short/long drought, 15 model comparison, 16 response curves, 17 scenario frequencies, 18 projection, 19 fixed-area regional projection, **19c variable-inundation projection (headline)**, 20 continent choropleth, 21 diagnostics, and 21b axis-orthogonality diagnostics. The slow calibration **19b is not run automatically** — 19c uses its saved `data/inundation_elasticities.csv` (or config defaults). Groups: `Rscript run_pipeline.R` (default, the 13–21 block), `regional` (19 + 19c projections + 20 continent map + 21 diagnostics), `analysis` (06–12 analysis + figures — builds `DroughtAnalysis.RDATA` first, then condition/RF/map/linear/Q10), `data` (01–05 upstream build), or `all` (everything). Figures are then rebuilt by `22_Figures.R`. `run_all.R` is a thin forwarding stub kept for backward compatibility.

## Key Analysis Objects

- `Drought.DF.final`: merged site metadata, fluxes, drought indices, EVI, soil moisture, and topography.
- `fluxes.drought`: methane flux observations with drought covariates.
- `fluxes.drought_normalized`: modeling table where `normalized_Fch4 = FCH4_F_ANNOPTLM - FCH4.normal`.
- `FCH4.normal`: each site's mean `FCH4_F_ANNOPTLM` during near-normal SPEI48 conditions (`-0.5 < SPEI48 < 0.5`).
- `Normalizex.spei48.model.rf`: random forest model for normalized methane flux.
- `Normalizex.spei48.model.rf.SA.DF`: sensitivity-analysis table generated from the fitted random forest.

## Current Notes

- The stored `DroughtAnalysis.RDATA` in `CH4_Drought/data` contains `fluxes.drought_normalized`, the fitted random forest, and the RF sensitivity table.
- `CH4_Drought/data/FinalDrought_Data.RDATA` may be absent in the current working tree, even though several downstream scripts load it.
- Stale `CH4_DROUGHT` figure paths have been normalized to `CH4_Drought`; keep path capitalization consistent on case-sensitive filesystems.
- The random-forest train/test split is row-based. For testing generalization to new sites or time periods, consider adding site-blocked or time-blocked validation.
- `07_ConditionChanges.R` defines drought as `SPEI1 <= -1`, normal as `-1 < SPEI1 < 1`, and extreme wet as `SPEI1 >= 1`. Deltas are calculated against a site-month normal baseline before being aggregated to sites, so seasonal effects are reduced.
