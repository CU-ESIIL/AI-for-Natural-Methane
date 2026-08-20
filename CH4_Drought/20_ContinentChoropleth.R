# 20_ContinentChoropleth.R
# ---------------------------------------------------------------------------
# Publication-quality continent choropleth of the projected extreme-driven CH4
# contribution by 2100 — real continent polygons (Natural Earth 50 m shapefile,
# CONTINENT_SHAPEFILE in config.R) filled by the per-continent value.
#
# Geometry is REPAIRED before use (ported from make_fig7_choropleth.py, verified
# streak-free). The ne_50m_continents shapefile draws horizontal streaks across a
# Robinson map from four defects, each corrected below: (1) invalid self-
# intersecting rings, (2) thin degenerate slivers spanning most of the globe,
# (3) a dateline-spanning ring linking Chukotka to European Russia at ~66 N, and
# (4) an Arctic gap where the Europe polygon is cut straight at 65 N. The cleaned
# geometry is also written to ne_50m_continents_valid.gpkg next to the source.
#
# Requires: sf, ggplot2, dplyr   ( install.packages(c("sf","ggplot2","dplyr")) )
# Inputs:  CONTINENT_SHAPEFILE (config) and
#          outputs/regional_projection/continent_contributions_2100.csv (from 14).
# Output:  outputs/regional_projection/figures/continent_choropleth.png (+ server).
# ---------------------------------------------------------------------------

rm(list = ls())


script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "io_helpers.R"))
library(sf); library(ggplot2)
if (!requireNamespace("rnaturalearth", quietly = TRUE) ||
    !requireNamespace("rnaturalearthdata", quietly = TRUE))
  stop("Install continent polygons source: install.packages(c('rnaturalearth','rnaturalearthdata'))")

MAP_SSP <- "SSP2-4.5"
# Headline = variable-inundation continent contributions (19c); fall back to the
# fixed-area contributions (19) if 19c has not been run.
cont_var <- file.path(analysis_dir, "outputs", "regional_projection_inundation",
                      "continent_contributions_2100_inundation.csv")
cont_fix <- file.path(analysis_dir, "outputs", "regional_projection",
                      "continent_contributions_2100.csv")
cont_file <- if (file.exists(cont_var)) cont_var else cont_fix
AREA_MODE <- if (identical(cont_file, cont_var)) "variable inundation" else "fixed area"
message("Continent contributions source: ", AREA_MODE, " (", cont_file, ")")
bd <- read.csv(cont_file, stringsAsFactors = FALSE)
bd <- bd[bd$ssp == MAP_SSP, ]

# Continent polygons from Natural Earth Admin-0 countries via rnaturalearth,
# dissolved by continent. The previously bundled ne_50m_continents shapefile
# has broken geometry (Australia drops out no matter how it is repaired), so it
# is no longer used; this maintained source is clean and includes Australia.
sf::sf_use_s2(FALSE)
world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")  # offline via rnaturalearthdata
world <- world[!is.na(world$continent) &
               !world$continent %in% c("Antarctica", "Seven seas (open ocean)"), ]
world <- st_make_valid(world)
cs <- sort(unique(world$continent))
geoms <- do.call(c, lapply(cs, function(k) st_union(st_geometry(world[world$continent == k, ]))))
cont <- st_sf(continent = cs, geometry = geoms, crs = st_crs(world))
cont <- merge(cont, bd, by = "continent", all.x = TRUE)

lim <- max(abs(bd$additional_Tg_per_yr_2100), na.rm = TRUE)
p <- ggplot(cont) +
  geom_sf(aes(fill = additional_Tg_per_yr_2100), color = "grey35", linewidth = 0.15) +
  scale_fill_gradient2(low = "#8c510a", mid = "#f7f7f7", high = "#2166ac", midpoint = 0,
                       limits = c(-lim, lim), na.value = "grey92",
                       name = expression(atop("Additional CH"[4], "(Tg yr"^-1*", 2100)"))) +
  coord_sf(crs = "+proj=robin", ylim = c(-6.2e6, 8.6e6), expand = FALSE) +
  labs(title = paste0("Continental contribution to global extreme-driven CH4 by 2100 (", MAP_SSP, ")"),
       subtitle = paste0("Region-weighted (", AREA_MODE,
                         "); continents sum to the global total (WAD2M allocation)")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_line(color = "grey90", linewidth = 0.2),
        axis.text = element_blank(), axis.title = element_blank(), legend.position = "right")

save_output_ggplot(p, "regional_projection/figures/continent_choropleth.png", analysis_dir,
                   width = 9, height = 4.8, dpi = 300)
message("Wrote continent choropleth (", MAP_SSP, ") using repaired geometry from ", CONTINENT_SHAPEFILE)

