# make_wetland_allocation.R
# ---------------------------------------------------------------------------
# Derive the region-weighting inputs for the projection FROM WAD2M wetland area
# (WAD2M_FILE in config.R), instead of prescribing them. Computes the long-term
# mean wetland area per 0.5-deg cell (fraction Fw x cell area), then aggregates
# to latitude bands and to a continent x band table.
#
# Writes:
#   data/wetland_region_config.csv     band shares (budget_share) + lat bounds + LA
#   data/continent_band_allocation.csv continent x band fractions of global area
#
# NOTE: WAD2M gives wetland AREA. Using area fractions as budget shares assumes
# CH4 emissions scale with inundated area. To emission-weight instead, set a
# per-band flux factor in BAND_FLUX below (defaults to 1 = pure area).
#
# Requires: ncdf4 (install.packages("ncdf4")). Continents are assigned by
# inclusive lon/lat boxes so no shapefile is needed.
# ---------------------------------------------------------------------------

rm(list = ls())
script_file <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1])
analysis_dir <- if (!is.na(script_file)) dirname(normalizePath(script_file)) else getwd()
source(file.path(analysis_dir, "config.R"))
library(ncdf4)

BAND_FLUX <- c(Tropical = 1, Temperate = 1, Boreal = 1)   # set >1 for emission weighting
LA        <- c(Tropical = 1.2, Temperate = 1.5, Boreal = 2.2)  # land amplification (carried through)
BANDS     <- c("Tropical", "Temperate", "Boreal")
CONTS     <- c("North America", "South America", "Africa", "Europe", "Asia", "Oceania")

nc  <- nc_open(WAD2M_FILE)
lat <- ncvar_get(nc, "lat"); lon <- ncvar_get(nc, "lon")
tn  <- nc$dim$time$len
# long-term mean fraction (subsample ~every 21st month to bound I/O)
idx <- seq(1, tn, by = 21); acc <- matrix(0, length(lon), length(lat))
for (i in idx) {
  s <- ncvar_get(nc, "Fw", start = c(1, 1, i), count = c(-1, -1, 1))
  s[is.na(s)] <- 0; acc <- acc + s
}
nc_close(nc)
meanFw <- acc / length(idx)                          # lon x lat
R <- 6371; d <- 0.5 * pi / 180
cell_area <- outer(rep(1, length(lon)), (R * d) * (R * d * cos(lat * pi / 180)))  # lon x lat km^2
wet <- meanFw * cell_area

LONm <- outer(lon, rep(1, length(lat))); LATm <- outer(rep(1, length(lon)), lat)
abslat <- abs(LATm)
band <- ifelse(abslat <= 23.5, "Tropical", ifelse(abslat <= 50, "Temperate", "Boreal"))
# continent by inclusive boxes (priority order resolves overlaps)
cont <- matrix("", length(lon), length(lat))
setc <- function(name, m) cont[m & cont == ""] <<- name
setc("Europe",        LONm >= -25 & LONm <= 55  & LATm >= 40  & LATm <= 72)
setc("Africa",        LONm >= -20 & LONm <= 52  & LATm >= -36 & LATm <= 37)
setc("North America", LONm >= -170& LONm <= -52 & LATm >= 13  & LATm <= 84)
setc("South America", LONm >= -93 & LONm <= -33 & LATm >= -56 & LATm <= 13)
setc("Oceania",       LONm >= 110 & LONm <= 180 & LATm >= -50 & LATm <= -11)
setc("Asia",          LONm >= 40  & LONm <= 180 & LATm >= -11 & LATm <= 78)

# Russia = Europe convention: keep all of boreal Eurasia (essentially the West
# Siberian Lowland and the rest of Russia north of 50 N, the world's largest
# boreal peatland complex) with Europe rather than Asia. This only moves area
# WITHIN the boreal band, so band shares -- and every band-weighted projection --
# are unchanged; it only reassigns the continent split used by Figure 7.
cont[cont == "Asia" & band == "Boreal"] <- "Europe"

# continent x band area, emission-weighted by BAND_FLUX, normalised to sum 1
alloc <- matrix(0, length(CONTS), length(BANDS), dimnames = list(CONTS, BANDS))
for (c in CONTS) for (b in BANDS)
  alloc[c, b] <- sum(wet[cont == c & band == b]) * BAND_FLUX[b]
alloc <- alloc / sum(alloc)

alloc_df <- data.frame(continent = rownames(alloc), round(alloc, 4), row.names = NULL)
write.csv(alloc_df, file.path(analysis_dir, CONTINENT_ALLOC), row.names = FALSE)

band_share <- colSums(alloc)                          # = WAD2M band shares
reg <- data.frame(region = BANDS,
                  abs_lat_min = c(0, 23.5, 50), abs_lat_max = c(23.5, 50, 90),
                  budget_share = round(band_share[BANDS], 3), land_amplification = LA[BANDS])
write.csv(reg, file.path(analysis_dir, REGION_CONFIG), row.names = FALSE)

message("WAD2M global mean wetland area: ", round(sum(wet) / 1e6, 2), " Mkm2")
message("Band shares: ", paste(sprintf("%s=%.3f", BANDS, band_share[BANDS]), collapse = "  "))
message("Wrote ", REGION_CONFIG, " and ", CONTINENT_ALLOC,
        " -- rerun 19_RegionalProjection.R to apply.")
