# This Script obtains ERA5 based drought indices for FLUXNET CH4 sites at different resolutions:

library(tidyverse) 
library(sf)
library(terra)
library(AOI)

# Extract Drought Indices for sites: ####
project.dir <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/CH4_Drought"
setwd( project.dir)

load(file="Fluxnet_Data.RDATA")

# Imports the site information
fluxnet <- ch4.sites.shp %>% 
  filter (FLUXNET.CH4 == "CC-BY-4.0" ) %>% st_transform(crs='+proj=longlat +datum=WGS84 +no_defs' )

global <- aoi_get(country= c("Europe","Asia" ,"North America", "South America", "Australia","Africa", "New Zealand"))

global = st_transform(global, crs= '+init=epsg:4087') %>% st_make_valid()

ggplot() + geom_sf(data = global) + geom_sf(data = fluxnet )+ theme_bw()

# ECMWF: Data stored on my server
ECMWF.dir <- '/Volumes/MaloneLab/Research/Natural_CH4_CO2/Drought/ECMWF_DroughtIndices_Global'
setwd(ECMWF.dir)

# Make a list of all the raster layers:
nc.files <- list.files(path='/Volumes/MaloneLab/Research/Natural_CH4_CO2/Drought/ECMWF_DroughtIndices_Global', pattern='.nc' )

# Extract data in a forloop and build a dataframe:
  FINAL.dataframe <- data.frame()
  
  for( i in 1: length(nc.files)){
    
    print(i)
    raster.layer <- terra::rast(nc.files[i])
    names(raster.layer )
    fluxnet$Drought.Indice <- terra::extract(raster.layer, fluxnet)[ names(raster.layer )]
    fluxnet$Indice.type <- names(raster.layer )
    fluxnet$time <- time(raster.layer)
    FINAL.dataframe <-rbind( FINAL.dataframe, fluxnet)
    fluxnet$Indice <-fluxnet$time <- fluxnet$Indice.type <-NULL
    print(paste('done with', i))
  }
  
  drought.data.fluxnet.sites <-   FINAL.dataframe
  
  # Change dataframe from long to wide format:
  drought.data.fluxnet.sites.final <- drought.data.fluxnet.sites %>% as.data.frame() %>% 
    pivot_wider(names_from = Indice.type, values_from = Drought.Indice, id_cols= c(SITE_ID, time)) %>%
    mutate(SPEI1 = SPEI1$SPEI1,
           SPEI12 = SPEI12$SPEI1,
           SPEI24 = SPEI24$SPEI1,
           SPEI3 = SPEI3$SPEI1,
           SPEI36 = SPEI36$SPEI1, 
           SPEI48 = SPEI48$SPEI1,
           SPEI6 = SPEI6$SPEI1,
           SPI1 = SPI1$SPEI1,
           SPI12 = SPI12$SPEI1,
           SPI24 = SPI24$SPEI1,
           SPI3 = SPI3$SPEI1,
           SPI36 = SPI36$SPEI1, 
           SPI48 = SPI48$SPEI1,
           SPI6 = SPI6$SPEI1)

  # Save the data:
save( drought.data.fluxnet.sites.final, file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/CH4_Drought/data/ECMWF_FLUXNET_CH4.RDATA' )

# Merge the flux data with the drought indices: ####
rm(list=ls())

project.dir <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/CH4_Drought"
setwd( project.dir)

load(file="Fluxnet_Data.RDATA")
load(file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/CH4_Drought/data/ECMWF_FLUXNET_CH4.RDATA' )

# format the time elements to prepare to join:

global <- aoi_get(country= c("Europe","Asia" ,"North America", "South America", "Australia","Africa", "New Zealand"))

drought <- drought.data.fluxnet.sites.final %>% mutate(YearMon = format(time, "%Y-%m"),
                                                       drought = case_when(SPEI1 <= -1 ~'Drought',
                                                                           SPEI1 > -1 ~'normal',
                                                                           SPEI1 > 1 ~'wet'))

# TImestamp formatting:
CH4.Flux.DD <- CH4.Flux.DD %>% mutate( Date = TIMESTAMP %>% as.character %>% as.Date( format='%Y%m%d'),
                                       YearMon= format(Date, "%Y-%m"),
                                       month = format(Date , '%m'))


fluxes.drought <- CH4.Flux.DD %>% full_join( drought, by = c('YearMon', 'SITE_ID'))

fluxes.drought %>% ggplot(aes(x= SPEI1, y=SITE_ID)) + geom_boxplot()
fluxes.drought %>% ggplot(aes(x= FCH4_F_ANNOPTLM, col=drought)) + geom_boxplot() + facet_wrap(~SITE_ID)
