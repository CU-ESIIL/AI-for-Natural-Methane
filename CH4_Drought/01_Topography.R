
rm(list=ls())
library(elevatr)
library(sf)
library(terra)
library(tidyverse)
library(spatialEco)

load(file="CH4_Drought/data/Fluxnet_Data.RDATA")

# Imports the site information
fluxnet <- ch4.sites.shp %>% 
  filter (FLUXNET.CH4 == "CC-BY-4.0" ) %>% st_transform(crs='+proj=longlat +datum=WGS84 +no_defs' )

Topo.df <- data.frame() # Dataframe to store information

for( i in 1:length( fluxnet$SITE_ID)) {
  print(fluxnet$SITE_ID[i])
  
  fluxnet.elev <- get_elev_raster(fluxnet[i,], z= 9, clip='bbox',  serial = TRUE ) %>% rast
  tpi <- terra::terrain(fluxnet.elev, v = "TPI", unit = "degrees", neighbors=8)
  tri <- terra::terrain(fluxnet.elev, v = "TRI", unit = "degrees", neighbors=8)
  slope <- terra::terrain(fluxnet.elev, v = "slope", unit = "degrees", neighbors=8)
  aspect <- terra::terrain(fluxnet.elev, v = "aspect", unit = "degrees", neighbors=8)
  roughness <- terra::terrain(fluxnet.elev, v = "roughness", unit = "degrees", neighbors=8)
  flowdir <- terra::terrain(fluxnet.elev, v = "flowdir", unit = "degrees", neighbors=8)
  
  

  ELEV <- global(fluxnet.elev, mean, na.rm=T)
  TRI <- global(tri, mean, na.rm=T)
  TPI <-global(tpi, mean, na.rm=T)
  SLOPE <- global( slope, mean, na.rm=T)
  ASPECT <- global( aspect, mean, na.rm=T)
  Rough <- global( roughness, mean, na.rm=T)
  FD <- global( flowdir, mean, na.rm=T)
  
  df <- data.frame(SITE_ID = fluxnet$SITE_ID[i]) %>% mutate( TRI = TRI$mean,
                TPI = TPI$mean,
                SLOPE= SLOPE$mean,
                ASPECT= ASPECT$mean,
                ROUGHNESS= Rough$mean,
                FLOWDIR= FD$mean,
                ELEV = ELEV$mean)

  Topo.df <- rbind( Topo.df, df )
}

write_csv(Topo.df, '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/Fluxnet_Topography.csv')

