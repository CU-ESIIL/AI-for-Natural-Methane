# Soil Moisture from Terra Climate:
library(sf)
library(climateR)
library(tidyverse)

project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data"
setwd( project.data.dir)

load( file='FinalDrought_Data.RDATA')

sf_object <- st_as_sf(fluxes.drought_normalized, crs = 4326) %>% select( SITE_ID, geometry) %>% distinct

soil_moisture_data_SITES <-data.frame()

for( i in 1:length(sf_object$SITE_ID)){
  print(i)
  soil_moisture_data <- getTerraClim(
    AOI = sf_object[i,],
    varname = "soil",
    startDate = "2000-01-01",
    endDate = "2020-12-31") %>% mutate( SITE_ID = sf_object$SITE_ID[i])
  
  soil_moisture_data_SITES <- rbind( soil_moisture_data_SITES, soil_moisture_data)
  print("done")
}

write.csv(soil_moisture_data_SITES, 'TERRA_SWC_FLUXNETSITES.csv' )
