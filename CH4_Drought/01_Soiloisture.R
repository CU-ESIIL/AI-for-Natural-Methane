# Extract soil moisture data for Fluxnet sites:


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

# Import the soilmoisture data:

# Soil Moisture
fluxnet <- ch4.sites.shp %>% 
  filter (FLUXNET.CH4 == "CC-BY-4.0" ) %>% st_transform(crs='+proj=longlat +datum=WGS84 +no_defs' )
fluxnet$SITE_ID
library(ncdf4)
example_nc <- nc_open('/Volumes/MaloneLab/Research/soil_moisture_FLUXNET_site_Daily_2000_2023_Youmi_Oh.nc')
ncvar_get(example_nc, "year")
ncvar_get(example_nc, "doy")
example_nc$dim$site$vals
test <- ncvar_get(example_nc) %>% data.frame()

example_nc$dim$site$vals
names(test) <- example_nc$dim$site$vals
nc_close(example_nc)



