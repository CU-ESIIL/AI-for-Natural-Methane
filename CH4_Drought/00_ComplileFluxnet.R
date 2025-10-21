library(gtools)
library(tidyverse)
library(gtools)
library(sf)

rm(list=ls())

# Unzip the files needed #####
data.dir <- '/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/FLUXNET_CH4_T1'
# COMPILE FLUX NET CH data downloaded from website: This has been changed to include only tier 1 data to avoid isues with connecting with authors.
setwd(data.dir)

# unzip files and extract HH and DD datasets# 

zip.files <- list.files(data.dir, pattern = ".zip")

for(k in 1:length(zip.files)){
  unzip(zipfile = file.path(data.dir, zip.files[k]), exdir = file.path("/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"))
 }
 
# Import and build Files: ####
data.dir <- '/Volumes/MaloneLab/Research/Natural_CH4_CO2/data'
folders <- list.files( data.dir, pattern= "FLX_")

CH4.Flux.HH <- data.frame( )
CH4.Flux.DD <- data.frame( )

for( i in folders){
  print(i)
  
  new.dir <- paste('/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/',i, sep="" )
  file.HH <- list.files( new.dir, pattern= "HH", full.names=T)
  file.DD <- list.files( new.dir, pattern= "DD", full.names=T)
  
  getsite <- list.files( new.dir, pattern= "HH")
  
  HH <- read.csv(  file.HH ) %>% select( TIMESTAMP_START , NEE_F_ANNOPTLM, FCH4_F_ANNOPTLM, LE_F_ANNOPTLM, VPD_F, PA_F, TA_F, P_F) %>% 
    mutate( SITE_ID = substr(getsite, 5, 10))
  DD <- read.csv(  file.DD ) %>% select( TIMESTAMP , NEE_F_ANNOPTLM, FCH4_F_ANNOPTLM,LE_F_ANNOPTLM, VPD_F, PA_F, TA_F, P_F) %>% 
    mutate( SITE_ID = substr(getsite, 5, 10))
  
  CH4.Flux.HH <- smartbind( CH4.Flux.HH, HH)
  CH4.Flux.DD <- smartbind( CH4.Flux.DD, DD)

  print("Done")
}

#Unit changes Next
CH4.Flux.HH.units <- CH4.Flux.HH %>% mutate( LE_HH = LE_F_ANNOPTLM*1800,
                        ET_mm =((LE_HH/22.6e5)/1e3)*1e3,
                        NEE_HH = as.numeric(NEE_F_ANNOPTLM)*1800,
                        CH4_HH = as.numeric(FCH4_F_ANNOPTLM)*1800, #units for NEE are umolCO2 m-2 s-1
                        FCH4_gC <- as.numeric(CH4_HH)*1e-9*12.011,
                        CH4_gC <- as.numeric(CH4_HH)*1e-9*12.011 )



# Get site level information table to make a shape_file for downloading spectral data.
sites <- read.csv( "~/Dropbox (YSE)/Research/Fluxnet_CH4/FluxNetSites.csv") %>% 
  mutate(Latitude = LOCATION_LAT,
         Longitude = LOCATION_LONG) 

ch4.sites <- sites[sites$SITE_ID %in%  unique(CH4.Flux.HH.units$SITE_ID) , ]

setdiff(sites$SITE_ID, unique(CH4.Flux.HH.units$SITE_ID))

# Check the sites:
length(unique(CH4.Flux.DD$SITE_ID))
length(unique(ch4.sites$SITE_ID))

ch4.sites.shp <- ch4.sites %>%  st_as_sf(coords = c('Longitude', 'Latitude'),
                                         crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>% 
  select( SITE_ID, SITE_NAME, FLUXNET.CH4, LOCATION_ELEV, IGBP, MAT,MAP,  geometry)

project.dir <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/CH4_Drought"
setwd( project.dir)

save(ch4.sites.shp, ch4.sites , CH4.Flux.HH.units, CH4.Flux.DD,
     file="Fluxnet_Data.RDATA")
