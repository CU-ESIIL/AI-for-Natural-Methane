library(tidyverse)
library(sf)


# Compile the data:
project.dir <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data"

setwd( project.dir)
load(file="Fluxnet_Data.RDATA")

# Imports the site information
fluxnet <- ch4.sites.shp %>% 
  filter (FLUXNET.CH4 == "CC-BY-4.0" ) %>% st_transform(crs='+proj=longlat +datum=WGS84 +no_defs' )

# Topography
Topo.df <- read.csv( '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/Fluxnet_Topography.csv')

# EVI
evi.df <- read.csv( '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/fluxes_drought_with_EVI.csv')

full.df <- Topo.df %>% full_join(evi.df, by="SITE_ID")

# Events:
events <- c(0, 1, 1, 1, 0, 0, 1, 1, 0, 1)
rle(events)$values[ rle(events)$values == 1] %>% sum # number of events
rle(events)$length[ rle(events)$values == 1]  # duration of each event

Drought.DF <- data.frame()
drought.indx <- names(full.df)[21:34]

# Drought definition:  Should run by site!

for ( site in full.df$SITE_ID %>% unique){
  
  full.df.sub <- full.df %>% filter( SITE_ID ==  site)
  for ( i in 1:length( drought.indx)){
    
    print(paste('working on', drought.indx[i], sep= " ") )
    
    # Define the Drought conditions and makes an indicator:  
    
    events <- case_when(full.df.sub [, drought.indx[i]] < -1~1 , full.df.sub[, drought.indx[i]] > -1~0)
    
    lenght.timeseries.months <- events %>% length
    freq <- rle(events)$values[ rle(events)$values == 1] %>% sum 
    duration <- rle(events)$length[ rle(events)$values == 1] %>% mean
    duration.nodrought <- rle(events)$length[ rle(events)$values == 0] %>% mean
    
    # Add to the file
    full.df.sub [,  paste('DI.', drought.indx[i], sep="")] <- events
    
    # Indicator for the number of events:
    full.df.sub [,  paste('DI.', drought.indx[i], sep="")] <- events 
    full.df.sub [,  paste('DI.', drought.indx[i],".PercentTime", sep="")] <- freq / lenght.timeseries.months
    full.df.sub [,  paste('DI.', drought.indx[i],".MeanDuration", sep="")] <- duration
    full.df.sub [,  paste('DI.', drought.indx[i],".MeanTimeBetween", sep="")] <-duration.nodrought
  }
  
  Drought.DF <- rbind( Drought.DF, full.df.sub)
}


names(Drought.DF)

load( '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')

Drought.DF.final <- fluxnet %>% as.data.frame() %>%  full_join (Drought.DF, by = 'SITE_ID')

# Data Prep: #####
fluxes.drought <- Drought.DF.final %>% filter( !is.na(FCH4_F_ANNOPTLM)) %>% mutate( month = as.factor(month)) 

fluxes.drought.normal <- fluxes.drought %>%  select(SITE_ID, FCH4_F_ANNOPTLM, SPEI48 ) %>% mutate(normal = case_when( SPEI48 < 0.5 & SPEI48 > -0.5 ~ 1)) %>% filter ( normal == 1) %>% reframe( .by= SITE_ID, FCH4.normal = mean(FCH4_F_ANNOPTLM, na.rm=T))

fluxes.drought_normalized <- fluxes.drought %>% left_join(fluxes.drought.normal, by = join_by(SITE_ID) ) %>% mutate(normalized_Fch4 = FCH4_F_ANNOPTLM -  FCH4.normal) %>% filter( IGBP != "CRO") %>% na.omit

save( Drought.DF.final,fluxes.drought , fluxes.drought_normalized,  file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')


