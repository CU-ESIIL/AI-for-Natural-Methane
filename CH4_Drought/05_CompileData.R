library(tidyverse)
library(sf)

# Resolve the folder holding config.R robustly, independent of the (mutable) working
# directory: this script setwd()s to the lab server below, which can otherwise strand
# a later interactive run. Order: --file, getwd()[/CH4_Drought], then the project path.
.a <- commandArgs(FALSE); .f <- .a[grepl("^--file=", .a)]
.cand <- if (length(.f)) dirname(normalizePath(sub("^--file=", "", .f[1]), mustWork = FALSE)) else character(0)
.cand <- c(.cand, getwd(), file.path(getwd(), "CH4_Drought"),
           "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
.hit <- .cand[file.exists(file.path(.cand, "config.R"))]
if (!length(.hit)) stop("Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
analysis_dir <- .hit[1]
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "temperature_index.R"))

# Compile the data:
project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
setwd( project.data.dir)
load(file="Fluxnet_Data.RDATA")

# Imports the site information
fluxnet <- ch4.sites.shp %>% 
  filter (FLUXNET.CH4 == "CC-BY-4.0" ) %>% st_transform(crs='+proj=longlat +datum=WGS84 +no_defs' )

# SVWC:
svwc <- read.csv('TERRA_SWC_FLUXNETSITES.csv') %>% 
  mutate(SVWC = soil_total,
         Date = as.Date(date,format='%Y-%m-%d'),
         YearMon = format(Date, "%Y-%m")) %>% 
  select( SVWC, YearMon, SITE_ID)

# Topography
Topo.df <- read.csv( 'Fluxnet_Topography.csv')

# EVI
evi.df <- read.csv( 'fluxes_drought_with_EVI.csv') %>% mutate(Date =Date %>% as.Date(format="%m/%d/%y"),
                                                              YearMon = format(Date, "%Y-%m"))


full.df <- Topo.df %>% full_join(evi.df, by="SITE_ID")%>% left_join(svwc, by=c("SITE_ID", "YearMon"))

# Events:
events <- c(0, 1, 1, 1, 0, 0, 1, 1, 0, 1)
rle(events)$values[ rle(events)$values == 1] %>% sum # number of events
rle(events)$length[ rle(events)$values == 1]  # duration of each event

Drought.DF <- data.frame()
drought.indx <- DROUGHT_INDEX

# Drought definition:  Should run by site!

for ( site in full.df$SITE_ID %>% unique){
  
  full.df.sub <- full.df %>% filter( SITE_ID ==  site)
  for ( i in 1:length( drought.indx)){
    
    print(paste('working on', drought.indx[i], sep= " ") )
    
    # Define the Drought conditions and makes an indicator:  
    
    events <- case_when(full.df.sub[[drought.indx[i]]] < DROUGHT_THRESHOLD ~ 1,
                        full.df.sub[[drought.indx[i]]] > DROUGHT_THRESHOLD ~ 0)
    
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

#load( '/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')

Drought.DF.final <- fluxnet %>% as.data.frame() %>%  full_join (Drought.DF, by = 'SITE_ID')

# Drop the sf geometry (sfc) column so the saved tables are plain data.frames.
# Downstream scripts (06/07/11/12/21/...) do not all load {sf}, and a leftover
# sfc list-column breaks dplyr::filter() under recent dplyr/vctrs. Mapping uses
# ch4.sites.shp (from Fluxnet_Data.RDATA) instead, so geometry is not needed here.
Drought.DF.final[["geometry"]] <- NULL

# Data Prep: #####
fluxes.drought <- Drought.DF.final %>% filter( !is.na(FCH4_F_ANNOPTLM)) %>% mutate( month = as.factor(month))

# Add the thermal-season (winter/summer) axis, defined per site and independent
# of SPEI, so downstream steps can separate hot vs cold events from wet vs dry
# events. See temperature_index.R and config.R for the definition/thresholds.
fluxes.drought <- add_thermal_season(fluxes.drought)

fluxes.drought.normal <- fluxes.drought %>%
  select(SITE_ID, FCH4_F_ANNOPTLM, all_of(DROUGHT_INDEX)) %>%
  mutate(normal = case_when(.data[[DROUGHT_INDEX]] < NORMAL_UPPER &
                              .data[[DROUGHT_INDEX]] > NORMAL_LOWER ~ 1)) %>%
  filter(normal == 1) %>%
  reframe(.by = SITE_ID, FCH4.normal = mean(FCH4_F_ANNOPTLM, na.rm = TRUE))

fluxes.drought_normalized <- fluxes.drought %>% left_join(fluxes.drought.normal, by = join_by(SITE_ID) ) %>% mutate(normalized_Fch4 = FCH4_F_ANNOPTLM -  FCH4.normal) %>% filter( !IGBP %in% EXCLUDE_IGBP, !SITE_ID %in% EXCLUDE_SITES ) %>% na.omit

setwd( project.data.dir)
save( Drought.DF.final,fluxes.drought , fluxes.drought_normalized,  file='FinalDrought_Data.RDATA')
