# Compile the data:

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

save( Drought.DF, file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')
