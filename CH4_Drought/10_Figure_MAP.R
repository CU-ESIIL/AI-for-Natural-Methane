library(tidyverse)
library(sf)
# NOTE: {AOI} is no longer used for the basemap. AOI imports `%>%` from {sf}, which
# recent sf versions no longer re-export, so `library(AOI)` fails ("object '%>%' is
# not exported by 'namespace:sf'"). The basemap is read from the local Natural Earth
# shapefile (config CONTINENT_SHAPEFILE) instead, which is also more reproducible.

# Resolve the folder holding config.R robustly (see 05_CompileData.R).
.a <- commandArgs(FALSE); .f <- .a[grepl("^--file=", .a)]
.cand <- if (length(.f)) dirname(normalizePath(sub("^--file=", "", .f[1]), mustWork = FALSE)) else character(0)
.cand <- c(.cand, getwd(), file.path(getwd(), "CH4_Drought"),
           "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
.hit <- .cand[file.exists(file.path(.cand, "config.R"))]
if (!length(.hit)) stop("Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
analysis_dir <- .hit[1]
source(file.path(analysis_dir, "config.R"))

#remotes::install_github("mikejohnson51/AOI", quite=F)

project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
setwd( project.data.dir)

load(file="Fluxnet_Data.RDATA")
load( file='FinalDrought_Data.RDATA')

fluxes.drought <-Drought.DF %>% filter( !is.na(FCH4_F_ANNOPTLM)) # the dataframe to use:

rm.INGBP <- EXCLUDE_IGBP # centralized in config.R (CRO/URB/SNO/WAT) so 05/06/10 match

sites.igbp <- ch4.sites.shp %>% filter(!IGBP %in% rm.INGBP, !SITE_ID %in% EXCLUDE_SITES)
study.sites.igbp <-sites.igbp$SITE_ID 


Dry.sites <- fluxes.drought %>% filter(.data[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD & SITE_ID %in%  study.sites.igbp) %>% select(SITE_ID) %>% distinct()

Normal.sites <- fluxes.drought %>% filter(.data[[DROUGHT_INDEX]] > DROUGHT_THRESHOLD & .data[[DROUGHT_INDEX]] < WET_THRESHOLD & SITE_ID %in%  study.sites.igbp) %>% select(SITE_ID) %>% distinct()

Wet.sites <- fluxes.drought %>% filter(.data[[DROUGHT_INDEX]] >= WET_THRESHOLD & SITE_ID %in%  study.sites.igbp) %>% select(SITE_ID) %>% distinct()


all.sites <- fluxes.drought %>% filter(  SITE_ID %in%  study.sites.igbp) %>% select(SITE_ID) %>% distinct()

extreme.events <- rbind(Wet.sites, Dry.sites) %>% distinct

aoi.terrestrial <- sf::st_read(file.path(analysis_dir, CONTINENT_SHAPEFILE), quiet = TRUE)


all.normal.map <- ggplot() + geom_sf( data=aoi.terrestrial, fill="white", col="grey60")+ 
  geom_sf( data= ch4.sites.shp %>% filter(SITE_ID %in% extreme.events$SITE_ID), size=0.5)+
  theme_bw()

all.wet.map <-ggplot() + geom_sf( data=aoi.terrestrial, fill="white", col="grey60")+ 
  geom_sf( data= ch4.sites.shp %>% filter(SITE_ID %in% Wet.sites$SITE_ID), size=0.5, col='blue')+
  theme_bw()


all.dry.map <-ggplot() + geom_sf( data=aoi.terrestrial, fill="white", col="grey60")+ 
  geom_sf( data= ch4.sites.shp %>% filter(SITE_ID %in% Dry.sites$SITE_ID), size=0.5, col='red')+
  theme_bw()


ExtremesMap <- ggarrange( all.normal.map, 
                         all.dry.map, labels=c('A', 'B'), ncol=1)

ggsave(ExtremesMap , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/Map_FIGURE.png", width = 5, height = 6)


# SPEI Range for Site:


project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data"
setwd( project.data.dir)

load( file='FinalDrought_Data.RDATA')


fluxes.drought <-Drought.DF %>% filter( !is.na(FCH4_F_ANNOPTLM)) # the dataframe to use:

fluxes.drought_reordered <- fluxes.drought %>%
  group_by(SITE_ID) %>%
  mutate(order_metric = mean(NEE_F_ANNOPTLM)) %>% # Calculate mean of order_by_column for each category
  ungroup() %>%
  mutate(SITE_ID = fct_reorder(SITE_ID, order_metric))


spei.range.plot <- fluxes.drought_reordered %>% filter( SITE_ID %in%  study.sites.igbp) %>%  ggplot(aes(y = SITE_ID, x = .data[[DROUGHT_INDEX]])) + geom_boxplot() + geom_vline(xintercept = DROUGHT_THRESHOLD, color = "red",) + xlim(-3, 3)+
  geom_vline(xintercept = WET_THRESHOLD, color = "blue",) +
  theme(text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw() + ylab('Site') + 
  xlab('SPEI')




spei.range.plot

SPEI <- ggarrange( spei.range.plot, labels=c('B'))

ggsave(SPEI , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/SPEI_FIGURE.png", width = 5, height = 10)

save(all.sites, study.sites.igbp, file="SiteList.RDATA")
