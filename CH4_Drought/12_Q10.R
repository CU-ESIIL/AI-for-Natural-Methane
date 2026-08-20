# Temperature Sensitivity:
library(tidyverse)
library(broom)

# Resolve the folder holding config.R robustly (see 05_CompileData.R).
.a <- commandArgs(FALSE); .f <- .a[grepl("^--file=", .a)]
.cand <- if (length(.f)) dirname(normalizePath(sub("^--file=", "", .f[1]), mustWork = FALSE)) else character(0)
.cand <- c(.cand, getwd(), file.path(getwd(), "CH4_Drought"),
           "/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought")
.hit <- .cand[file.exists(file.path(.cand, "config.R"))]
if (!length(.hit)) stop("Could not find config.R. setwd() to the CH4_Drought folder (or its parent) and rerun.")
analysis_dir <- .hit[1]
source(file.path(analysis_dir, "config.R"))
source(file.path(analysis_dir, "temperature_index.R"))

project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/"
setwd( project.data.dir)

load( file='FinalDrought_Data.RDATA')
load(file="Fluxnet_Data.RDATA")
# CH4.Flux.HH.units

# Fit temperature response curve: ####
source( '/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/calc_Q10.R')

q10_cache_file <- "/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/Fluxnet_Q10_YearMon.RDATA"

if (!file.exists(q10_cache_file)) {
  FLUXNET_TRC_PARMS_04_YearMon <- data.frame()
  FLUXNET_TRC_PARMS_05_YearMon <- data.frame()

  for ( site in CH4.Flux.HH.units$SITE_ID %>% unique( ) ) {
    
    print(site)
   
    FLUXNET_TRC_PARMS_04 <- TRC_PARMS_04(data.frame = CH4.Flux.HH.units %>% filter( SITE_ID == site),
                                         iterations = 5000,
                                         priors.trc = brms::prior("normal(2.0, 0.3)", nlpar = "Q10", lb = 1.0, ub = 5) +
                                           brms::prior("normal(0.5, 0.3)", nlpar = "Rref", lb = 0.001, ub = 5),
                                         idx.colname = 'YearMon',
                                         NEE.colname = 'FCH4_F_ANNOPTLM',
                                         TA.colname = 'TA_F',
                                         Tref = 1)  %>%  mutate( SITE_ID = site)
    
    FLUXNET_TRC_PARMS_04_YearMon <- rbind(   FLUXNET_TRC_PARMS_04_YearMon ,
                                             FLUXNET_TRC_PARMS_04)
    
    FLUXNET_TRC_PARMS_05 <- TRC_PARMS_05(data.frame = CH4.Flux.HH.units %>% filter( SITE_ID == site),
                                         iterations = 5000,
                                         priors.trc = brms::prior("normal(0.2 , 1)", nlpar = "a", lb = 0.1, ub = 1) +
                                           brms::prior("normal(0.5, 0.03)", nlpar = "b", lb = 0.001, ub = 0.9),
                                         idx.colname = 'YearMon',
                                         NEE.colname = 'FCH4_F_ANNOPTLM',
                                         TA.colname = 'TA_F') %>%  mutate( SITE_ID = site)
    
    FLUXNET_TRC_PARMS_05_YearMon <- rbind(   FLUXNET_TRC_PARMS_05_YearMon ,
                                             FLUXNET_TRC_PARMS_05)
  }

  FLUXNET_TRC_PARMS_05_YearMon <- FLUXNET_TRC_PARMS_05_YearMon %>% separate(idx, into = c("month", "year"), sep = "-")

  FLUXNET_TRC_PARMS_04_YearMon <- FLUXNET_TRC_PARMS_04_YearMon %>% separate(idx, into = c("month", "year"), sep = "-")

  save(FLUXNET_TRC_PARMS_04_YearMon, FLUXNET_TRC_PARMS_05_YearMon, 
       file = q10_cache_file)
} else {
  message("Using cached Q10 parameters: ", q10_cache_file)
}

# Plot Data: ####
load(file = q10_cache_file)

library(tidyverse)
FLUXNET_TRC_PARMS_04_YearMon %>% ggplot() + geom_boxplot(aes( x= Q10.mean, y= SITE_ID)) 


FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= Q10, y= SITE_ID, col=month))
FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= b.mean, y= SITE_ID))

FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= b.mean, y= Q10))
FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= a.mean, y= Q10))


#  SPEI ANALYSIS: ####

load(file = q10_cache_file)

load( file= '/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

site_list_file <- file.path(project.data.dir, "SiteList.RDATA")
if (!file.exists(site_list_file)) {
  site_list_file <- file.path(analysis_dir, "SiteList.RDATA")
}
if (file.exists(site_list_file)) {
  load(file = site_list_file)
} else {
  all.sites <- fluxes.drought_normalized %>% select(SITE_ID) %>% distinct()
  study.sites.igbp <- all.sites$SITE_ID
}
# Combine the data and compare the Q10 for different SPEI within and between ecosystems:

FLUXNET_TRC_PARMS_04_YearMon <- FLUXNET_TRC_PARMS_04_YearMon %>% mutate( YearMon = paste( year, month, sep="-"), SITE_ID = case_when(SITE_ID =="US-Pfa" ~ "US-PFa",.default =  SITE_ID)) %>% filter( SITE_ID %in% all.sites$SITE_ID)

FLUXNET_Q10 <- FLUXNET_TRC_PARMS_04_YearMon %>% select(-c(month)) %>% right_join( { .dq <- fluxes.drought_normalized %>% select(-any_of("geometry")); if (!"thermal_season" %in% names(.dq)) .dq <- add_thermal_season(.dq); if (!"temp_class" %in% names(.dq)) .dq <- add_temp_anomaly_class(.dq); .dq }, by=c('SITE_ID', 'YearMon'))%>% filter( SITE_ID %in% all.sites$SITE_ID)

FLUXNET_Q10_normal <- FLUXNET_Q10 %>%  select(SITE_ID, Q10.mean, all_of(DROUGHT_INDEX), month, Rref.mean ) %>% 
  mutate(normal = case_when(.data[[DROUGHT_INDEX]] < NORMAL_UPPER &
                              .data[[DROUGHT_INDEX]] > NORMAL_LOWER ~ 1)) %>% 
  filter ( normal == 1) %>% reframe( .by= c(SITE_ID, month), Q10.normal = mean(Q10.mean, na.rm=T), Rref.normal = mean(Rref.mean, na.rm=T)) %>% filter( SITE_ID %in% all.sites$SITE_ID)


FLUXNET_Q10_normalized <- FLUXNET_Q10 %>% full_join(FLUXNET_Q10_normal, by=c("SITE_ID","month")) %>% mutate(Q10.normalized = Q10.mean- Q10.normal,
                                                                                                            Rref.normalized = Rref.mean- Rref.normal) %>% filter( SITE_ID %in% all.sites$SITE_ID)

# ---------------------------------------------------------------------------
# Thermal-season view of temperature sensitivity: is Q10 (and its response to
# the SPEI wet-dry gradient) different for winter vs summer events?
# ---------------------------------------------------------------------------
q10_by_season <- FLUXNET_Q10_normalized %>%
  filter(!is.na(thermal_season)) %>%
  group_by(thermal_season) %>%
  summarise(
    n_obs     = dplyr::n(),
    Q10_mean  = mean(Q10.mean, na.rm = TRUE),
    Q10_sd    = stats::sd(Q10.mean, na.rm = TRUE),
    Rref_mean = mean(Rref.mean, na.rm = TRUE),
    Rref_sd   = stats::sd(Rref.mean, na.rm = TRUE),
    .groups   = "drop"
  )

q10_spei_slope_by_season <- FLUXNET_Q10_normalized %>%
  filter(!is.na(thermal_season), !is.na(.data[[DROUGHT_INDEX]]), !is.na(Q10.normalized)) %>%
  group_by(thermal_season) %>%
  group_modify(~ broom::tidy(lm(as.formula(paste("Q10.normalized ~", DROUGHT_INDEX)), data = .x))) %>%
  ungroup()

q10_events_dir <- file.path(analysis_dir, "outputs", "temperature_events")
dir.create(q10_events_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(q10_by_season,            file.path(q10_events_dir, "q10_by_season.csv"))
readr::write_csv(q10_spei_slope_by_season, file.path(q10_events_dir, "q10_spei_slope_by_season.csv"))

Q10.season.figure <- FLUXNET_Q10_normalized %>%
  filter(!is.na(thermal_season)) %>%
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized, color = thermal_season)) +
  geom_smooth(method = "lm") + theme_bw() + xlim(-4, 4) +
  geom_vline(xintercept = DROUGHT_THRESHOLD, linetype = "dashed") +
  geom_vline(xintercept = WET_THRESHOLD, linetype = "dashed") +
  scale_color_manual(values = c(winter = "#2166ac", shoulder = "grey60", summer = "#b2182b")) +
  labs(x = DROUGHT_INDEX, y = "Normalized Q10", color = "Thermal season")
ggsave(Q10.season.figure, filename = file.path(q10_events_dir, "q10_spei_by_season.png"),
       width = 5, height = 4)

# Temperature-anomaly view: is temperature sensitivity different for hot vs
# normal vs cold events (hotter/colder than normal for the site and month)?
q10_by_temp_anomaly <- FLUXNET_Q10_normalized %>%
  filter(!is.na(temp_class)) %>%
  group_by(temp_class) %>%
  summarise(
    n_obs     = dplyr::n(),
    Q10_mean  = mean(Q10.mean, na.rm = TRUE),
    Q10_sd    = stats::sd(Q10.mean, na.rm = TRUE),
    Rref_mean = mean(Rref.mean, na.rm = TRUE),
    Rref_sd   = stats::sd(Rref.mean, na.rm = TRUE),
    .groups   = "drop"
  )

anom_events_dir <- file.path(analysis_dir, "outputs", "temperature_anomaly_events")
dir.create(anom_events_dir, recursive = TRUE, showWarnings = FALSE)
readr::write_csv(q10_by_temp_anomaly, file.path(anom_events_dir, "q10_by_temp_anomaly.csv"))

Q10.temp.figure <- FLUXNET_Q10_normalized %>%
  filter(!is.na(temp_class)) %>%
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized, color = temp_class)) +
  geom_smooth(method = "lm") + theme_bw() + xlim(-4, 4) +
  scale_color_manual(values = c(cold = "#2166ac", normal = "grey60", hot = "#b2182b")) +
  labs(x = DROUGHT_INDEX, y = "Normalized Q10", color = "Temp anomaly")
ggsave(Q10.temp.figure, filename = file.path(anom_events_dir, "q10_spei_by_temp_anomaly.png"),
       width = 5, height = 4)

FLUXNET_Q10_normalized %>% ggplot() + geom_point(aes(x=Q10.mean, y = Rref.mean))

library(ggplot2)
library(ggpubr)

Q10.plot.Linear.spei <- FLUXNET_Q10_normalized %>% ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized)) + 
  geom_point(col="black", alpha=0.006) + geom_smooth(method="lm", col="darkseagreen") + theme_bw() + xlim(-4, 4) +
  stat_regline_equation(label.x = 0, label.y = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 3.5) +
  ylab("Normalized Q10") + xlab(DROUGHT_INDEX)


Rref.plot.Linear.spei <- FLUXNET_Q10_normalized %>% ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Rref.normalized)) + 
  geom_point(col="black", alpha=0.006) + geom_smooth(method="lm", col="darkseagreen") + theme_bw() + xlim(-4, 4) +
  stat_regline_equation(label.x = 0, label.y = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 3.5) +
  ylab("Normalized Rref") + xlab(DROUGHT_INDEX)


ggsave(Q10.plot.Linear.spei , filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/12_Q10_Linear_SPEI_FIGURE.png", width = 4, height = 3)

# SITE LevelLinear Analysis:

Q10.linear.results <- data.frame()
Rref.linear.results <- data.frame()

for( site in FLUXNET_Q10_normalized$SITE_ID %>% unique){
  
  print(site)
  subset <- FLUXNET_Q10_normalized %>% filter( SITE_ID == site)
  
  try(lm.spei <- lm(as.formula(paste("Q10.normalized ~", DROUGHT_INDEX)), data = subset) %>% summary, silent=T)
  
  try(lm.spei.results <- data.frame(    Q10.Intercept =lm.spei$coefficients[1] %>% round(3), # intercept
                                        Q10.Intercept.pvalue = lm.spei$coefficients[7] %>% round(3), # intercept - Pvalue
                                        Q10.slope = lm.spei$coefficients[2] %>% round(3), # slope
                                        Q10.slope.pvalue=lm.spei$coefficients[8] %>% round(3), # slope- pvalue
                                        Q10.R2 =lm.spei$r.squared %>% round(3),
                                        mean.spei = subset[[DROUGHT_INDEX]] %>% mean(na.rm=T) %>% round(3),
                                        min.spei = subset[[DROUGHT_INDEX]] %>% min(na.rm=T) %>% round(3),
                                        max.spei = subset[[DROUGHT_INDEX]] %>% max(na.rm=T) %>% round(3),
                                        var.spei = subset[[DROUGHT_INDEX]] %>% var(na.rm=T) %>% round(3),
                                        SITE_ID = site), silent = T)
  


    try( Q10.linear.results <- rbind(Q10.linear.results, lm.spei.results), silent=T)

  
  try(rm( lm.spei, lm.spei.results), silent =T)
  
  # Rref:
  
  try(lm.spei.Rref <- lm(as.formula(paste("Rref.normalized ~", DROUGHT_INDEX)), data = subset) %>% summary, silent=T)
  
  try(lm.spei.Rref.results <- data.frame( Rref.Intercept =lm.spei.Rref$coefficients[1] %>% round(3), # intercept
                                          Rref.Intercept.pvalue = lm.spei.Rref$coefficients[7] %>% round(3), # intercept - Pvalue
                                          Rref.slope = lm.spei.Rref$coefficients[2] %>% round(3), # slope
                                          Rref.slope.pvalue=lm.spei.Rref$coefficients[8] %>% round(3), # slope- pvalue
                                          Rref.R2 =lm.spei.Rref$r.squared %>% round(3),
                                        SITE_ID = site), silent = T)
  
  
  
  try( Rref.linear.results <- rbind(Rref.linear.results, lm.spei.Rref.results), silent=T)
  
  
  try(rm( lm.spei.Rref, lm.spei.Rref.results), silent =T)
}


ggplot(data=Q10.linear.results ,aes( x= Q10.slope, y = SITE_ID)) + 
  geom_point(col="black") 

FLUXNET_Q10_normalized %>% ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized)) + geom_smooth(method="lm", col="darkseagreen") + theme_bw() + xlim(-4, 4) + ylab("Normalized Q10") + xlab(DROUGHT_INDEX) + 
  facet_wrap(~SITE_ID) + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed")



FLUXNET_Q10_normalized_Linear <- FLUXNET_Q10_normalized %>%  left_join(Q10.linear.results, by='SITE_ID')%>%  left_join(Rref.linear.results, by='SITE_ID')

Q10.linear.plot.neg <- FLUXNET_Q10_normalized_Linear %>% 
  filter( Q10.slope > 0) %>%  
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized,col=SITE_ID)) + 
  geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylim(-4, 3) + 
  ylab("Normalized Q10") + xlab(DROUGHT_INDEX)  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed") +  
  theme(text = element_text(size = 20)) + scale_color_viridis_d(option = "rocket")

Q10.linear.plot.pos <-FLUXNET_Q10_normalized_Linear %>% 
  filter( Q10.slope < 0) %>%  
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Q10.normalized,col=SITE_ID)) + ylim(-4, 3) + 
  geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylab("Normalized Q10") + xlab(DROUGHT_INDEX)  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed")+  
  theme(text = element_text(size = 20))+ scale_color_viridis_d(option = "mako")




Rref.linear.plot.pos <-FLUXNET_Q10_normalized_Linear %>%
  filter( Rref.slope < 0) %>% 
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Rref.normalized,col=SITE_ID)) + 
  geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylim(-4, 3) + 
  ylab("Normalized Rref") + xlab(DROUGHT_INDEX)  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed") +  
  theme(text = element_text(size = 20)) + scale_color_viridis_d(option = "mako")


Rref.linear.plot.neg <-FLUXNET_Q10_normalized_Linear %>%
  filter( Rref.slope > 0) %>% 
  ggplot(aes(x = .data[[DROUGHT_INDEX]], y = Rref.normalized,col=SITE_ID)) + 
  geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylim(-4, 3) + 
  ylab("Normalized Rref") + xlab(DROUGHT_INDEX)  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed") +  
  theme(text = element_text(size = 20)) + scale_color_viridis_d(option = "rocket")



Q10.linear.sites.plot<- ggarrange( Q10.linear.plot.neg, Q10.linear.plot.pos, 
                                   labels=c('A', 'B'), ncol=1)


Rref.linear.sites.plot <- ggarrange(Rref.linear.plot.neg, Rref.linear.plot.pos,
                                   labels=c('A', 'B'), ncol=1)

ggsave(Q10.linear.sites.plot , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/12_Q10_Linear_SPEI_SITES_FIGURE.png", width = 8, height = 10)
ggsave(Rref.linear.sites.plot , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/04_Rref_Linear_SPEI_SITES_FIGURE.png", width = 8, height = 10)


# Create a Map of the sites showing the slope:
library(sf)
# {AOI} dropped: it imports `%>%` from {sf}, which recent sf no longer re-exports,
# so `library(AOI)` errors. Basemap now read from the local Natural Earth shapefile.

site.Q10.Slope <- FLUXNET_Q10_normalized_Linear %>% reframe( .by=SITE_ID, Q10.slope)

Q10.linear.results.shp <- Q10.linear.results %>% left_join(ch4.sites, by='SITE_ID') %>% st_as_sf( coords = c("LOCATION_LONG" , "LOCATION_LAT"), crs = 4326) %>% mutate(Response =case_when(Q10.slope <= 0 ~ 'Dampened', .default='Enhanced') %>% as.factor) %>% filter(SITE_ID %in% all.sites$SITE_ID)

aoi.terrestrial <- sf::st_read(file.path(analysis_dir, CONTINENT_SHAPEFILE), quiet = TRUE)


library(RColorBrewer)
Q10.slope.map <- ggplot() + geom_sf( data=aoi.terrestrial, fill="white", col="grey60")+ 
  geom_sf( data= Q10.linear.results.shp, aes( size= abs(Q10.slope), col= Response), alpha =0.4) + labs(size = "Q10 Slope") + theme_minimal() + scale_color_manual(values = c("brown", "cyan3")) 



ggsave(Q10.slope.map , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/Map_FIGURE_Enhanced.png", width = 8, height = 3)


site.spei.plot <- FLUXNET_Q10_normalized_Linear  %>% mutate(Response =case_when(Q10.slope <= 0 ~ 'Dampened', .default='Enhanced') %>% as.factor) %>% ggplot()+
  geom_boxplot((aes(y=SITE_ID, x = .data[[DROUGHT_INDEX]], col=Response)), alpha=0.5) + scale_color_manual(values = c("brown", "cyan3")) + geom_vline(xintercept = DROUGHT_THRESHOLD, linetype='dashed') + geom_vline(xintercept = WET_THRESHOLD, linetype='dashed') + theme_bw() + ylab("") + xlab(DROUGHT_INDEX) + xlim(-4, 4)

ggsave(site.spei.plot , 
       filename="/Users/sm3466/Library/CloudStorage/Dropbox-YSE/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/FIGURES/SPEI_SITE_Enhanced.png", width = 4, height = 10)
