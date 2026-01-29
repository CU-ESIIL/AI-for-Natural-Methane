# Temperature Sensitivity:

load( file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')

load(file="/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/Fluxnet_Data.RDATA")
# CH4.Flux.HH.units

# Fit temperature response curve: ####
load( '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/calc_Q10.R')

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
     file="/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/Fluxnet_Q10_YearMon.RDATA")

# Plot Data: ####
load(file="/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/Fluxnet_Q10_YearMon.RDATA")

library(tidyverse)
FLUXNET_TRC_PARMS_04_YearMon %>% ggplot() + geom_boxplot(aes( x= Q10.mean, y= SITE_ID)) 


FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= Q10, y= SITE_ID, col=month))
FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= b.mean, y= SITE_ID))

FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= b.mean, y= Q10))
FLUXNET_TRC_PARMS_05_YearMon %>% ggplot() + geom_point(aes( x= a.mean, y= Q10))


#  SPEI ANALYSIS: ####

load(file="/Volumes/MaloneLab/Research/Natural_CH4_CO2/data/Fluxnet_Q10_YearMon.RDATA")

load( file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

# Combine the data and compare the Q10 for different SPEI within and between ecosystems:

FLUXNET_TRC_PARMS_04_YearMon <- FLUXNET_TRC_PARMS_04_YearMon %>% mutate( YearMon = paste( year, month, sep="-"), SITE_ID = case_when(SITE_ID =="US-Pfa" ~ "US-PFa",.default =  SITE_ID))

FLUXNET_Q10 <- FLUXNET_TRC_PARMS_04_YearMon %>% select(-c(month)) %>% right_join( Drought.DF.final, by=c('SITE_ID', 'YearMon'))

FLUXNET_Q10_normal <- FLUXNET_Q10 %>%  select(SITE_ID, Q10.mean, SPEI48, month ) %>% mutate(normal = case_when( SPEI48 < 0.5 & SPEI48 > -0.5 ~ 1)) %>% filter ( normal == 1) %>% reframe( .by= c(SITE_ID, month), Q10.normal = mean(Q10.mean, na.rm=T))


FLUXNET_Q10_normalized <- FLUXNET_Q10 %>% full_join(FLUXNET_Q10_normal, by=c("SITE_ID","month")) %>% mutate(Q10.normalized = Q10.mean- Q10.normal)



Q10.plot.Linear.spei <- FLUXNET_Q10_normalized %>% ggplot(aes( x= SPEI48, y = Q10.normalized)) + 
  geom_point(col="black", alpha=0.006) + geom_smooth(method="lm", col="darkseagreen") + theme_bw() + xlim(-4, 4) +
  stat_regline_equation(label.x = 0, label.y = 4, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 3.5) +
  ylab("Normalized Q10") + xlab("SPEI (48 months)")


ggsave(Q10.plot.Linear.spei , filename="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_DROUGHT/FIGURES/04_Q10_Linear_SPEI_FIGURE.png", width = 4, height = 3)

# SITE LevelLinear Analysis:

Q10.linear.results <- data.frame()

for( site in FLUXNET_Q10_normalized$SITE_ID %>% unique){
  
  print(site)
  subset <- FLUXNET_Q10_normalized %>% filter( SITE_ID == site)
  
  try(lm.spei <- lm(data = subset ,Q10.normalized ~ SPEI48  ) %>% summary, silent=T)
  
  try(lm.spei.results <- data.frame(    Intercept =lm.spei$coefficients[1] %>% round(3), # intercept
                                        Intercept.pvalue = lm.spei$coefficients[7] %>% round(3), # intercept - Pvalue
                                        slope = lm.spei$coefficients[2] %>% round(3), # slope
                                        slope.pvalue=lm.spei$coefficients[8] %>% round(3), # slope- pvalue
                                        R2 =lm.spei$r.squared %>% round(3),
                                        mean.spei48 = subset$SPEI48 %>% mean(na.rm=T) %>% round(3),
                                        min.spei48 = subset$SPEI48 %>% min(na.rm=T) %>% round(3),
                                        max.spei48 = subset$SPEI48 %>% max(na.rm=T) %>% round(3),
                                        var.spei48 = subset$SPEI48 %>% var(na.rm=T) %>% round(3),
                                        SITE_ID = site), silent = T)
  


    try( Q10.linear.results <- rbind(Q10.linear.results, lm.spei.results), silent=T)

  
  try(rm( lm.spei, lm.spei.results), silent =T)
}


Q10.linear.results <- Q10.linear.results %>% as.data.frame()

ggplot(data=Q10.linear.results ,aes( x= slope, y = SITE_ID)) + 
  geom_point(col="black") 

FLUXNET_Q10_normalized %>% ggplot(aes( x= SPEI48, y = Q10.normalized)) + geom_smooth(method="lm", col="darkseagreen") + theme_bw() + xlim(-4, 4) + ylab("Normalized Q10") + xlab("SPEI (48 months)") + 
  facet_wrap(~SITE_ID) + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed")

Q10.linear.results %>% names

FLUXNET_Q10_normalized_Linear <- FLUXNET_Q10_normalized %>%  left_join(Q10.linear.results, by='SITE_ID')

Q10.linear.plot.neg <- FLUXNET_Q10_normalized_Linear %>% filter( slope > 0) %>%  ggplot(aes( x= SPEI48, y = Q10.normalized,col=SITE_ID)) + geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylab("Normalized Q10") + xlab("SPEI (48 months)")  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed")

Q10.linear.plot.pos <-FLUXNET_Q10_normalized_Linear %>% filter( slope < 0) %>%  ggplot(aes( x= SPEI48, y = Q10.normalized,col=SITE_ID)) + geom_smooth(method="lm") + theme_bw() + xlim(-4, 4) + ylab("Normalized Q10") + xlab("SPEI (48 months)")  + 
  geom_vline(xintercept = -1.5,linetype = "dashed")+ 
  geom_vline(xintercept = 1.5,linetype = "dashed")



Q10.linear.sites.plot<- ggarrange( Q10.linear.plot.neg,
           Q10.linear.plot.pos, labels=c('A', 'B'), ncol=1)

ggsave(Q10.linear.sites.plot , filename="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_DROUGHT/FIGURES/04_Q10_Linear_SPEI_SITES_FIGURE.png", width = 8, height = 10)
