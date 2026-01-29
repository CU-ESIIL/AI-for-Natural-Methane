# linear model

load( file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')
Drought.DF.final$geometry <- NULL # Remove the geometry for tidyverse...


fluxes.drought <- Drought.DF.final %>% filter( !is.na(FCH4_F_ANNOPTLM)) # the dataframe to use:

drought.linear.results <- data.frame()

for( site in fluxes.drought$SITE_ID %>% unique){

  print(site)
  subset <- fluxes.drought %>% filter( SITE_ID == site)
      
      lm.spei <- lm(data = subset ,FCH4_F_ANNOPTLM ~ SPEI48  ) %>% summary
   
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
      
    try( drought.linear.results <- rbind(drought.linear.results,lm.spei.results))
      try(rm( lm.spei), silent =T)
}

save(drought.linear.results,  file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data_LineaModel.RDATA')

load(file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data_LineaModel.RDATA')

drought.linear.results %>% names
drought.linear.results <-drought.linear.results %>% mutate(F.0 = Intercept + slope*0,
                                                           F.m3 = Intercept + slope*-3)

drought.linear.results %>% ggplot() +geom_segment(
             aes(x = F.0, y = SITE_ID,
                 yend = SITE_ID, xend = F.m3), #use the $ operator to fetch data from our "Females" tibble
             color = "#aeb6bf",
             size = 4.5, #Note that I sized the segment to fit the points
             alpha = .5) +
  geom_point(aes(x = F.0, y = SITE_ID), size = 4, show.legend = TRUE)+
  geom_point(aes(x = F.m3, y = SITE_ID),col="brown", size = 2, show.legend = TRUE) + xlim(-100, 100)


