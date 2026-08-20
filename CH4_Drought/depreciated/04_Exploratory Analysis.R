# Exploratory Analysis:

# 4_Analysis

library(tidyverse)
library(GGally)

config_file <- file.path(getwd(), "CH4_Drought", "config.R")
if (!file.exists(config_file)) config_file <- "config.R"
source(config_file)

project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data"
setwd( project.data.dir)

load( file='FinalDrought_Data.RDATA')

if (exists("fluxes.drought") && "geometry" %in% names(fluxes.drought)) {
  fluxes.drought$geometry <- NULL
}
if (exists("Drought.DF.final") && "geometry" %in% names(Drought.DF.final)) {
  Drought.DF.final$geometry <- NULL
}

if (!exists("fluxes.drought")) {
  fluxes.drought <- Drought.DF.final %>% filter(!is.na(FCH4_F_ANNOPTLM))
} else {
  fluxes.drought <- fluxes.drought %>% filter(!is.na(FCH4_F_ANNOPTLM))
}

fluxes.drought_reordered <- fluxes.drought %>%
  group_by(SITE_ID) %>%
  mutate(order_metric = mean(NEE_F_ANNOPTLM)) %>% # Calculate mean of order_by_column for each category
  ungroup() %>%
  mutate(SITE_ID = fct_reorder(SITE_ID, order_metric))


fluxes.drought_reordered %>% ggplot(aes(y = SITE_ID, x = .data[[DROUGHT_INDEX]])) + geom_boxplot() + geom_vline(xintercept = DROUGHT_THRESHOLD, color = "maroon",) +
  geom_vline(xintercept = WET_THRESHOLD, color = "springgreen3",) +
  theme(text = element_text(size = 5),
        axis.text.x = element_text(angle = 90, hjust = 1)) + theme_bw() + ylab('FLUXNET Site') + 
  xlab('SPEI')

names(fluxes.drought)
theme_bw()

# kruskal ####
drought.kruskal.results <- data.frame()

# Performs Kruskal test for differences between fluxes under drought and non-drought conditions:
for( site in fluxes.drought$SITE_ID %>% unique){
  print(site)
  subset <- fluxes.drought %>% filter( SITE_ID == site)
  
  drought.indx <- DROUGHT_INDEX
  
  for( i in 1:length(drought.indx)){
    print(drought.indx[i])
    
    subset.drought <- subset %>%
      mutate(drought = case_when(.data[[drought.indx[i]]] <= DROUGHT_THRESHOLD ~ "Drought",
                                 .data[[drought.indx[i]]] > DROUGHT_THRESHOLD ~ "No Drought"))
    
    
    
    if( length(subset.drought$drought %>% unique) > 1){
      try( kt <- kruskal.test(data = subset.drought ,FCH4_F_ANNOPTLM ~ drought  ), silent =T)
      try( pw <- pairwise.wilcox.test(subset.drought$FCH4_F_ANNOPTLM, subset.drought$drought), silent =T)
      try(  kruskal.results[,'Kpvalue'] <- kt$p.value, silent =T)
      try(  means <- reframe( subset.drought, .by=drought, mean=mean(FCH4_F_ANNOPTLM, na.rm=T), SE= sd(FCH4_F_ANNOPTLM, na.rm=T)/length(FCH4_F_ANNOPTLM)), silent =T)
      
      kruskal.results <- data.frame(Kpvalue = kt$p.value,
                                    drought.mean = mean(subset.drought$FCH4_F_ANNOPTLM[subset.drought$drought == "Drought"], na.rm=T),
                                    drought.sd = sd(subset.drought$FCH4_F_ANNOPTLM[subset.drought$drought == "Drought"], na.rm=T), 
                                    normal.mean = mean(subset.drought$FCH4_F_ANNOPTLM[subset.drought$drought != "Drought"], na.rm=T),
                                    normal.sd = sd(subset.drought$FCH4_F_ANNOPTLM[subset.drought$drought != "Drought"], na.rm=T),
                                    Drought.IDX = drought.indx[i], SITE_ID = site)
      
      try( drought.kruskal.results <- rbind(drought.kruskal.results, kruskal.results), silent =T)
      try(rm( kruskal.results, kt, pw), silent =T)
    }
    
  }
  
}

drought.kruskal.results <- drought.kruskal.results %>% mutate( delta = ((normal.mean-drought.mean)/normal.mean*100))

drought.kruskal.results$delta %>% summary

drought.kruskal.results %>% ggplot( aes( x=delta, col=Drought.IDX)) + geom_density() + xlim( -100, 100) +  geom_vline(xintercept = 0, color = "red",)

drought.kruskal.results %>% ggplot( aes( x=delta, col=Drought.IDX)) + geom_density() + xlim( -100, 100) +  geom_vline(xintercept = 0, color = "red",)

drought.kruskal.results$drought.mean
drought.kruskal.results %>% filter(Kpvalue <= 0.05 ) %>% ggplot( ) + geom_boxplot(aes(y = Drought.IDX, x= drought.mean), col='maroon') +
  geom_boxplot(aes(y = Drought.IDX, x= normal.mean), col='springgreen', fill='transparent') +
  xlim( -100, 100)

drought.kruskal.results %>% names

save( fluxes.drought, file= 'Drought_Analysis.RDATA' )
save( drought.kruskal.results, file= 'Results_kruskal.RDATA' )


fluxes.drought %>% ggplot(aes(.data[[DROUGHT_INDEX]])) + geom_density() + theme_bw() + xlab(DROUGHT_INDEX)

drought.data <- fluxes.drought %>% filter(.data[[DROUGHT_INDEX]] <= DROUGHT_THRESHOLD) %>% na.omit()
drought.data$SITE_ID %>% unique() %>% length

drought.test.kruskal2 <- drought.kruskal.results %>%
  left_join(fluxes.drought %>%
              filter(drought == "Drought") %>%
              reframe(.by = SITE_ID,
                      SPEI.Drought.mean = mean(.data[[DROUGHT_INDEX]]) %>% round(3),
                      SPEI.Drought.max = min(.data[[DROUGHT_INDEX]]) %>% round(3),
                      Month = month %>% as.numeric), by = 'SITE_ID')
fluxes.drought.SPEI <- fluxes.drought %>%
  mutate(SPEI.r = .data[[DROUGHT_INDEX]] %>% round(1)) %>%
  reframe(.by = c(SITE_ID, SPEI.r), FCH4_F_ANNOPTLM = mean(FCH4_F_ANNOPTLM, na.rm=T))


# Add the month of the drought.                         
fluxes.drought.SPEI%>% ggplot(aes( y=FCH4_F_ANNOPTLM , x= SPEI.r)) + geom_point() + geom_smooth(method="lm") + ylim(-200, 200) 

# Change the order for the factor for plotting:



# Normalize the Sites by values near 0.

fluxes.drought_normalized_data_zscore <- fluxes.drought %>%
  group_by(SITE_ID) %>%
  mutate(normalized_Fch4_zscore = scale(FCH4_F_ANNOPTLM)) %>%
  ungroup() %>% na.omit 

fluxes.drought_normalized_data_zscore.df <- as.data.frame(fluxes.drought_normalized_data_zscore)

drought.test.lm.SPEI <- data.frame()

for( site in unique(fluxes.drought_normalized_data_zscore.df$SITE_ID )){
  print(site)
  subset <- fluxes.drought_normalized_data_zscore.df %>% filter( SITE_ID == site)
  
  drought.indx <- DROUGHT_INDEX
  
  if( length(  subset$SITE_ID) > 1){
    for( i in 1:length(drought.indx)){
      print(drought.indx[i])
      
      try({flux = subset[,'normalized_Fch4_zscore'] 
      index = subset[[drought.indx[i]]]
      lm.model = lm(  flux  ~  index) %>% summary
      results = data.frame(SITE_ID = site )
      results$slope = lm.model$coefficients[2]
      results$r2 =lm.model$r.squared %>% round(3) 
      results$pvalue =lm.model$coefficients[2,4] %>% round(2) 
      results$Drought.IDX = drought.indx[i]
      drought.test.lm.SPEI = rbind( drought.test.lm.SPEI , results)
      rm( results, lm.model)
      })
      
      
    }}
}


drought.test.lm.SPEI %>%  ggplot(aes( slope)) + geom_density(aes( col =Drought.IDX )) 


drought.test.lm.SPEI %>%  ggplot(aes(y= slope, x= Drought.IDX)) + geom_boxplot() 
project.data.dir <-"/Volumes/MaloneLab/Research/Natural_CH4_CO2/data"
setwd( project.data.dir)
save( drought.test.lm.SPEI, file= 'Results_LM_SPEI.RDATA' )
