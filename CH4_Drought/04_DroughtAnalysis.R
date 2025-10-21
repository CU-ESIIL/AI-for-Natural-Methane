# 4_Analysis

load( file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')

fluxes.drought <-Drought.DF # the dataframe to use:

drought.test.kruskal <- data.frame()

for( i in fluxes.drought$SITE_ID %>% unique){
  print(i)
  subset <- fluxes.drought %>% filter( SITE_ID == i)
  test <- data.frame(SITE_ID = i )
  
  if( length(subset$drought %>% unique) > 1){
    try( kt <- kruskal.test(data = subset ,FCH4_F_ANNOPTLM ~ drought  ), silent =T)
    try( pw <- pairwise.wilcox.test(subset$FCH4_F_ANNOPTLM, subset$drought), silent =T)
    try(  test$Kpvalue <- kt$p.value, silent =T)
    try(  means <- reframe( subset, .by=drought, mean=mean(FCH4_F_ANNOPTLM, na.rm=T), SE= sd(FCH4_F_ANNOPTLM, na.rm=T)/length(FCH4_F_ANNOPTLM)), silent =T)
    try( test$drought.mean <- mean(subset$FCH4_F_ANNOPTLM[subset$drought == "Drought"], na.rm=T), silent =T)
    try(  test$drought.sd <- sd(subset$FCH4_F_ANNOPTLM[subset$drought == "Drought"], na.rm=T), silent =T)
    try( test$normal.mean <- mean(subset$FCH4_F_ANNOPTLM[subset$drought != "Drought"], na.rm=T), silent =T)
    try( test$normal.sd <- sd(subset$FCH4_F_ANNOPTLM[subset$drought != "Drought"], na.rm=T), silent =T)
    try( drought.test.kruskal <- rbind(drought.test.kruskal, test), silent =T)
    try(rm( test, kt, pw), silent =T)
  }
  
}

drought.test.kruskal <- drought.test.kruskal %>% mutate( delta = ((drought.mean-normal.mean)/normal.mean)*100)

drought.test.kruskal$delta %>% hist()




save( fluxes.drought, file= '~/YSE Dropbox/Sparkle Malone/Research/CH4_Drought/data/Drought_Analysis.RDATA' )
save( drought.test.kruskal, file= '~/YSE Dropbox/Sparkle Malone/Research/CH4_Drought/data/Results_kruskal.RDATA' )


fluxes.drought %>% ggplot(aes( SPEI1)) + geom_density() + theme_bw() + xlab('SPEI')


drought.data <- fluxes.drought %>% filter (SPEI1 <= -1) %>% na.omit()
drought.data$SITE_ID %>% unique() %>% length

drought.test.kruskal2 <- drought.test.kruskal %>% left_join(fluxes.drought %>% filter( drought == "Drought") %>% reframe(.by=SITE_ID, SPEI1.Drought.mean = mean(SPEI1) %>% round(3), 
                                                                                                                         SPEI1.Drought.max = min(SPEI1) %>% round(3),
                                                                                                                         Month=month %>% as.numeric), by = 'SITE_ID') 
fluxes.drought.SPEI <- fluxes.drought %>% mutate(SPEI.r = SPEI1 %>% round(1)) %>% reframe( .by=c(SITE_ID,SPEI.r),  FCH4_F_ANNOPTLM = mean(FCH4_F_ANNOPTLM, na.rm=T))


# Add the month of the drought.                         
fluxes.drought.SPEI%>% ggplot(aes( y=FCH4_F_ANNOPTLM , x= SPEI.r)) + geom_point() + geom_smooth(method="lm") + ylim(-200, 200) 

# Normalize the Sites by values near 0.

fluxes.drought_normalized_data_zscore <- fluxes.drought %>%
  group_by(SITE_ID) %>%
  mutate(normalized_Fch4_zscore = scale(FCH4_F_ANNOPTLM)) %>%
  ungroup()

drought.test.lm.SPEI <- data.frame()

for( i in unique(fluxes.drought_normalized_data_zscore$SITE_ID )){
  
  print(i)
  try(subset <- fluxes.drought_normalized_data_zscore %>% filter( SITE_ID == i), silent=T)
  try(lm.model <- lm(subset$FCH4_F_ANNOPTLM ~ subset$SPEI1) %>% summary, silent=T)
  try(results <- data.frame(SITE_ID = i ), silent=T)
  try(results$slope <- lm.model$coefficients[2]  , silent=T) # Slope
  try(results$r2 <-lm.model$r.squared %>% round(3) , silent=T)
  try(results$pvalue <-lm.model$coefficients[2,4] %>% round(2) , silent=T)
  
  try(drought.test.lm.SPEI <- rbind(drought.test.lm.SPEI , results) , silent=T)
  
  rm( results, lm.model)
  
}

drought.test.lm.SPEI %>%  ggplot(aes( slope)) + geom_density()
save( drought.test.lm.SPEI, file= '~/YSE Dropbox/Sparkle Malone/Research/CH4_Drought/data/Results_LM_SPEI.RDATA' )

fluxes.drought %>% names()