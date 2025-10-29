library(tidyverse)
library(randomForest)
library(GGally)

rm(list=ls())

load( file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data.RDATA')

fluxes.drought_normalized %>% names
 
# Random Forest Model Development:  ####

Normalizex.spei48.model.rf <- randomForest::randomForest(normalized_Fch4 ~ 
                                         SPEI48 + ELEV + DI.SPI48.MeanDuration
                                         + month + VPD_F+TA_F ,
                                         data= fluxes.drought_normalized,
                                         importance=TRUE )

Normalizex.spei48.model.rf$importance %>% as.data.frame() %>% mutate(vars = rownames(Normalizex.spei48.model.rf$importance)) %>% 
  ggplot( aes( x=reorder(vars, IncNodePurity), y= IncNodePurity) )+ 
            geom_point() + geom_segment( aes(x=reorder(vars, IncNodePurity), xend=reorder(vars, IncNodePurity), y=0, yend=IncNodePurity), color="grey") + coord_flip() + theme_bw() +xlab('Variable Names')


save( Normalizex.spei48.model.rf, fluxes.drought_normalized,
      file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

# get the list of variables used in the model:

model <-Normalizex.spei48.model.rf
dataframe <- fluxes.drought_normalized
y <- 'normalized_Fch4'
factors <- 'month'

sensitivity.df <- function( model, dataframe, y, factors){
  
  library(gtools)
  # get the variables
  vars <- model$importance %>%  as.data.frame() %>% row.names() 
  vars.no.factors <-vars[vars != factors]
  
  
  sub.set.mean <- dataframe %>% select( vars.no.factors) %>% summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
 
 sensitividy.df <- data.frame()
 for( i in vars.no.factors){
   print(i)
   new.sub <-  dataframe %>% select( i) 
   new.sub[i] %>% min
   new.sub[i] %>% max %>% round(1)/10
   seq(new.sub[i] %>% min, new.sub[i] %>% max , new.sub[i] %>% max %>% round(1)/10 )
   
   target <- data.frame(data = seq(new.sub[i] %>% min, new.sub[i] %>% max , new.sub[i] %>% max %>% round(1)/10 ) )
   
   names(target) <- i
   
   new.sub.set.mean <- sub.set.mean %>% select(- i) %>% cross_join(target ) %>% mutate(target = i)
   
   sensitividy.df <- smartbind( sensitividy.df, new.sub.set.mean )
   
 }
 
 # Deal with factors:
 sensitividy.df.final <- sensitividy.df
 for( f in factors){
   
   factor.sub <- data.frame( data =dataframe[,f] %>% unique)
   names(factor.sub) <- f
   sensitividy.df.final <- sensitividy.df.final %>% cross_join(factor.sub)
 }
 
 sensitividy.df.final$predictions <- predict(model, newdata=sensitividy.df.final )
 
 return( sensitividy.df.final )
}

Normalizex.spei48.model.rf.SA.DF <- sensitivity.df(
  model =Normalizex.spei48.model.rf,
  dataframe = fluxes.drought_normalized,
  y ='normalized_Fch4',
  factors = 'month')

save( Normalizex.spei48.model.rf, fluxes.drought_normalized,Normalizex.spei48.model.rf.SA.DF, 
      file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')


summary(Normalizex.spei48.model.rf.SA.DF)

Normalizex.spei48.model.rf.SA.DF %>% names()

Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_boxplot(aes(x= SPEI48, y =predictions, col=month ))


month.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_boxplot(aes(x= month, y =predictions)) + geom_hline( yintercept = 0, col="red")

SPEI.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_smooth(aes(x= SPEI48, y =predictions, col=month)) + geom_hline( yintercept = 0, col="red")

elevation.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'ELEV')%>% ggplot( ) + geom_smooth(aes(x= ELEV, y =predictions), col="black")

duration.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'DI.SPI48.MeanDuration') %>% ggplot( ) + geom_smooth(aes(x= DI.SPI48.MeanDuration, y =predictions), col="black")

TA.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'TA_F') %>% ggplot( ) + geom_smooth(aes(x= TA_F, y =predictions), col="black")

VPD.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'VPD_F') %>% ggplot( ) + geom_smooth(aes(x= VPD_F, y =predictions), col="black")


library(ggpubr)
sensitivity.plots <- ggarrange(elevation.plot,
          duration.plot,
          month.plot,
          SPEI.plot,
          TA.plot,
          VPD.plot, 
          nrow=2, ncol=3)






Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_line(aes(x= SPEI48, y =predictions, col=month ))

Normalizex.spei48.model.rf.SA.DF  %>% ggplot( ) + geom_smooth(aes(x= TA_F, y =predictions) )

Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48')  %>% ggplot( ) + geom_smooth(aes(x= SPEI48, y =predictions) )
Normalizex.spei48.model.rf.SA.DF$NEE_F_ANNOPTLM

Normalizex.spei48.model.rf.SA.DF %>% ggplot( ) + geom_point(aes(x= VPD_F, y =predictions, col=month) )
