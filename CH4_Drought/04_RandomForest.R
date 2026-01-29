library(tidyverse)
library(randomForest)
library(GGally)
library(sf)

rm(list=ls())

load( file='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/FinalDrought_Data_10292025.RDATA')

fluxes.drought_normalized <-  fluxes.drought_normalized2
# Random Forest Model Fit: ####
set.seed(111) # set the randomnumber generator

#create ID column
fluxes.drought_normalized$id <- 1:nrow(fluxes.drought_normalized)

#use 90% of dataset as training set and 30% as test set 
train <- fluxes.drought_normalized %>% dplyr::sample_frac(0.80)
test  <- dplyr::anti_join(fluxes.drought_normalized, train, by = 'id')

# Random Forest Model Development:  ####

Normalizex.spei48.model.rf <- randomForest::randomForest(normalized_Fch4 ~ 
                                         SPEI48 + SLOPE + DI.SPI48.MeanDuration
                                         + month + VPD_F+TA_F + SVWC ,
                                         data= train,
                                         importance=TRUE )
Normalizex.spei48.model.rf$importance %>% as.data.frame() %>% names

# Variable Importance Plot



Imp.plot.1 <- Normalizex.spei48.model.rf$importance %>% as.data.frame() %>% 
  mutate(vars = rownames(Normalizex.spei48.model.rf$importance),
         vars2 = case_when(vars == "SPEI48" ~ 'SPEI',
                           vars == "SLOPE" ~ 'SLOPE',
                           vars == "DI.SPI48.MeanDuration" ~ 'Drought Duration',
                           vars == "month" ~ 'Month',
                           vars == "VPD_F" ~ 'VPD',
                           vars == "TA_F" ~ 'TA',
                           vars == "SVWC" ~ 'SVWC')) %>% 
  ggplot( aes( x=reorder(vars2, `%IncMSE`), y= `%IncMSE`) )+ 
  geom_point() + geom_segment( aes(x=reorder(vars2, `%IncMSE`), xend=reorder(vars2, `%IncMSE`), y=0, yend=`%IncMSE`), color="grey") + coord_flip() + theme_bw() +xlab('Variable Names')

Imp.plot.2 <- Normalizex.spei48.model.rf$importance %>% as.data.frame() %>% 
  mutate(vars = rownames(Normalizex.spei48.model.rf$importance),
         vars2 = case_when(vars == "SPEI48" ~ 'SPEI',
                           vars == "SLOPE" ~ 'SLOPE',
                           vars == "DI.SPI48.MeanDuration" ~ 'Drought Duration',
                           vars == "month" ~ 'Month',
                           vars == "VPD_F" ~ 'VPD',
                           vars == "TA_F" ~ 'TA',
                           vars == "SVWC" ~ 'SVWC')) %>% 
  ggplot( aes( x=reorder(vars2, IncNodePurity), y= IncNodePurity) )+ 
  geom_point() + geom_segment( aes(x=reorder(vars2, IncNodePurity), xend=reorder(vars2, IncNodePurity), y=0, yend=IncNodePurity), color="grey") + coord_flip() + theme_bw() +xlab('')

project.dir <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought"
setwd(project.dir)

png("FIGURES/04_VARIMP_FIGURE.png", width = 600, height = 300)
ggarrange(Imp.plot.1, Imp.plot.2, labels=c("A", "B") )
dev.off()


# Model Validation
train$PRED.48 <- predict(Normalizex.spei48.model.rf, train)
test$PRED.48 <- predict(Normalizex.spei48.model.rf, test)

# Model Testing:

train.plot <- ggplot( data = train, aes( x= normalized_Fch4, y= PRED.48 )) + geom_point(alpha=0.25) +
  geom_smooth(method='lm') + ylab("Predicted") + xlab('Observed')   + 
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") + theme_bw() +
  geom_smooth(method = "lm", se = FALSE, col="darkseagreen") +
  stat_regline_equation(label.x = 0, label.y = 1200, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 1000) # Add R-squared and p-value
  
test.plot <- ggplot( data = test, aes( x= normalized_Fch4, y= PRED.48 )) + geom_point(alpha=0.25) +
  geom_smooth(method='lm', col="darkseagreen") + ylab("Predicted") + xlab('Observed')   + 
  geom_abline(intercept = 0, slope = 1, color = "grey", linetype = "dashed") + theme_bw()  +
  stat_regline_equation(label.x = 0, label.y = 1200, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 0, label.y = 1000) # Add R-squared and p-value

ggsave(ggarrange(train.plot, test.plot, labels=c("A", "B") ), filename="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_DROUGHT/FIGURES/04_Model-Validation_FIGURE.png", width = 6, height = 3)

save( Normalizex.spei48.model.rf, fluxes.drought_normalized, test, train,
      file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

# get the list of variables used in the model:

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

Normalizex.spei48.model.rf.SA.DF <- sensitivity.df( model =Normalizex.spei48.model.rf,
                                                    dataframe = fluxes.drought_normalized,
                                                    y ='normalized_Fch4',
                                                    factors = 'month')
  

save( Normalizex.spei48.model.rf, fluxes.drought_normalized,Normalizex.spei48.model.rf.SA.DF, 
      file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

# Sensitivity Analysis: ####

load( file= '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_Drought/data/DroughtAnalysis.RDATA')

summary(Normalizex.spei48.model.rf.SA.DF)

Normalizex.spei48.model.rf.SA.DF %>% names()

month.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_boxplot(aes(x= month, y =predictions)) + geom_hline( yintercept = 0, col="red") + theme_bw() + ylab(expression(paste("F"[CH4]))) +
  xlab("Month")

SPEI.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_smooth(aes(x= SPEI48, y =predictions), col="black") + geom_hline( yintercept = 0, col="red")  + theme_bw() + ylab(expression(paste("F"[CH4])))  +
  xlab("SPEI48")

slope.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SLOPE')%>% ggplot( ) + geom_smooth(aes(x= SLOPE, y =predictions), col="black") + theme_bw()  + ylab(expression(paste("F"[CH4]))) +
  xlab("Slope")

duration.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'DI.SPI48.MeanDuration') %>% ggplot( ) + geom_smooth(aes(x= DI.SPI48.MeanDuration, y =predictions), col="black")+ theme_bw() + ylab(expression(paste("F"[CH4]))) +
  xlab("Mean Drought Duration")

TA.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'TA_F') %>% ggplot( ) + geom_smooth(aes(x= TA_F, y =predictions), col="black") + theme_bw()  + ylab(expression(paste("F"[CH4])))  +
  xlab("TA")

VPD.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'VPD_F') %>% ggplot( ) + geom_smooth(aes(x= VPD_F, y =predictions), col="black") + theme_bw()  + ylab(expression(paste("F"[CH4])))  +
  xlab("VPD")

SVWC.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SVWC') %>% ggplot( ) + geom_smooth(aes(x= SVWC, y =predictions), col="black") + theme_bw()  + ylab(expression(paste("F"[CH4])))  +
  xlab("SVWC")

SPEI.month.plot <- Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_smooth(aes(x= SPEI48, y =predictions), col="black") + geom_hline( yintercept = 0, col="red")  + theme_bw()  + ylab(expression(paste("F"[CH4])))  


library(ggpubr)
sensitivity.plots <- ggarrange(slope.plot,
          duration.plot,
          month.plot,
          SPEI.plot,
          TA.plot,
          VPD.plot,
          SVWC.plot, labels=c("A", "B","C", "D", "E", "F", "G"),
          nrow=3, ncol=3)

ggsave(sensitivity.plots ,  filename='/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/AI-for-Natural-Methane/CH4_DROUGHT/FIGURES/04_RF_SENSITIVITY.png',
      width=6, height=6 )
Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48') %>% ggplot( ) + geom_line(aes(x= SPEI48, y =predictions, col=month ))

Normalizex.spei48.model.rf.SA.DF  %>% ggplot( ) + geom_smooth(aes(x= TA_F, y =predictions) )

Normalizex.spei48.model.rf.SA.DF %>% filter(target == 'SPEI48')  %>% ggplot( ) + geom_smooth(aes(x= SPEI48, y =predictions) )


Normalizex.spei48.model.rf.SA.DF %>% ggplot( ) + geom_point(aes(x= VPD_F, y =predictions, col=month) )

