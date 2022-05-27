#####################################################################################
###### Title: Combine leaf area and weight measurements
###### Description: Combine and analyze the leaf area and weight data from the transpiration experiment
###### Author: Reah Gonzales
###### Date: 19:05:2022
### Reah.Gonzales@usys.ethz.ch
###########################################################################################


#### Libraries ####
.libPaths("T:/R4Userlibs")
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(readxl)

setwd("C:/Users/rgonzales/Desktop/transpiration_experiment 2022") 

####### Import the data sets 
Weight <- read.table("C:/Users/rgonzales/Desktop/transpiration_experiment 2022/Out/Weight.txt",
                     header = T, sep = "\t")

Weight$recording_day <-  as.Date(Weight$recording_day)

###### Ensure that the TR for the re-water period is not calculated 

corrected_TR <- Weight%>%filter(trt2=="Control")%>%
  group_by(recording_day,Genotype)%>%
  arrange(recording_day,Genotype)%>%
  mutate(TR2 = ifelse(TR==last(TR),0,TR))

######## obtain the mean transpiration rate for the control plants 

Control_mean_TR <- corrected_TR%>%group_by(recording_day,trt)%>%
  arrange(recording_day,trt)%>%
  mutate(mean_TR_con = mean(TR))%>%
  select(trt,recording_day,mean_TR_con)%>%
  distinct(.keep_all= TRUE)%>%
  ungroup()


####### Extract the mean transpiration rate for the Drought plants

Drought_mean_TR <- Weight%>%filter(trt2=="Drought")%>%
  group_by(recording_day,trt)%>%
  arrange(recording_day,trt)%>%
  mutate(mean_TR_drought = mean(TR))%>%
  select(trt,recording_day,mean_TR_drought)%>%
  distinct(.keep_all= TRUE)%>%
  ungroup


######### Merge the data sets #############

TR_CD <- merge(Control_mean_TR,Drought_mean_TR)

########## Look at a few plots ############

p <- ggplot(TR_CD %>%filter(trt=="Mara 31"),
            aes(recording_day,mean_TR_drought, color=trt)) +
  geom_point(size=1.5) + #ylim(0,30)+
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  #ggtitle("Barhoney 37") + 
  labs(x = "Date", y = "weight [g]")

p 


######## Computing Fraction of Transpired Soil Water (FTSW) ####################
### this only make sense for the droughted plants 
### control plants have a different final weight and to many negative values 
getftsw_dr <- function(GEN){
  data <- Weight_fun[Weight_fun$Genotype==GEN,]
  print(dim(data))
  data$final_weight <- last(data$Weight)
  data$initial_weight <- first(data$Weight)
  data$daily_FTSW <- (data$Weight-data$final_weight)/(data$initial_weight-data$final_weight)
  data$TSW <- data$initial_weight-data$final_weight
  plot(data$recording_day,data$daily_FTSW)
  return(data)
  }

Weight_fun <- Weight%>%filter(trt2=="Drought")
GEN <- unique(Weight_fun$Genotype)
GFSW_dro <- data.frame()
GFSW_dro <- getftsw_dr(GEN[1])
for (i in 2:length(GEN)){GFSW_dro <- rbind(GFSW_dro ,getftsw_dr(GEN[i]))}

################ merge the FTSW and Tanspiration Ratio data sets ###################

#### 1st get a mean FTSW and TSW values for each genotype 
#### this only makes sense for the droughted pots 

Drought_mean_FTSW <- GFSW_dro%>%group_by(recording_day,trt)%>%
  arrange(recording_day,trt)%>%
  mutate(mean_FTSW = mean(daily_FTSW))%>%
  mutate(mean_TSW = mean(TSW))%>%
  select(trt,recording_day,mean_TSW,mean_FTSW)%>%
  distinct(.keep_all= TRUE)%>%
  ungroup

###### merge the data sets ###############

TR_FTSW <- merge(Drought_mean_FTSW,TR_CD)

######## now calculate the normalized transpiration rate (NTR) #############
####### this is a normalization function, it also save the slope and plateu/lower range [b and e] 

getNTR <- function(GEN){
  data <- TR_NTR[TR_NTR$trt==GEN,]
  print(dim(data))
  fit <-  drm(rel_TR~mean_FTSW, data = data[data$rel_TR < 3,], 
              fct = LL.4(fixed = c(NA, 0, 2, NA)),
              control = drmc(errorm=FALSE))
  data$NTR <- predict(fit,data)
  plot(fit)
  plot(data$mean_FTSW,data$rel_TR)
  data$b <- fit$coefficients[1]
  data$e <- fit$coefficients[2]
  print(summary(fit))
  return(data)
}

TR_NTR <- TR_FTSW
TR_NTR$rel_TR <- TR_NTR$mean_TR_drought/TR_NTR$mean_TR_con
GEN <- unique(TR_FTSW$trt)
NTR <- data.frame()
NTR <- getNTR(GEN[1])
for (i in 2:length(GEN)){NTR <- rbind(NTR ,getNTR(GEN[i]))}

