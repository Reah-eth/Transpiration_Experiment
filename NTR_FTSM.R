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

######## Computing NTR and FTSW ####################
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

Weight_fun <- corrected_TR
GEN <- unique(corrected_TR$Genotype)
GFSW_con <- data.frame()
GFSW_con <- getftsw_dr(GEN[1])
for (i in 2:length(GEN)){GFSW_con <- rbind(GFSW_con ,getftsw_dr(GEN[i]))}

##### make a cool publication plot ##### 
##### look at the the FTSW FOR ALL DROUGHTED PLANTS ##### 

p <- ggplot(GFSW_dro%>%filter(daily_FTSW > 0),
            aes(recording_day,daily_FTSW)) +
  geom_point(aes(shape=trt),size=7) + 
  ylim(0,1)+
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  labs(x = "Date", y = "FTSW")+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=30))
p 
ggsave(p, 
       filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/sub_set_FTSW.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")


p <- ggplot(GFSW_dro%>%filter(daily_FTSW > 0),
            aes(recording_day,daily_FTSW)) +
  geom_point(aes(colour=trt),size=2.5) + 
  ylim(0,1)+
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  labs(x = "Date", y = "FTSW")+
  facet_wrap(.~trt, scales="free")
p 

ggsave(p, 
         filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/facet_FTSW.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")


################ merge the FTSW and TR data sets ###################

#### 1st get a mean FTSW and TSW values for each genotype 
#### this only makes sense for the droughted pots 

Drought_mean_FTSW <- GFSW_dro%>%group_by(recording_day,trt)%>%
  arrange(recording_day,trt)%>%
  mutate(mean_FTSW = mean(daily_FTSW))%>%
  mutate(mean_TSW = mean(TSW))%>%
  select(trt,recording_day,mean_TSW,mean_FTSW)%>%
  distinct(.keep_all= TRUE)%>%
  ungroup

###### merge the data sets 

TR_FTSW <- merge(Drought_mean_FTSW,TR_CD)

########## make a plot to look at the data ##############

p <- ggplot(TR_FTSW %>%filter(mean_TR_drought < 50),
            aes(mean_FTSW,mean_TR_drought)) +
  geom_point(size=1.5) + 
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  labs(x = "Date", y = "weight [g]")

p 

######## now calculate the normalized transpiration rate #############

####### this is a normalization function 

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
 
getNTR <- function(GEN){
  data <- TR_NTR[TR_NTR$trt==GEN,]
  print(dim(data))
  data$NTR <- log10(data$rel_TR)
  plot(data$mean_FTSW,data$NTR)
  return(data)
}

1.05/(1+2.5*exp(-9*(data$mean_FTSW)))

TR_NTR <- TR_FTSW
TR_NTR$rel_TR <- TR_NTR$mean_TR_drought/TR_NTR$mean_TR_con
GEN <- unique(TR_FTSW$trt)
NTR <- data.frame()
NTR <- getNTR(GEN[1])
for (i in 2:length(GEN)){NTR <- rbind(NTR ,getNTR(GEN[i]))}

##### plot all of the data 

NTR$NTR <- 1.05/(1+2.5*exp(-9*(NTR$mean_FTSW)))

p <- ggplot(NTR%>%filter(NTR < 1 ),
            aes(mean_FTSW,NTR)) +
  xlim(1,0)+
  geom_point(aes(shape=trt),size=7) + 
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="TOP")+
  labs(x = "FTSW", y = "NTR [log10]")+
  theme(axis.text=element_text(size=24),
        axis.title=element_text(size=30))
p
ggsave(p, 
       filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/sub_set_NTR_FTSW.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")


p <- ggplot(NTR%>%filter(NTR < 1 ),
            aes(mean_FTSW,NTR)) +
  xlim(1,0)+
  geom_point(aes(colour=trt),size=2.5) + 
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  labs(x = "FTSW", y = "NTR [log10]")+
  facet_wrap(.~trt, scales="free")
p 

ggsave(p, 
       filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/facet_NTR_FTSW.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")

