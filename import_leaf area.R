###########################################################################################
### Title: Importing the leaf area data 
###         -- this data was extracted from the images using image analysis
### Author: Reah Gonzales
### Date: 16.05.2022
### Reah.Gonzales@usys.ethz.ch
###########################################################################################

####### Libraries 

.libPaths("T:/R4Userlibs")
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
#library(lubridate)
library(ggplot2)
library(cowplot)
library(gganimate)
library(gifski)
library(readxl)

#### this is the path to the .txt files  
my_path <- setwd("C:/Users/rgonzales/Desktop/transpiration_experiment 2022/Image_data")

my_dir <- list.files(path = my_path, recursive = T,
                     pattern = "*2022*", full.names = T)

imp_data <- vector("list", length(my_dir))

for (i in seq_along(my_dir)) {
  file_info <- unlist(str_split(my_dir[i], pattern = "//"))
  recording_day <- substr(file_info, 69 , nchar(file_info)-9)
  
  
  imp_data[[i]] <- read.table(my_dir[i], header = T, sep = "\t",
                                               colClasses = c("character","numeric"),
                                               col.names=c("Genotype","leaf_area"))
  imp_data[[i]] <- imp_data[[i]] %>% 
    mutate(Genotype = substr(Genotype, 1, nchar(Genotype)-26))%>%   
    mutate(recording_day = recording_day)%>%
    distinct(Genotype, leaf_area, recording_day, .keep_all= TRUE)
}

imp_data <- bind_rows(imp_data)



imp_data$recording_day <-  as.Date(imp_data$recording_day)


imp_data <- imp_data %>% 
  arrange(Genotype) 


############## Import the design file ##################

Exp_design <- read_excel("C:/Users/rgonzales/Desktop/transpiration_experiment 2022/Data/LF400_extreems_split plot.xlsx")
Exp_design <- Exp_design%>%mutate(Genotype = str_replace(Genotype, " ", "_"))

############ Merge data files with experimental design ###########

imp_data1 <- merge(imp_data,Exp_design, by = "Genotype")

############### filter the data ###################################
imp_data2 <- imp_data1%>%
  filter(leaf_area > 0)%>%
  arrange(Genotype,recording_day)%>% 
  group_by(Genotype)%>%
  mutate(LG = leaf_area - dplyr::lag(leaf_area, n = 1, default = NA))


imp_data2 <- imp_data2%>%
  arrange(Genotype,recording_day)%>%
  group_by(Genotype)%>% 
  mutate(cum_LG = cumsum(replace_na(LG, 0)))

test_plot <- ggplot(imp_data2%>%filter(trt2=="Drought" & Species==c("Festuca","Lolium"))) +
  geom_boxplot(aes(recording_day,leaf_area, colour=Species), size=4) +
  theme_cowplot()+ ggtitle("Top Camera Drought (Lolium vs Festuca)") +# ylim(0,100) +
  facet_wrap(.~recording_day, scales="free")
test_plot


ggsave(test_plot, 
       filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/transpiration_1.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")



p <- ggplot(imp_data2%>%filter(trt2=="Drought" ),
            aes(recording_day,leaf_area, color =Genotype)) +
  geom_line(size=1.5) + #ylim(0,30)+
  theme_cowplot()+
  scale_color_viridis_d() +
  theme(legend.position="none")+
  #ggtitle("Barhoney 37") + 
  labs(x = "Date", y = "area [cm sq]")

p 


ggsave(p, 
       filename = "C:/Users/rgonzales/Desktop/transpiration_experiment 2022/figures/Barhoney_37.png", 
       bg = "transparent", width = 48,height = 38,units = "cm")
