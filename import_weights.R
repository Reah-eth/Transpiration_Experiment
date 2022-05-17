.libPaths("T:/R4Userlibs")
library(dplyr)
library(tidyverse)
library(stringi)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(gganimate)
library(gifski)
library(readxl)

setwd("C:/Users/rgonzales/Desktop/transpiration_experiment 2022")

my_path <- setwd("C:/Users/rgonzales/Desktop/transpiration_experiment 2022/Data")

my_dir <- list.files(path = my_path, recursive = T,
                     pattern = "*2022*", full.names = T)

imp_data <- vector("list", length(my_dir))

for (i in seq_along(my_dir)) {
  file_info <- unlist(str_split(my_dir[i], pattern = "//"))
  recording_day <- substr(file_info, 63 , nchar(file_info)-4)
  
  
  imp_data[[i]] <- suppressWarnings(read.table(my_dir[i], header = FALSE, sep = "",
                                          colClasses = c("character","numeric"),
                                          col.names=c("Genotype","Weight")))
  imp_data[[i]] <- imp_data[[i]] %>% 
      mutate(Genotype = substr(Genotype, 1, nchar(Genotype)-30))%>%   
      mutate(recording_day = recording_day)%>%
      distinct(Genotype, Weight, recording_day, .keep_all= TRUE)
 }

imp_data <- bind_rows(imp_data)

imp_data$recording_day <-  as.Date(imp_data$recording_day)


imp_data <- imp_data %>% 
    arrange(Genotype) 


############## Import the design file ##################

Exp_design <- read_excel("LF400_extreems_split plot.xlsx")
Exp_design <- Exp_design%>%mutate(Genotype = str_replace(Genotype, " ", "_"))

############ Merge data files with experimental design ###########

imp_data1 <- merge(imp_data,Exp_design, by = "Genotype")

############### filter the data ###################################
imp_data2 <- imp_data1%>%
  filter(Weight > 0)%>%
  arrange(Genotype,recording_day)%>% 
  group_by(Genotype)%>%
  mutate(TR = Weight - dplyr::lag(Weight, n = 1, default = NA))%>%
  mutate(TR  = abs(TR))

imp_data2 <- imp_data2%>%
  arrange(Genotype,recording_day)%>%
  group_by(Genotype)%>% 
  mutate(cum_H20_loss = cumsum(replace_na(TR, 0)))
