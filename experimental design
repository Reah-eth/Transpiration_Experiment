
#####################################################################################################
### Title: Experimental design and analysis for Transpiration experiment (Split plot)
### Author: Reah Gonzales
### Date: 19.04.2022
### Reah.Gonzales@usys.ethz.ch
#####################################################################################################
setwd("C:/Users/rgonzales/Desktop/transpiration_experiment 2022")
.libPaths("C:/Users/rgonzales/R3Userlibs")
library(agricolae)
library(writexl)
library(readxl)
library(dplyr)
library(stringr)

data <- read_excel("Data/LF400_genotype_list.xlsx")

trt <- data$Genotype
trt2 <- c("Control", "Drought")
outdesign <-design.split(trt,trt2,r=2,serie=2,seed=44564,
                         randomization=TRUE,
                         first=TRUE,
                         kinds ="Super-Duper",design = 'rcbd')#seed=44564
book<-outdesign$book

#### We need to have sub plot and block as numeric in oder to create the barcode. 

book$splots <- as.numeric(book$splots)
book$block <- as.numeric(book$block)

book$code <- ifelse(book$block == 1,str_c(book$trt ,"_",book$splots),str_c(book$trt ,"_",book$splots+book$block)) 


write_xlsx(book,"Out/LF400_split_plot.xlsx")

### reoder

barcode <-  book[order(book$code),]
write_xlsx(barcode,"Out/LF400_split_plot_barcodes.xlsx")

#### Plot the final design 

book$idx <-  str_c(substr(book$plots,2,3),book$splots)

indexes <- data.frame(idx=levels(as.factor(book$idx)),
                      r=rep(c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                              2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                              4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                              5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                              6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                              7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                              8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                              9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                              10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10),
                            length.out=200),c=rep(1:20,length.out=200))

designplt <- full_join(book,indexes)

d <- ggplot(designplt)
d <- d + geom_rect(aes(xmin=as.numeric(c)-.5,xmax=as.numeric(c)+.5,
                       ymin=as.numeric(r)-.5,ymax=as.numeric(r)+.5),
                   color="black", fill="white")
d <- d + geom_text(aes(x=as.numeric(c),y=as.numeric(r),label=trt2,angle = 90),
                   size=2,hjust = 0.25,nudge_y=-0.25)
d <- d + geom_text(aes(x=as.numeric(c),y=as.numeric(r),label=code,angle = 90),
                   nudge_x=.25,size=1.9,hjust = 0.25,nudge_y=-0.25)
#d <- d + facet_wrap(~block,ncol = 2)
d <- d + labs(title = "Transpiration Experiment Janurary 2022 [Split plot] ",y="Rows",x="Columns")
d <- d + theme_set(theme_cowplot())
d
