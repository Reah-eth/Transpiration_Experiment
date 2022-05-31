
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

######## here we plot the experimental design ###############

LF_400_split_plot <- read_xlsx("Out/LF400_split_plot.xlsx")

#### this creates an unique indentification code for each pot 
LF_400_split_plot$index <-   levels(as.factor(paste(row.names(LF_400_split_plot))))

##### there are a total of 1264 pots in this experiment each row can have only 10 plants, therefore we need 127 columns 
indexes <- data.frame(index=levels(as.factor(LF_400_split_plot$index)),
                      r=rep(c(rep(1,127),rep(2,127),rep(3,127),rep(4,127),
                              rep(5,127),rep(6,127),rep(7,127),rep(8,127),
                              rep(9,127),rep(10,127)),
                            length.out=1264),c=rep(1:127,length.out=1264))

designplt <- full_join(LF_400_split_plot,indexes)

d <- ggplot(designplt)
d <- d + geom_rect(aes(xmin=as.numeric(c)-.5,xmax=as.numeric(c)+.5,
                       ymin=as.numeric(r)-.5,ymax=as.numeric(r)+.5),
                   color="black", fill="white")
d <- d + geom_text(aes(x=as.numeric(c),y=as.numeric(r),label=trt2,angle = 90),
                   size=2,hjust = 0.25,nudge_y=-0.25)
d <- d + geom_text(aes(x=as.numeric(c),y=as.numeric(r),label=code,angle = 90),
                   nudge_x=.25,size=1.9,hjust = 0.25,nudge_y=-0.25)
#d <- d + facet_wrap(~block,ncol = 2)
d <- d + labs(title = "Transpiration Experiment June 2022 [Split plot] ",y="Rows",x="Columns")
d <- d + theme_set(theme_cowplot())
d

ggsave(d, filename = "Out/Experimental_design_Jun2022.png", 
       bg = "transparent", width = 98,height = 28,units = "cm")
