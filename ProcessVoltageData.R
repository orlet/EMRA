dir.create("voltagePlots", showWarnings = FALSE)

library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(dplyr)
theme_set(theme_few(base_size = 16))


### for quickly generating plots of:
### 1) voltage and current information and 2) conductivity data
### across the whole length of a run. 
files <- list.files(pattern = ".trc", recursive = FALSE)
lapply(files, function(i){

  dat <- read.delim(i, sep = "\t", header = FALSE, skip = 1)
  names(dat)[c(1:3)] <- c("timesec", "voltage", "current") 
  dat <- mutate(dat, time= timesec/60) 
  dat <- mutate(dat, conductivity = current / voltage)
  
  dat.melt1 <- melt(dat, measure.vars = c("voltage", "current"), id.vars = c("time"))
  voltCurrent <- ggplot(dat.melt1, aes(x = time, y = value, color = variable)) + geom_line(size=1.5) +
    theme_few(base_size = 16) + xlab(label = "Time (min)")
  ggsave(voltCurrent, filename = paste0("voltagePlots/",i,".png"), width = 10, height = 6)
  
  conductivityPlot <- ggplot(dat, aes(time, conductivity )) + geom_line(size = 1.2) + theme_few(base_size = 16) + 
    labs( x = "Time (min)", 
          y = expression(paste("Conductivity (",mu,"S)")))
  ggsave(conductivityPlot, filename = paste0("voltagePlots/",i,"conductivityPlot.png"), width = 10, height = 6)
  
})


### for generating plot with all voltage and all current in a single folder
### use to evaluate consistency of current profiles in a group of experiments

files <- list.files(pattern = ".trc", recursive = FALSE)
hvsdata <- lapply(files, function(i){
  dat <- read.delim(i, sep = "\t", header = FALSE, skip = 5)
  dat$Experiment <- as.character(i)
  hvsdata1 <- rbind(dat)
  hvsdata1 <- mutate(hvsdata1, time = (V1/60))
  hvsdata1
}) %>% bind_rows()

allCurrent <- ggplot(hvsdata, aes(x = time, y = V3, color = Experiment)) +
  geom_line(size = 1.2) + theme_few(base_size = 16) + scale_color_brewer(palette = "Set1") +
  labs( x = "Time (min)", 
        y = expression(paste("Current (",mu,"A)"))) #+ xlim(0, 20) #+ ylim(-5,70)
allCurrent
ggsave(allCurrent, filename= paste0("voltagePlots/allCurrent.png"), width = 10, height = 6)

allVoltage <- ggplot(hvsdata, aes(x = time, y = V2, color = Experiment)) +
  geom_line(size = 1.2) + theme_few(base_size = 16) + scale_color_brewer(palette = "Set1") +
  labs( x = "Time (min)", 
        y = "Voltage (V)") #+ xlim(0, 20) #+ ylim(-5,70)
allVoltage
ggsave(allVoltage, filename= paste0("voltagePlots/allVoltage.png"), width = 10, height = 6)


### Get conductivity while adjusting to the injection time point 
startTime <- 17.5
nameFile <- ("HVSMeas171311-1152.trc")
title <- unlist(strsplit(nameFile, split = "\\."))[1]
dat <- read.delim(nameFile, sep = "\t", header = FALSE, skip = 1)
dat <- mutate(dat, time= (V1/60)) 
dat <- mutate(dat, conductivity = (V3/V2)) 
dat.1 <- subset(dat, time > startTime)
dat.1 <- mutate(dat.1, time = time - time[1])

conductivityPlot <- ggplot(dat.1, aes(time, conductivity )) + geom_line(size = 1.2) + theme_few(base_size = 16) + 
  labs( x = "Time (min)", 
        y = expression(paste("Conductivity (",mu,"S)"))) #+ ylim(0.2, 0.6)
conductivityPlot
ggsave(conductivityPlot, filename = paste0("voltagePlots/",title,"start",startTime,".png"), width = 5, height = 3)


