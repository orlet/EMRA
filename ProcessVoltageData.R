files <- list.files(pattern = ".trc", recursive = FALSE)
lapply(files, function(i){
  library(tidyverse)
  library(reshape2)
  library(ggthemes)
  dat <- read.delim(i, sep = "\t", header = FALSE, skip = 10)
  names(dat)[c(1:3)] <- c("timesec", "voltage", "current") 
  dat <- mutate(dat, time= timesec/60)
  

  dat <- mutate(dat, resistance = voltage / current)
  dat <- mutate(dat, conductivity = current / voltage)
  
  
  dat.melt <- melt(dat, measure.vars = c("voltage", "current"), id.vars = c("time"))
  voltCurrent <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + geom_line(size=1.5) +
    theme_few() 
  ggsave(voltCurrent, filename = paste0(i,".png"), width = 10, height = 6)
  
  dat.melt <- melt(dat, measure.vars = c("conductivity", "resistance"), id.vars = c("time"))
  resistConduct <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + geom_line(size=1.5) +
    theme_few() 
  ggsave(resistConduct, filename = paste0(i,"resistConduct.png"), width = 10, height = 6)
})
