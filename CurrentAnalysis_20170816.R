nameFile <- ("run1.trc") 
title <- unlist(strsplit(nameFile, split = "\\."))[1]
dat <- read.delim(nameFile, sep = "\t", header = FALSE, skip = 10)
names(dat)[c(1:3)] <- c("timesec", "voltage", "current") 
dat <- mutate(dat, time= timesec/60)

library(reshape2)

dat <- mutate(dat, resistance = voltage / current)
dat <- mutate(dat, conductivity = current / voltage)


dat.melt <- melt(dat, measure.vars = c("voltage", "current"), id.vars = c("time"))
voltCurrent <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + geom_line(size=1.5) +
  theme_few() #+ 
  #ylim(-2, 15) + 
  #xlim(NA,NA)
voltCurrent
ggsave(voltCurrent, filename = paste0(title,"voltCurrent.png"), width = 10, height = 6)

dat.melt <- melt(dat, measure.vars = c("conductivity", "resistance"), id.vars = c("time"))
resistConduct <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + geom_line(size=1.5) +
  theme_few() + 
  ylim(0, 15) #+ 
  #xlim(1,NA)
resistConduct
ggsave(resistConduct, filename = paste0(title,"resistConduct.png"), width = 10, height = 6)

