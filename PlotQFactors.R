nameFile <- ("18.csv") 
title <- unlist(strsplit(nameFile, split = "\\."))[1]
dat <- read.csv(file = nameFile, header = FALSE)
dat <- dat[,c(1,18:22)]
names(dat)[c(1:6)] <- c("time", "Q1", "Q2", "Q3", "Q4", "Q5") 
dat.melt <- melt(dat, id.vars="time", measure.vars = c("Q1", "Q2", "Q3", "Q4", "Q5"))

QFactor <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + theme_few() + geom_line(size=.5) #+  
QFactor 

SmoothFactor <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + 
  theme_few() + stat_smooth(method = "loess", span = 0.02, se=TRUE)
SmoothFactor

ggsave(QFactor, filename = paste0('plots',"/",title,"QFactor.png"), width = 10, height = 6)
ggsave(SmoothFactor, filename = paste0('plots',"/",title,"SmoothQFactor.png"), width = 10, height = 6)



ringList <- c("19.csv", "67.csv", "123.csv")
for(i in ringList){
  nameFile <- (i) 
  title <- unlist(strsplit(i, split = "\\."))[1]
  dat <- read.csv(file = i, header = FALSE)
  dat <- dat[,c(1,18:22)]
  names(dat)[c(1:6)] <- c("time", "Q1", "Q2", "Q3", "Q4", "Q5") 
  dat.melt <- melt(dat, id.vars="time", measure.vars = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  QFactor <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + geom_line(size=.5) + theme_few() #+ 
  #xlim(0, 140)
  QFactor
  ggsave(QFactor, filename = paste0('plots',"/",title,"QFactor.png"), width = 10, height = 6)
}

ringList <- c("19.csv", "67.csv", "123.csv")
for(i in ringList){
  nameFile <- (i) 
  title <- unlist(strsplit(i, split = "\\."))[1]
  dat <- read.csv(file = i, header = FALSE)
  dat <- dat[,c(1,18:22)]
  names(dat)[c(1:6)] <- c("time", "Q1", "Q2", "Q3", "Q4", "Q5") 
  dat.melt <- melt(dat, id.vars="time", measure.vars = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  SmoothFactor <- ggplot(dat.melt, aes(x = time, y = value, color = variable)) + 
    theme_few() + stat_smooth(method = "loess", span = 0.02, se=TRUE)
  SmoothFactor
  ggsave(SmoothFactor, filename = paste0('plots',"/",title,"SmoothQFactor.png"), width = 10, height = 6)
}
