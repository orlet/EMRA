GetName <- function(){
  # get the filename from the current working directory
  directory <- basename(getwd())
  
  # directory naming from MRR: "CHIPNAME_gaskGASK_DATE"
  # extracts and returns GASK from directory name
  name <- unlist(strsplit(directory, split = "_"))[2]
  
  # define name as global variable for use in other functions
  name <<- gsub('gask','',name) # removes "gask" from name
}      

#### for blank rings, filename= 'groupNames_LMR.csv'
#### for DNA rings, filename= 'TargetDNA_allClusters.csv'
AggData <- function(loc = 'plots', filename = 'TargetDNA_allClusters.csv') {
  library(tidyverse)
  
  # get working directory to reset at end of function
  directory <- getwd()
  
  # get information of chip layout from github repository
  if (!file.exists(filename)){
    git <- "https://raw.githubusercontent.com/orlet/EMRA/master/"
    url <- paste0(git, filename)
    filename <- basename(url)
    download.file(url, filename)
  }
  
  # read in recipe/chip layout
  recipe <- read_csv(filename)
  colnames(recipe)[1] <- "Target" # rename column to remove byte order mark
  targets <- recipe$Target
  
  # generate list of rings to analyze (gets all *.csv files)
  rings <- list.files(directory, pattern = ".csv", recursive = FALSE)
  idfile <- grepl("group|Target|Reverse", rings)
  removeFiles <- c("comments.csv", rings[idfile])
  rings <- rings[!rings %in% removeFiles]
  
  # add data to data frame corresponding for each ring in rings
  df <- lapply(rings, function(i){
    dat <- read_csv(i, col_names = FALSE)
    ringNum <- as.numeric(strsplit(i, "\\.")[[1]][1])
    recipeCol <- which(recipe$Ring == ringNum)
    tmp <- dat[,c(1,2)] # time and shift from raw data
    tmp$ring <- ringNum
    tmp$group <- recipe$Group[recipeCol]
    tmp$groupName <- as.character(recipe$Target[[recipeCol]])
    tmp$channel <- recipe$Channel[[recipeCol]]
    tmp$run <- name
    tmp$timePoint <- seq(1:nrow(dat))
    tmp
  })
  
  df <- bind_rows(df)
  
  # renames columns in df
  names(df) <- c("Time", "Shift", "Ring", "Group", "Target", "Channel",
                 "Experiment", "Time Point")
  
  # creates "plots" directory
  dir.create(loc, showWarnings = FALSE)
  
  # saves aggregated data with name_allRings.csv
  write_csv(df, paste(loc, '/', name, "_allRings.csv", sep=""))
}

SubtractControl <- function(loc = 'plots', ch, cntl){
  #load relevant libraries
  library(tidyverse)
  
  # get ring data and filter by channel
  dat <- read_csv(paste0(loc, "/", name, "_", "allRings.csv"))
  if (ch != "U"){dat <- filter(dat, Channel == ch)}
  dat <- filter(dat, Target != "Ignore")
  # get thermal control averages
  controls <- filter(dat, Target == cntl) %>% group_by(`Time Point`) %>%
    summarise_at("Shift", mean) %>% select(Shift) %>% unlist()
  dat$Cntl <- rep(controls, length(unique(dat$Ring)))
  # subtracts thermal controls from each ring
  dat.cntl <- mutate(dat, Shift = Shift - Cntl)
  # remove control column and control rings
  dat.cntl <- filter(dat.cntl, Target != cntl)
  dat.cntl$cntl <- NULL
  
  write_csv(dat.cntl, paste(loc,"/", name, "_", cntl, "Control", "_ch", ch, 
                            ".csv", sep = ''))   
}

PlotRingData <- function(cntl, ch, loc = 'plots', splitPlot = FALSE){
  # loads relevant libraries and plot theme
  library(tidyverse)
  library(ggthemes)
  theme_set(theme_few(base_size = 16))
  
  # use thermally controlled data if desired
  if (cntl != "raw"){
    dat <- read_csv(paste(loc, "/", name, "_", cntl, "Control", 
                          "_ch", ch,".csv", sep=''))
  } else if (cntl == "raw") {
    dat <- read_csv(paste(loc, "/", name, "_allRings.csv", sep=''))
    if (ch != "U") {dat <- filter(dat, Channel == ch)}
  }
  
  # configure plot and legend
  plots <- ggplot(dat, aes(x = Time, y = Shift,
                           color = Target, group = Ring)) + 
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)")),
         color = "Target") +
    scale_color_brewer(palette = "Set1") +
    geom_line(size=1.3, alpha=0.5) + 
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste(name, "Ch:", ch, "Control:", cntl, sep = " ")) 
  
  # alternative plots with averaged clusters
  
  dat.2 <- dat %>% group_by(Target, `Time Point`) %>% 
    summarise_at(vars(Time, Shift), funs(mean, sd))
  
  plot2 <- ggplot(dat.2, aes(x = Time_mean, y = Shift_mean, 
                             color = Target)) +
    scale_color_brewer(palette = "Set1") + 
    geom_line(size = 1.2) +
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste(name, "Ch:", ch, "Control:", cntl, sep = " "))
  
  plot3 <- plot2 + 
    geom_ribbon(aes(ymin = Shift_mean - Shift_sd,
                    ymax = Shift_mean + Shift_sd, 
                    linetype = NA, fill = Target), alpha = 1/8) + scale_fill_brewer(palette = "Set1") 
  
  ### if using raw data, then remove thermals from plot
  if(cntl == "raw"){
    dat.4 <- filter(dat, Target != "thermal")
    dat.4.avg <- dat.4 %>%
      group_by(Target, `Time Point`) %>%
      summarise_each(funs(mean,sd), c(Time, Shift))
    plot4 <- ggplot(dat.4.avg, 
                    aes(x = Time_mean, y = Shift_mean, 
                        color = Target)) + geom_line(size = 1.2) + 
      scale_color_brewer(palette = "Set1") + 
      labs(x = "Time (min)", 
           y = expression(paste("Relative Shift (",Delta,"pm)")))
    
    plot5 <- plot4 + geom_ribbon(aes(ymin = Shift_mean - Shift_sd, 
                                     ymax = Shift_mean + Shift_sd, 
                                     linetype = NA, fill = Target), alpha = 1/8) + scale_fill_brewer(palette = "Set1")
    ggtitle(paste(name, "Ch:", ch, "Control:", cntl, "thermal removed", sep = " "))
    
    ggsave(plot = plot5,
           file = paste0(loc, "/", name, "_", cntl,
                         "Control", "_ch", ch, "_avg_thermalRemoved.png"),
           width = 10, height = 6)
  }
  
  
  if (splitPlot){
    plots <- plots + facet_grid(. ~ Channel)
  }
  
  # save plots
  ggsave(plot = plots, 
         file = paste0(loc, "/", name, "_", cntl, 
                       "Control_ch", ch, ".png"),
         width = 10, height = 6)
  ggsave(plot = plot2, 
         file = paste0(loc, "/", name, "_", cntl,
                       "Control", "_ch", ch, "_avg.png"),
         width = 10, height = 6)
  ggsave(plot = plot3, 
         file = paste0(loc, "/", name, "_", cntl,
                       "Control", "_ch", ch, "_avg.png"),
         width = 10, height = 6)
}


CheckRingQuality <- function(loc = 'plots', time1, time2, varLevel = 100) {
  library(tidyverse)
  
  dat <- read_csv(paste0(loc,"/", name, "_allRings.csv"))
  dat <- subset(dat, Time > time1 & Time < time2)
  
  dat.avg <- dat %>% group_by(Ring) %>%
    summarise_at(vars(Shift), funs(var))
  
  ringWinners <- filter(dat.avg, Shift < varLevel) %>% select(Ring)
  ringLosers <- filter(dat.avg, Shift > varLevel) %>% select(Ring)
  
  write_csv(ringWinners, paste0(loc, '/', name, "_ringWinners.csv"))
  write_csv(ringLosers, paste0(loc, '/', name, "_ringLosers.csv"))
}

PlotBlankCorr <- function(startTime, loc='plots'){
  dat <- read_csv(paste("plots/", name, "_allRings.csv", sep=''))
  dat1 <- filter(dat, Channel == 2, Target != "thermal")
  
  sub1 <- "Blank"
  
  dat.cntl1 <- filter(dat1, Target == sub1)
  dat.cntl1 <- dat.cntl1 %>% group_by(`Time Point`) %>% summarise_at(vars(Shift), funs(mean,sd))
  cntl1 <- dat.cntl1$mean
  
  dat1 <- dat1 %>% group_by(Ring) %>%
    mutate(Shifted = Shift - cntl1)
  
  dat1 <- subset(dat1, Time > startTime)
  dat1 <- mutate(dat1, Time = Time - Time[1],
                 Shift = Shift - Shift[1],
                 Shifted = Shifted - Shifted[1])
  
  dat.avg1 <- dat1 %>% group_by(Target, `Time Point`) %>% 
    summarise_at(vars(Time, Shifted), funs(mean,sd))
  dat.avg1 <- filter(dat.avg1, Target != sub1)
  
  plot1 <- ggplot(dat.avg1,
                  aes(x = Time_mean,
                      y = Shifted_mean,
                      color = (Target))) +
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste0(name," DNA, Blank Subtracted")) +
    theme_few(base_size = 16) +
    geom_line(size=1.2) +
    scale_color_brewer(palette = "Set1") 
  
  if(startTime>0){
    ggsave(plot1, filename = paste0(loc,"/","BlankCorrect",startTime,".png"), 
           width = 10, height = 6)
  } else{
    ggsave(plot1, filename = paste0(loc,"/","BlankCorrect",startTime,".png"),
           width = 10, height = 6)
    
  }
  
}

PlotOffTargetCorr <- function(startTime, loc='plots'){
  dat5 <- read_csv(paste("plots/", name, "_allRings.csv", sep=''))
  dat5 <- filter(dat5, Channel == 2, Target != "Blank", Target != "thermal")
  
  sub5 <- "Off-Target"
  
  dat.cntl5 <- filter(dat5, Target == sub5)
  dat.cntl5 <- dat.cntl5 %>% group_by(`Time Point`) %>% summarise_at(vars(Shift), funs(mean,sd))
  cntl5 <- dat.cntl5$mean
  
  
  dat5 <- dat5 %>% group_by(Ring) %>%
    mutate(Shifted = Shift - cntl5)
  
  dat5 <- subset(dat5, Time > startTime)
  dat5 <- mutate(dat5, Time = Time - Time[1],
                 Shift = Shift - Shift[1],
                 Shifted = Shifted - Shifted[1])
  
  dat.avg5 <- dat5 %>% group_by(Target, `Time Point`) %>% 
    summarise_at(vars(Time, Shifted), funs(mean,sd))
  dat.avg5 <- filter(dat.avg5, Target != sub5)
  
  plot6 <- ggplot(dat.avg5,
                  aes(x = Time_mean,
                      y = Shifted_mean,
                      color = (Target))) +
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste(name," DNA, Off-Target Subtracted", sep='')) +
    theme_few(base_size=16) +
    scale_color_brewer(palette = "Set1") +
    geom_line(size=1.2) 
  if(startTime>0){
    ggsave(plot6, filename = paste0(loc,"/","OffTargetCorrect",startTime,".png"), width = 10, height = 6)
  } else{
    ggsave(plot6, filename = paste0(loc,"/","OffTargetCorrect",startTime,".png"), width = 10, height = 6)
  }
}

AnalyzeData <- function() {
  GetName()
  AggData(filename = "TargetDNA_allClusters.csv")
  SubtractControl(ch = 2, cntl = "thermal")
  PlotRingData(cntl = "thermal", ch = 2, splitPlot = FALSE)
  PlotRingData(cntl = "raw", ch = 2, splitPlot = FALSE)
  PlotBlankCorr(startTime = 0)
  PlotOffTargetCorr(startTime = 0)
  CheckRingQuality(time1 = 10, time2 = 20)
  if (startTime != 0){
          PlotInjectionTime()
          PlotInjectionTimeThermal()
  }
        
}

AnalyzeAllData <- function() {
  foldersList <- list.dirs(recursive = FALSE)
  lapply(foldersList, function(i){
    directory <- getwd()
    setwd(i)
    AnalyzeData()
    setwd(directory)
  })
}

PlotInjectionTime <- function(startTime, loc='plots'){
  library(tidyverse)
  library(ggthemes)
  theme_set(theme_few(base_size = 16))
  
  dat <- read_csv(paste("plots/", name, "_allRings.csv", sep=''))
  dat <- filter(dat, Channel == 2)
  
  datSub <- subset(dat, Time > startTime)
  datAdjusted <- mutate(datSub, Time = Time - Time[1],
                        Shift = Shift - Shift[1])
  
  datAveraged <- datAdjusted %>% 
    group_by(Target, `Time Point`) %>% 
    summarise_at(vars(Time, Shift), funs(mean,sd))
  
  
  plotRaw <- ggplot(datAveraged,
                    aes(x = Time_mean,
                        y = Shift_mean,
                        color = (Target))) +  
    geom_line(size=1.2) +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste0(name," Ch:2, raw data, injection after ",as.character(startTime)))
  
  plotRawFinal <- plotRaw + geom_ribbon(aes(ymin = Shift_mean - Shift_sd,
                                            ymax = Shift_mean + Shift_sd, 
                                            linetype = NA, fill = Target), alpha = 1/8) + scale_fill_brewer(palette = "Set1") 
  
  ggsave(plotRawFinal, filename = paste0(loc,"/","TimeCorrectRaw",startTime,".png"), width = 10, height = 6)
}

PlotInjectionTimeThermal <- function(startTime, loc='plots'){
  library(tidyverse)
  library(ggthemes)
  theme_set(theme_few(base_size = 16))
  
  dat <- read_csv(paste("plots/", name, "_allRings.csv", sep=''))
  dat <- filter(dat, Channel == 2)
  
  reference <- "thermal"
  datThermal <- filter(dat, Target == reference)
  datThermal <- datThermal %>%
    group_by(`Time Point`) %>%
    summarise_at(vars(Shift), funs(mean,sd))
  control <- datThermal$mean
  
  datThermal <- dat %>% 
    group_by(Ring) %>%
    mutate(Shifted = Shift - control)
  
  datThermalSub <- subset(datThermal, Time > startTime)
  datThermalAdjust <- mutate(datThermalSub, Time = Time - Time[1], 
                             Shift = Shift - Shift[1],
                             Shifted = Shifted - Shifted[1])
  
  datThermalAveraged <- datThermalAdjust %>% 
    group_by(Target, `Time Point`) %>% 
    summarise_at(vars(Time, Shifted), funs(mean,sd))
  datThermalAveraged <- filter(datThermalAveraged, Target != reference)
  
  plotThermal <- ggplot(datThermalAveraged,
                        aes(x = Time_mean,
                            y = Shifted_mean,
                            color = (Target))) +  
    geom_line(size=1.2) +
    scale_color_brewer(palette = "Set1") +
    labs(x = "Time (min)", 
         y = expression(paste("Relative Shift (",Delta,"pm)"))) +
    ggtitle(paste0(name," Ch:2, raw data, injection after ",as.character(startTime)))
  
  plotThermalFinal <- plotThermal + geom_ribbon(aes(ymin = Shifted_mean - Shifted_sd,
                                                    ymax = Shifted_mean + Shifted_sd, 
                                                    linetype = NA, fill = Target), alpha = 1/8) + scale_fill_brewer(palette = "Set1")
  
  ggsave(plotThermalFinal, filename = paste0(loc,"/","TimeCorrectThermal",startTime,".png"), width = 10, height = 6)
}


# for zooming in and out..
# change dat_ch2 to dat_ch2_avg for std dev of rings

# 
# dat <- read_csv(paste("plots/2uM_allRings.csv", sep=''))
# dat_ch2 <- filter(dat, Channel == 2, Target != "thermal")
# dat_ch2_avg <- dat_ch2 %>%
#   group_by(Target, `Time Point`) %>%
#   summarise_each(funs(mean,sd), c(Time,Shift))
# 
# 
# plots <- ggplot(dat_ch2_avg, aes(x = Time_mean, y = Shift_mean, color = Target)) +
#   geom_line(size = 1.2) + theme_few(base_size = 16) + scale_color_brewer(palette = "Set1")#+ xlim(8, 30) + ylim(-5,70)
# 
# plots <- plots + geom_ribbon(aes(ymin = Shift_mean - Shift_sd,
#                                  ymax = Shift_mean + Shift_sd,
#                                  fill = Target,
#                                  linetype = NA), alpha = 1/4) + scale_fill_brewer(palette = "Set1") +  xlab("Time (min)")  + 
#   ylab(expression(paste("Shift (",Delta,"pm)"))) +
#   ggtitle(label = "Thermal Removed")
# plots
# ggsave(plots, filename= paste0('plots/',"hiddenThermal.png"), width = 10, height = 6)
# 
# 
# for picking out rings that misbehave manually
# library(plotly)
# dat <- read_csv(paste("plots/ssDNA_allRings.csv", sep=''))
# dat_ch2 <- filter(dat, Channel == 2)
# 
# plots <- ggplot(dat_ch2, aes(x = Time, y = Shift, color = Target, group = Ring)) +
#   geom_line(alpha = 1/2) #+ xlim(6,20)
# 
# plots
# ggplotly(plots)library(plotly)

