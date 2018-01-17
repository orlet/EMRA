### Source this code. For an individual set of runs, input would look like
### ParseRingShifts(time1 = 41, time2 = 52) 

ParseRingShifts <- function(time1, time2){
        ### create list of files to iterate through based on avg qualifier
        files <- list.files(pattern = "*_avg.txt", recursive = FALSE)
        data <- lapply(files, function(i){
                ### read in each file and store data frame
                data <- read.delim(i, sep = "\t", header = TRUE)
                ### get name for each target
                target <- unlist(strsplit(i, split = "_"))[1:2]
                target <- gsub(pattern = "group", x = target, "")
                target <- paste(target, collapse = "_")
                ### find index for each time point
                ind1 <- which.min(abs(data$X_avg-time1))
                ind2 <- which.min(abs(data$X_avg-time2))
                ### pull pertinent data at each index 
                data1<- data[ind1,1:4]
                data2<- data[ind2,1:4]
                ### rename second data point headers
                names(data2) <- c("X_avg2", "Y_avg2", "Y_std2", "Y_stderr2")
                ### join data so adjacent in file
                allData <- c(data1, data2)
                ### calculate and store net shift, standard deviation and standard error
                NetShift <- (allData$Y_avg2 - allData$Y_avg)
                Stdev <- sqrt((allData$Y_std2)^2 + (allData$Y_std)^2)
                StdErr <- sqrt((allData$Y_stderr2)^2 + (allData$Y_stderr)^2)
                allData$Target <- target
                allData$NetShift <- NetShift
                allData$Stdev <- Stdev
                allData$StdErr <- StdErr
                allData
        })
        data <- dplyr::bind_rows(data)
        readr::write_csv(data, "ShingRifts.csv")
}

### if analyzing multiple sets of data
ParseAllData <- function() {
        foldersList <- list.dirs(recursive = FALSE)
        lapply(foldersList, function(i){
                directory <- getwd()
                setwd(i)
                ParseRingShifts(time = 41, time2 = 52)
                setwd(directory)
        })
}