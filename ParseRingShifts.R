ParseRingShifts <- function(time1, time2){
        files <- list.files(pattern = "*_avg.txt", recursive = FALSE)
        data <- lapply(files, function(i){
                data <- read.delim(i, sep = "\t", header = TRUE)
                target <- unlist(strsplit(i, split = "_"))[1:2]
                target <- gsub(pattern = "group", x = target, "")
                target <- paste(target, collapse = "_")
                data <- data[,1:5]
                ind1 <- which.min(abs(data$X_avg-time1))
                ind2 <- which.min(abs(data$X_avg-time2))
                data1<- data[ind1,1:4]
                data2<- data[ind2,1:4]
                names(data2) <- c("X_avg2", "Y_avg2", "Y_std2", "Y_stderr2")
                allData <- c(data1, data2)
                NetShift <- (allData$Y_avg2 - allData$Y_avg)
                Stdev <- sqrt((allData$Y_std2)^2 + (allData$Y_std)^2)
                StdErr <- sqrt((allData$Y_stderr2)^2 + (allData$Y_stderr)^2)
                allData$Target <- target
                allData$Stdev <- Stdev
                allData$StdErr <- StdErr
                allData$NetShift <- NetShift
                allData
                })
        # print(data)
        data <- dplyr::bind_rows(data)
        print(data)
        readr::write_csv(data, "RungShorts.csv")
}

