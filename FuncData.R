# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Data
# author: Edward J. Xu
# date: May 22th, 2019
########################################################################################################################
frameDataTrain <- function(strData){
    # dat <- read.csv("Data/TrainData3.csv", header = T)
    dat <- read.csv(strData, header = T)
    dat <- dat[2:length(dat[,1]),]  # Remove the first line
    numTrain <- length(dat[,1])
    # Time Stamp
    cat("Whether all timeStamps are non-NA?", all(!is.na(dat[1:numTrain, 1])), "\n")
    vecTimeStamp <- ymd_hm(dat[, 1], "%Y%m%d %H:%M", tz = "UTC")[1:numTrain]
    # Angle related data
    vecCos100 <- dat[, 5] / sqrt(dat[, 5]^2 + dat[, 6]^2)
    vecCos10 <- dat[, 3] / sqrt(dat[, 3]^2 + dat[, 4]^2)
    vecAngle100 <- acos(vecCos100) * 180 / pi
    vecAngle100[dat[, 6] < 0] <- 360 - vecAngle100[dat[, 6] < 0]
    # Index for 360 angles
    vecDegree <- round(vecAngle100)
    vecDegree[vecDegree == 0] <- 360
    # Frame data
    datf <- data.frame("series" = seq(1, numTrain), "time" = vecTimeStamp,
                        "power" = dat[, 2], "u10" = dat[, 3], "v10" = dat[, 4],
                        "u100" = dat[, 5], "v100" = dat[, 6], "cos10" = vecCos10,
                        "cos100" = vecCos100, "angle100" = vecAngle100, "degree100" = vecDegree)
    return(datf)
}
frameDataPred <- function(strData){
    dat <- read.csv(strData, header = T)
    numPred <- length(dat[,1])
    vecTimeStampRaw <- ymd_hm(dat[1:numPred, 1], "%Y%m%d %H:%M", tz = "UTC")
    vecTimeStamp <- vecTimeStampRaw[1:numPred]
    # Angle related data
    vecCos100 <- dat[, 4] / sqrt(dat[, 4]^2 + dat[, 5]^2)
    vecCos10 <- dat[, 2] / sqrt(dat[, 2]^2 + dat[, 3]^2)
    vecAngle100 <- acos(vecCos100) * 180 / pi
    vecAngle100[dat[, 5] < 0] <- 360 - vecAngle100[dat[, 5] < 0]
    # Index for 360 angles
    vecDegree <- round(vecAngle100)
    vecDegree[vecDegree == 0] <- 360
    # Frame data
    datf <- data.frame("series" = seq(1, numPred), "time" = vecTimeStamp, "u10" = dat[, 2],
                        "v10" = dat[, 3], "u100" = dat[, 4], "v100" = dat[, 5],
                        "cos10" = vecCos10, "cos100" = vecCos100, "angle100" = vecAngle100,
                        "degree100" = vecDegree)
    return(datf)
}
calNorminalSpeed <- function(alpha, datf){
    vec_speed <- (1 - alpha) * sqrt(datf$u10^2 + datf$v10^2) + alpha * sqrt(datf$u100^2 + datf$v100^2)
    return(vec_speed)
}
setIndexCrossVali <- function(num, numFold){
    # Index in 10 fold for cross validation
    numPerFold <- round(num / numFold)
    vecIndex <- rep(numFold, num)
    for (i in 1:(numFold - 1)) {
        vecIndex[(numPerFold * (i - 1) + 1):(numPerFold * i)] <- rep(i, numPerFold)
    }
    return(vecIndex)
}
