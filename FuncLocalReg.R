# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Local Regression
# author: Edward J. Xu
# date: May 22th, 2019
# setwd("~/Desktop/WindPowerForecast_DTU31761A3_EDXU")
########################################################################################################################
## 1,  Functions to generate vector of kernals
calVecWeightLocalGaussian <- function(sigma, vecX, kernal, numTrain){
    vecWeight <- rep(NA, numTrain)
    for (i in 1:numTrain) {
        vecWeight[i] <- exp(-(vecX[i] - kernal)^2 / 2 / sigma^2)
    }
    return(vecWeight)
}
########################################################################################################################
## 2,  Functions for calculate kernal values
removeNaData <- function(vecData, vec1, vec2){
    vecData <- vecData[!(is.na(vec1) | is.na(vec2))]
    return(vecData)
}
calKernalValue <- function(vecWeightKernal, kernal, numTrainClean, vecX = vecXClean, vecY = vecYClean){
    matWeightKernalDiag <- diag(vecWeightKernal, numTrainClean)
    matX <- matrix(1, nrow = numTrainClean, ncol = 2)
    matX[,2] <- vecX
    vecColY <- matrix(1, nrow = numTrainClean, ncol = 1)
    vecColY[,1] <- vecY
    vecBeta <- matrix(NA, nrow = 2, ncol = 1)
    vecBeta <- solve(t(matX) %*% matWeightKernalDiag %*% matX) %*% t(matX) %*% matWeightKernalDiag %*% vecColY
    cat("vecBeta = [", paste(vecBeta, collapse = ", "), "]\n", sep = "")
    ## Calculate value for the kernal
    vecColXKernal <- matrix(1, nrow = 2, ncol = 1)
    vecColXKernal[2, 1] <- kernal
    kernalValue <- (t(vecBeta) %*% vecColXKernal)[1]
    return(kernalValue)
}
calVecKernalValue <- function(matWeight = matWeight, datf = datfTrain){
    numKernal <- length(matWeight[1,])
    ## Remove the NA data
    vecXClean <- removeNaData(datf$speed.center, datf$power, datf$speed.center)
    vecYClean <- removeNaData(datf$power, datf$power, datf$speed.center)
    numTrainClean <- length(vecXClean)
    ## Being calculation
    vecKernalValue <- rep(NA, numKernal)
    for (i in 1:numKernal) {
        # cat("--------------------------------------------------------------------------------")
        vecWeightClean <- removeNaData(matWeight[,i], datf$power, datf$speed.center)
        # cat("Whether matWeightKernalDiag all non-NA? ", all(!is.na(matWeightKernalDiag)), "\n", sep = "")
        # cat("Whether matX all non-NA? ", all(!is.na(matX)), "\n", sep = "")
        # cat("Whether vecColY all non-NA? ", all(!is.na(vecColY)), "\n", sep = "")
        cat(i, "-th kernal = ", vecKernal[i], "\n", sep = "")
        vecKernalValue[i] <- calKernalValue(vecWeightClean, vecKernal[i], numTrainClean, vecXClean, vecYClean)
    }
    return(vecKernalValue)
}
########################################################################################################################
## 1,  Functions to predict using local regression
predLocalReg <- function(vecX, vecKernal, vecKernalValue){
    # cat("Whether all vecX are non-NA?", all(!is.na(vecX)), "\n")
    # cat("Whether all vecX are NULL?", is.null(vecX)), "\n")
    numPred <- length(vecX)
    vecPred <- rep(NA, numPred)
    for (i in 1:numPred) {
        x <- vecX[i]
        j <- 1
        while (!((vecKernal[j] <= x) & (vecKernal[(j + 1)] > x))) {
            j <- j + 1
        }
        x1 <- vecKernal[j]
        x2 <- vecKernal[(j + 1)]
        # print(x1)
        # print(x2)
        y1 <- vecKernalValue[j]
        y2 <- vecKernalValue[(j + 1)]
        vecPred[i] <- y1 + (x - x1) * (y2 - y1) / (x2 - x1)
    }
    return(vecPred)
}
