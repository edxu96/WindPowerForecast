# DTU31761A3: Wind Power Output Prediction using Regression
# Adaptive Model with Sliding SliWin
# author: Edward J. Xu
# date: May 20th, 2019
# setwd("~/Desktop/WindPowerForecast_DTU31761A3_EDXU")
rm(list = ls())
library(lubridate)
source("Data.R")
########################################################################################################################
## 0,  Functions
calVecWeightLocalGaussian <- function(sigma, vecX, kernal, numTrain){
    vecWeight <- rep(NA, numTrain)
    for (i in 1:numTrain) {
        vecWeight[i] <- exp(-(vecX[i] - kernal)^2 / 2 / sigma^2)
    }
    return(vecWeight)
}
removeNaData <- function(vecData, vecY = datfTrain$power){
    vecData <- vecData[!is.na(vecY)]
    return(vecData)
}
calKernalValue <- function(vecWeight, kernal, numTrainClean, vecX = datfTrain$speed.center, vecY = datfTrain$power){
    matWeightDiag <- diag(vecWeight, numTrainClean)
    matX <- matrix(1, nrow = numTrainClean, ncol = 2)
    matX[,2] <- vecX
    vecColY <- matrix(1, nrow = numTrainClean, ncol = 1)
    vecColY[,1] <- vecY
    vecBeta <- solve(t(matX) %*% matWeightDiag %*% matX) %*% t(matX) %*% matWeightDiag %*% vecColY
    vecColXKernal <- matrix(1, nrow = 2, ncol = 1)
    vecColXKernal[2,1] <- kernal
    kernalValue <- (t(vecBeta) %*% vecColXKernal)[1]
    return(kernalValue)
}
calVecWeightSliWinGaussian <- function(vecSeries, kernalSliWin, numTrain, lengthSliWin){
    vecWeightSliWin <- rep(0, numTrain)
    windowTail <- max(kernalSliWin - lengthSliWin, 1)  # If the windowTail is negative, choose 1 as the window tail
    for (i in windowTail:kernalSliWin) {
        vecWeightSliWin[i] <- 1
    }
    return(vecWeightSliWin)
}
calVecKernalValue <- function(matWeight = matWeight, numKernal = numKernal, dat = datfTrain){
    ## Remove the NA data
    vecXClean <- removeNaData(dat$speed.center, dat$power)
    vecYClean <- removeNaData(dat$power, dat$power)
    numTrainClean <- length(vecXClean)
    ## Being calculation
    vecKernalValue <- rep(NA, numKernal)
    for (i in 1:numKernal) {
        vecWeightClean <- removeNaData(matWeight[,i], dat$power)
        vecKernalValue[i] <- calKernalValue(vecWeightClean, vecKernal[i], numTrainClean, vecXClean, vecYClean)
    }
    return(vecKernalValue)
}
calMatWeightSliWinGaussian <- function(matWeight, series = datfTrain$series, kernalSliWin, numTrain, numKernal){
    matWeightSliWin <- matrix(NA, nrow = numTrain, ncol = numKernal)
    for (i in 1:numKernal) {
        matWeightSliWin[,i] <- matWeight[,i] * calVecWeightSliWinGaussian(series, kernalSliWin, numTrain, 720 * 3)
        # Length of the sliding window is set to 3 months
    }
    return(matWeightSliWin)
}
########################################################################################################################
## 2.1,
numSpan <- 10
vecKernal <- seq(0, 1, by = 1 / numSpan)
numKernal <- length(vecKernal)
matWeight <- matrix(NA, nrow = numTrain, ncol = numKernal)
for (i in 1:numKernal) {
    matWeight[,i] <- calVecWeightLocalGaussian(0.05, datfTrain$speed.center, vecKernal[i], numTrain)
}
vecKernalValue_1 <- calVecKernalValue(matWeight, numKernal, datfTrain)
## 2.2,
kernalSliWin_2 <- numTrain
matWeightSliWin_2 <- calMatWeightSliWinGaussian(matWeight, datfTrain$series, kernalSliWin_2, numTrain, numKernal)
vecKernalValue_2 <- calVecKernalValue(matWeightSliWin_2, numKernal, datfTrain)
## 2.3,
kernalSliWin_3 <- numTrain - 8760
matWeightSliWin_3 <- calMatWeightSliWinGaussian(matWeight, datfTrain$series, kernalSliWin_3, numTrain, numKernal)
vecKernalValue_3 <- calVecKernalValue(matWeightSliWin_3, numKernal, datfTrain)
## 2.4,
kernalSliWin_4 <- numTrain - 8760 * 2
matWeightSliWin_4 <- calMatWeightSliWinGaussian(matWeight, datfTrain$series, kernalSliWin_4, numTrain, numKernal)
vecKernalValue_4 <- calVecKernalValue(matWeightSliWin_4, numKernal, datfTrain)
########################################################################################################################
## 3,  Plot the Result
setEPS()
postscript("Image/102.eps", width = 11, height = 6)
plot(datfTrain$speed.center, datfTrain$power, xlab = 'Normalized Wind Speed', ylab = 'Wind Power Output', bty = "n",
     col = "lightgrey")
lines(vecKernal, vecKernalValue_2, col = "blue", lwd = 2, type = "b", pch = 18)
lines(vecKernal, vecKernalValue_3, col = "red", lwd = 2, lty = 2, type = "b", pch = 18)
lines(vecKernal, vecKernalValue_4, col = "forestgreen", lwd = 2, lty = 3, type = "b", pch = 18)
title(main = "Wind Power Forecast Training Data and Adaptive Model with Sliding SliWin")
legend("bottomright", inset = .02, legend = c("Training Data", "Current Model", "One Year Back", "Two Years Back"),
    col = c("lightgrey", "blue", "red", "forestgreen"), pch = c(1, 18, 18, 18), lty = c(NA, 1, 2, 3),
    lwd = c(NA, 2, 2, 2))
dev.off()
########################################################################################################################
