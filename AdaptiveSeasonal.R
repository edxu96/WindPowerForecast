# DTU31761A3: Wind Power Output Prediction using Regression
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
calVecWeightSeasonGaussian <- function(vecSeries, kernalSeason, numTrain){
    vecWeightSeason <- rep(NA, numTrain)
    for (i in 1:numTrain) {
        vecWeightSeason[i] <- (cos((vecSeries[i] - kernalSeason) * (2 * pi / 8760)) + 1) / 2
    }
    return(vecWeightSeason)
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
calMatWeightSeasonGaussian <- function(matWeight, series = datfTrain$series, kernalSeason, numTrain, numKernal){
    matWeightSeason <- matrix(NA, nrow = numTrain, ncol = numKernal)
    for (i in 1:numKernal) {
        matWeightSeason[,i] <- matWeight[,i] * calVecWeightSeasonGaussian(series, kernalSeason, numTrain)
        # numRow of matWeightSeason is numTrain
        # numCol of matWeightSeason is numKernal
    }
    return(matWeightSeason)
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
kernalSeason_2 <- numTrain
matWeightSeason_2 <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason_2, numTrain, numKernal)
vecKernalValue_2 <- calVecKernalValue(matWeightSeason_2, numKernal, datfTrain)
## 2.3,
kernalSeason_3 <- numTrain - 8760 / 2
matWeightSeason_3 <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason_3, numTrain, numKernal)
vecKernalValue_3 <- calVecKernalValue(matWeightSeason_3, numKernal, datfTrain)
## 2.4,
kernalSeason_4 <- numTrain - 8760 / 4
matWeightSeason_4 <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason_4, numTrain, numKernal)
vecKernalValue_4 <- calVecKernalValue(matWeightSeason_4, numKernal, datfTrain)
########################################################################################################################
## 3,  Plot the Result
setEPS()
postscript("Image/101.eps", width = 11, height = 6)
plot(datfTrain$speed.center, datfTrain$power, xlab = 'Normalized Wind Speed', ylab = 'Wind Power Output', bty = "n",
     col = "lightgrey")
lines(vecKernal, vecKernalValue_2, col = "blue", lwd = 2, type = "b", pch = 18)
lines(vecKernal, vecKernalValue_3, col = "red", lwd = 2, lty = 2, type = "b", pch = 18)
lines(vecKernal, vecKernalValue_4, col = "forestgreen", lwd = 2, lty = 3, type = "b", pch = 18)
title(main = "Wind Power Forecast Training Data and Seasonal Adaptive Model")
legend("bottomright", inset = .02, legend = c("Training Data", "Current Season", "One Season Back", "Two Season Back"),
    col = c("lightgrey", "blue", "forestgreen", "red"), pch = c(1, 18, 18, 18), lty = c(NA, 1, 3, 2),
    lwd = c(NA, 2, 2, 2))
dev.off()
########################################################################################################################
