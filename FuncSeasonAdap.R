# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Adaptive Seasonal Model
# author: Edward J. Xu
# date: May 20th, 2019
# setwd("~/Desktop/WindPowerForecast_DTU31761A3_EDXU")
########################################################################################################################
calVecWeightSeasonGaussian <- function(vecSeries, kernalSeason, numTrain){
    vecWeightSeason <- rep(NA, numTrain)
    for (i in 1:numTrain) {
        vecWeightSeason[i] <- (cos((vecSeries[i] - kernalSeason) * (2 * pi / 8760)) + 1) / 2
    }
    return(vecWeightSeason)
}
calMatWeightSeasonGaussian <- function(matWeight, series = datfTrain$series, kernalSeason){
    numTrain <- dim(matWeight)[1]
    numKernal <- dim(matWeight)[2]
    matWeightSeason <- matrix(NA, nrow = numTrain, ncol = numKernal)
    for (i in 1:numKernal) {
        matWeightSeason[,i] <- matWeight[,i] * calVecWeightSeasonGaussian(series, kernalSeason, numTrain)
    }
    return(matWeightSeason)
}
