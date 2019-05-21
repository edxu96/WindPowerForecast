# DTU31761A3: Wind Power Output Prediction using Regression
# author: Edward J. Xu
# date: May 20th, 2019
# setwd("~/Desktop/WindPowerForecast_DTU31761A3_EDXU")
########################################################################################################################
# rm(list = ls())
library(lubridate)
cat("################################################################################\n") ##############################
cat("######## 0,  Data and Functions ########\n")
source("Data.R")
source("FuncCrossVali.R")
source("FuncLocalReg.R")
source("FuncSeasonAdap.R")
source("FuncWindDirec.R")
source("FuncOutput.R")
cat("################################################################################\n") ##############################
cat("######## 1,  vecKernal and matWeight for Local Regression ########\n")
source("PrepareLocalReg.R")
cat("################################################################################\n") ##############################
cat("######## 2.1,  Calculate kernalSeason and matWeightSeason for every fold ########\n")
numFold <- 10  # [number of folds for cross validation]
datfTrain["index"] <- setIndexCrossVali(numTrain, numFold)  # Set index for datfTrain according to fold
vecKernalSeason <- rep(NA, numFold)  # Vector of kernals for adaptive seasonal local regression
listMatWeightSeason <- vector("list", numFold)
for (i in 1:numFold) {
    minSeriesVali <- min(datfTrain[(datfTrain$index == i),]$series)
    maxSeriesVali <- max(datfTrain[(datfTrain$index == i),]$series)
    vecKernalSeason[i] <- (minSeriesVali + maxSeriesVali) / 2
    listMatWeightSeason[[i]] <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, vecKernalSeason[i])
    listMatWeightSeason[[i]][minSeriesVali:maxSeriesVali,] <- 0
}
cat("vecKernalSeason = [", paste(vecKernalSeason, collapse = ", "), "]\n", sep = "")
cat("--------------------------------------------------------------------------------\n")
cat("######## 2.2,  Calculate kernal value for every kernalSeason ########\n")
listVecKernalValue <- vector("list", numFold)
for(i in 1:numFold) {
    cat("---- Calculate kernal value for ", i, "-th kernalSeason ----\n")
    listVecKernalValue[[i]] <- calVecKernalValue(listMatWeightSeason[[i]], datfTrain)
    cat(i, "-th vecKernalValue = [", paste(listVecKernalValue[[i]], collapse = ", "), "]\n", sep = "")
    cat("--------------------------------------------------------------------------------\n")
}
outputListVecKernalValue(listVecKernalValue)
cat("################################################################################\n") ##############################
cat("######## 3,  Cross Validation of SALR ########\n")
cat("-------- 3.1,  First Iteration --------\n")
listResult <- optimWindDirection(1, listVecKernalValue, vecKernalSeason, datfTrain)
vecOptimPar <- listResult$par
vecOptimObj <- listResult$obj
rm(listResult)
# outputVecOptimPar(vecOptimPar)
cat("First iteration: vecOptimPar = [", paste(vecOptimPar, collapse = ", "), "]\n", sep = "")
cat("                 vecOptimObj = [", paste(vecOptimObj, collapse = ", "), "]\n", sep = "")
cat("--------------------------------------------------------------------------------\n")
numIte <- 3
matOptimPar <- matrix(1, nrow = numIte, ncol = 360)
matOptimObj <- matrix(1, nrow = numIte, ncol = 360)
matOptimPar[1,] <- vecOptimPar
matOptimObj[1,] <- vecOptimObj
# 3.2,  Following Iterations
for (ite in 2:numIte) {
    cat("--------", ite, "-th Iteration --------\n", sep = "")
    datfTrain$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfTrain)
    datfPred$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfPred)
    cat("The wind speed is centered.\n")
    listResult <- optimWindDirection(ite, listVecKernalValue, vecKernalSeason, datfTrain)
    matOptimPar[ite,1:length(listResult$par)] <- listResult$par
    matOptimObj[ite,1:length(listResult$obj)] <- listResult$obj
    cat(ite, "-th iteration: vecOptimPar = [", paste(matOptimPar[ite,], collapse = ", "), "]\n", sep = "")
    cat("--------------------------------------------------------------------------------\n")
}; rm(listResult, ite)
outputMatOptimPar(matOptimPar)
## 3.3,  Get vecOptimParProduct
vecOptimParProduct <- rep(1, 360)
for (i in 1:numIte) {
    for (j in 1:360) {
        vecOptimParProduct[j] <- vecOptimParProduct[j] * matOptimPar[i, j]
    }
}
cat("################################################################################\n") ##############################
cat("######## 4,  Prediction using SALR and Centered Wind Speed ########\n")
## 4.1,  Center the wind speed using optimal par from wind direction model
# datfTrain$speed.center <- updateWindSpeedCenter(vecOptimPar, datfTrain)
# datfPred$speed.center <- updateWindSpeedCenter(vecOptimPar, datfPred)
datfTrain$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfTrain)
datfPred$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfPred)
cat("The wind speed is centered.\n")
## 4.2,  Kernal for Adaptive Seasonal LR
kernalSeason <- numTrain  # Kernal of adaptive seasonal local regression
cat("kernalSeason = ", kernalSeason, "\n", sep = "")
matWeightSeason <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason)
## 4.3,  Calculate Kernal value
vecKernalValue <- calVecKernalValue(matWeightSeason, datfTrain)
# outputVecKernalValue(vecKernalValue)
cat("--------------------------------------------------------------------------------\n")
cat("Final: vecKernalValue = [", paste(vecKernalValue, collapse = ", "), "]\n", sep = "")
cat("################################################################################\n") ##############################
cat("######## 5,  Prediction ########\n")
vecPred <- predLocalReg(datfVali$speed.center, vecKernal, vecKernalValue)
rmse <- calPredictionRMSE(vecPred, datfVali$power)
cat("Final: RMSE = ", rmse, "\n", sep = "")
