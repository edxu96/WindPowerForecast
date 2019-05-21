# DTU31761A3: Wind Power Output Prediction using Regression
# author: Edward J. Xu
# date: May 20th, 2019
setwd("~/Documents/Github/WindPowerPrediction")
rm(list = ls())
library(lubridate)
########################################################################################################################
cat("################################################################################\n") ##############################
cat("######## 0,  Control Parameters, Data and Functions ########\n")
## Control parameters
numFold      <- 10     # [number of folds for cross validation]
numIte       <- 3      # [number of further iterations]
wheFurIte    <- FALSE  # [whether do further iterations]
OutputSeries <- 1      # [series number of the output file]
wheOuput     <- FALSE  # [whether to output the results]
wheVali      <- FALSE  # [whether to validate the result]
## Name of the data files
# data for validation is the tail data in training data in next session
strNameTrain <- "Data/TrainData3.csv"
strNamePred  <- "Data/WeatherForecastInput4.csv"
strNameVali  <- "Data/TrainData4.csv"
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
cat("######## 2.1,  kernalSeason and matWeightSeason for Cross Validation ########\n")
datfTrain["index"] <- setIndexCrossVali(numTrain, numFold)  # Set index for datfTrain according to fold
vecKernalSeason <- rep(NA, numFold)  # Vector of kernals for seasonal adaptive local regression
listMatWeightSeason <- vector("list", numFold)
for (i in 1:numFold) {
    minSeriesVali <- min(datfTrain[(datfTrain$index == i),]$series)  # [beginning of the validation period]
    maxSeriesVali <- max(datfTrain[(datfTrain$index == i),]$series)  # [end of the validation period]
    vecKernalSeason[i] <- (minSeriesVali + maxSeriesVali) / 2  # Set the center time of validation period as the kernal
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
if (wheOutput) {
    outputlistVec(listVecKernalValue, OutputSeries)
}
cat("################################################################################\n") ##############################
cat("####### 3,  Cross Validation to Find Optimal Con-Coef for Wind Direction #######\n")
cat("-------- 3.1,  First Iteration --------\n")
listResult <- optimWindDirection(1, listVecKernalValue, vecKernalSeason, datfTrain)
vecOptimPar <- listResult$par
vecOptimObj <- listResult$obj
rm(listResult)
if (wheOutput) {
    outputResult(vecOptimPar, OutputSeries)
}
cat("First iteration: vecOptimPar = [", paste(vecOptimPar, collapse = ", "), "]\n", sep = "")
cat("                 vecOptimObj = [", paste(vecOptimObj, collapse = ", "), "]\n", sep = "")
cat("--------------------------------------------------------------------------------\n") # ----------------------------
# 3.2,  Further Iterations
if (wheFurIte) {
    matOptimPar <- matrix(1, nrow = numIte, ncol = 360)
    matOptimObj <- matrix(1, nrow = numIte, ncol = 360)
    matOptimPar[1,] <- vecOptimPar
    matOptimObj[1,] <- vecOptimObj
    for (ite in 2:numIte) {
        cat("--------", ite, "-th Iteration --------\n", sep = "")
        # The speed.center should be updated before every further iteration
        datfTrain$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfTrain, "datfTrain")
        datfPred$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfPred, "datfPred")
        cat("The wind speed is centered.\n")
        listResult <- optimWindDirection(ite, listVecKernalValue, vecKernalSeason, datfTrain)
        matOptimPar[ite, 1:length(listResult$par)] <- listResult$par
        matOptimObj[ite, 1:length(listResult$obj)] <- listResult$obj
        cat(ite, "-th iteration: vecOptimPar = [", paste(matOptimPar[ite,], collapse = ", "), "]\n", sep = "")
        cat("--------------------------------------------------------------------------------\n")
    }; rm(listResult, ite)
}
if (wheOutput) {
    outputResult(matOptimPar, OutputSeries)
}
## 3.3,  Get vecOptimParProduct ----------------------------------------------------------------------------------------
# The final optimal coefficient for every degree are the product of every iteration.
vecOptimParProduct <- rep(1, 360)
for (i in 1:numIte) {
    for (j in 1:360) {
        vecOptimParProduct[j] <- vecOptimParProduct[j] * matOptimPar[i, j]
    }
}
cat("################################################################################\n") ##############################
cat("######## 4,  Prediction using SALR and Centered Wind Speed ########\n")
## 4.1,  Center the wind speed using optimal par from wind direction model
if (wheFurIte) {
    datfTrain$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfTrain, "datfTrain")
    datfPred$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfPred, "datfPred")
} else {
    datfTrain$speed.center <- updateWindSpeedCenter(vecOptimPar, datfTrain, "datfTrain")
    datfPred$speed.center <- updateWindSpeedCenter(vecOptimPar, datfPred, "datfPred")
}
## 4.2,  Kernal for Adaptive Seasonal LR
kernalSeason <- numTrain  # Kernal of adaptive seasonal local regression
cat("kernalSeason = ", kernalSeason, "\n", sep = "")
matWeightSeason <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason)
## 4.3,  Calculate Kernal value
vecKernalValue <- calVecKernalValue(matWeightSeason, datfTrain)
if (wheOutput) {
    outputResult(vecKernalValue, OutputSeries)
}
cat("--------------------------------------------------------------------------------\n")
cat("Final: vecKernalValue = [", paste(vecKernalValue, collapse = ", "), "]\n", sep = "")
cat("################################################################################\n") ##############################
cat("######## 5,  Prediction ########\n")
vecPred <- predLocalReg(datfPred$speed.center, vecKernal, vecKernalValue)
if (wheOutput) {
    outputResult(vecPred, OutputSeries)
}
if (wheVali) {
    rmse <- calPredictionRMSE(vecPred, datfVali$power)
    cat("Final: RMSE = ", rmse, "\n", sep = "")
}
