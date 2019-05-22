# DTU31761A3: Wind Power Output Prediction using Regression
# author: Edward J. Xu
# date: May 22th, 2019
# version: 3.1
########################################################################################################################
# setwd("~/Documents/Github/WindPowerPrediction")
rm(list = ls())
library(lubridate)
########################################################################################################################
cat("#### 0,  Control Parameters, Data and Functions ################################\n") ##############################
## 0.1,  Control Parameters 1
numFold      <- 10   # [number of folds for cross validation]
numIte       <- 3    # [number of further iterations]
wheFurIte    <- F    # [whether do further iterations]
outputSeries <- 4    # [series number of the output file]
wheOutput    <- T    # [whether to output the results]
wheVali      <- F    # [whether to validate the result]
numConCoef   <- 360  # [number of concentration coefficients]
## 0.2, Name of the data files
strNameTrain <- "Data/TrainData3.csv"
strNamePred  <- "Data/WeatherForecastInput4.csv"
strNameVali  <- "Data/TrainData4.csv"  # Data for validation is the tail data in training data in next session
source("Data.R")  # All functions needed for Data.R is in FuncData.R
## 0.3, Function Files
source("FuncCrossVali.R")
source("FuncLocalReg.R")
source("FuncSeasonAdap.R")
source("FuncWindDirec.R")
if (wheOutput) {
    source("FuncOutput.R")
}
## 0.4,  Control Parameters 2
deltaKernalSeasonPred <- 0  # [forward value of kernalSeasonPred] If 10, means kernalSeasonPred = numTrain + 10.
# It can be numPred / 2, which will set the center of prediction period as the main season.
cat("################################################################################\n") ##############################
cat("#### 1/6,  vecKernal and matWeight for Local Regression ########################\n")
source("PreLocalReg.R")
cat("################################################################################\n") ##############################
cat("#### 2/6,  Prepare Seasonal Adaptive Models ####################################\n")
source("PreSeasonAdap.R")
cat("################################################################################\n") ##############################
cat("#### 3/6,  Cross Validation to Find Optimal Con-Coef for Wind Direction ########\n")
cat("---- 3.1,  Benchmark without Con-Coef ------------------------------------------\n")
mseBenchmark <- crossValid(vecKernal, listVecKernalValue, datfTrain, 10)
cat("mseBenchmark =", mseBenchmark, "\n")
cat("---- 3.2,  First Iteration -----------------------------------------------------\n")
listResult <- optimWindDirection(1, listVecKernalValue, vecKernalSeason, numConCoef, datfTrain)
vecOptimPar <- listResult$par
vecOptimObj <- listResult$obj
rm(listResult)
if (wheOutput) {
    outputResult(vecOptimPar, OutputSeries)
}
cat("aveImprove = ", sum(vecOptimObj - mseBenchmark) / numConCoef / mseBenchmark, "\n", sep = "")
# cat("vecOptimPar = [", paste(vecOptimPar, collapse = ", "), "]\n", sep = "")  # It's too long to print
# cat("vecOptimObj = [", paste(vecOptimObj, collapse = ", "), "]\n", sep = "")  # It's too long to print
cat("--------------------------------------------------------------------------------\n") # ----------------------------
cat("---- 3.3,  Further Iterations --------------------------------------------------\n")
# 3.2,  Further Iterations
if (wheFurIte) {
    matOptimPar <- matrix(1, nrow = numIte, ncol = numConCoef)
    matOptimObj <- matrix(1, nrow = numIte, ncol = numConCoef)
    matOptimPar[1,] <- vecOptimPar
    matOptimObj[1,] <- vecOptimObj
    for (ite in 2:numIte) {
        cat("----", ite, "-th Iteration ---------------------------------------------------\n", sep = "")
        # The speed.center should be updated before every further iteration
        datfTrain$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfTrain, numConCoef)
        datfPred$speed.center <- updateWindSpeedCenter(matOptimPar[(ite - 1),], datfPred, numConCoef)
        cat("The wind speed is centered.\n")
        listResult <- optimWindDirection(ite, listVecKernalValue, vecKernalSeason, numConCoef, datfTrain)
        matOptimPar[ite, 1:length(listResult$par)] <- listResult$par
        matOptimObj[ite, 1:length(listResult$obj)] <- listResult$obj
        cat("aveImprove = ", sum(matOptimObj[ite] - mseBenchmark) / numConCoef / mseBenchmark, "\n", sep = "")
        # cat("vecOptimPar = [", paste(matOptimPar[ite,], collapse = ", "), "]\n", sep = "")  # It's too long to print
        cat("--------------------------------------------------------------------------------\n")
    }; rm(listResult, ite)
}
if (wheOutput) {
    outputResult(matOptimPar, OutputSeries)
}
## 3.3,  Get vecOptimParProduct ----------------------------------------------------------------------------------------
# The final optimal coefficient for every degree are the product of every iteration.
vecOptimParProduct <- rep(1, numConCoef)
for (i in 1:numIte) {
    for (j in 1:numConCoef) {
        vecOptimParProduct[j] <- vecOptimParProduct[j] * matOptimPar[i, j]
    }
}
cat("################################################################################\n") ##############################
cat("#### 4/6,  SALR Model and Centered Wind Speed ##################################\n")
## 4.1,  Center the wind speed using optimal par from wind direction model
if (wheFurIte) {
    datfTrain$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfTrain, numConCoef)
    datfPred$speed.center <- updateWindSpeedCenter(vecOptimParProduct, datfPred, numConCoef)
} else {
    datfTrain$speed.center <- updateWindSpeedCenter(vecOptimPar, datfTrain, numConCoef)
    datfPred$speed.center <- updateWindSpeedCenter(vecOptimPar, datfPred, numConCoef)
}
## 4.2,  Kernal for Adaptive Seasonal LR
kernalSeasonPred <- numTrain + deltaKernalSeasonPred  # [kernal of seasonal adaptive local regression]
cat("kernalSeasonPred = ", kernalSeasonPred, "\n", sep = "")
matWeightSeasonPred <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeasonPred)
## 4.3,  Calculate Kernal value
vecKernalValuePred <- calVecKernalValue(matWeightSeasonPred, datfTrain)
if (wheOutput) {
    outputResult(vecKernalValuePred, OutputSeries)
}
cat("--------------------------------------------------------------------------------\n")
cat("Final: vecKernalValuePred = [", paste(vecKernalValuePred, collapse = ", "), "]\n", sep = "")
cat("################################################################################\n") ##############################
cat("#### 5/6,  Prediction ##########################################################\n")
vecPowerPred <- predLocalReg(datfPred$speed.center, vecKernal, vecKernalValuePred)
if (wheOutput) {
    outputResult(vecPowerPred, OutputSeries)
}
if (wheVali) {
    rmse <- calPredictionRMSE(vecPowerPred, datfVali$power)
    cat("Final: RMSE = ", rmse, "\n", sep = "")
}
cat("#### Calculation End ###########################################################\n") ##############################
