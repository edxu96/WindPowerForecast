# DTU31761A3: Wind Power Output Prediction using Regression
# author: Edward J. Xu
# date: May 22th, 2019
# version: 3.0
########################################################################################################################
# setwd("~/Documents/Github/WindPowerPrediction")
rm(list = ls())
library(lubridate)
########################################################################################################################
cat("#### 0,  Control Parameters, Data and Functions ################################\n") ##############################
## Control parameters 1
numFold      <- 10        # [number of folds for cross validation]
numIte       <- 3         # [number of further iterations]
wheFurIte    <- F         # [whether do further iterations]
outputSeries <- 4         # [series number of the output file]
wheOutput     <- T        # [whether to output the results]
wheVali      <- F         # [whether to validate the result]
numConCoef   <- 360
## Name of the data files
# data for validation is the tail data in training data in next session
strNameTrain <- "Data/TrainData3.csv"
strNamePred  <- "Data/WeatherForecastInput4.csv"
strNameVali  <- "Data/TrainData4.csv"
source("FuncCrossVali.R")
source("FuncLocalReg.R")
source("FuncSeasonAdap.R")
source("FuncWindDirec.R")
source("FuncOutput.R")
source("Data.R")
## Control parameters 2
kernalSeason <- numTrain  # [kernal of seasonal adaptive local regression]
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
cat("#### 5/6,  Prediction ##########################################################\n")
vecPred <- predLocalReg(datfPred$speed.center, vecKernal, vecKernalValue)
if (wheOutput) {
    outputResult(vecPred, OutputSeries)
}
if (wheVali) {
    rmse <- calPredictionRMSE(vecPred, datfVali$power)
    cat("Final: RMSE = ", rmse, "\n", sep = "")
}
cat("#### Calculation End ###########################################################\n") ##############################
