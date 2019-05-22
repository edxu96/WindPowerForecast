# DTU31761A3: Wind Power Output Prediction using Regression
# author: Edward J. Xu
# date: May 22th, 2019
# version: 3.3
# setwd("~/Documents/Github/WindPowerPrediction")
########################################################################################################################
rm(list = ls())
library(lubridate)
########################################################################################################################
cat("#### 0,  Control Parameters, Data and Functions ################################\n") ##############################
## 0.1,  Control Parameters 1
numFold      <- 10   # [number of folds for cross validation]
numIte       <- 1    # [number of further iterations] if = 1, there is no further iteration to optimize the coeffcients.
outputSeries <- 7    # [series number of the output file]
wheOutput    <- T    # [whether to output the results]
wheVali      <- F    # [whether to validate the result]
numConCoef   <- 360  # [number of concentration coefficients]
if (numIte > 1) {
    wheFurIte <- T
} else {
    wheFurIte <- F    # [whether do further iterations]
}
## 0.2, Name of the data files
strNameTrain <- "Data/TrainData4.csv"
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
deltaKernalSeasonPred <- numPred / 2  # will set the center of prediction period as the main season.
# [forward value of kernalSeasonPred] If 10, means kernalSeasonPred = numTrain + 10.
cat("################################################################################\n") ##############################
cat("#### 1/6,  vecKernal and matWeight for Local Regression ########################\n")
source("PreLocalReg.R")
cat("################################################################################\n") ##############################
cat("#### 2/6,  Prepare Seasonal Adaptive Models ####################################\n")
source("PreSeasonAdap.R")
cat("################################################################################\n") ##############################
cat("#### 3/6,  Cross Validation to Find Optimal Con-Coef for Wind Direction ########\n")
cat("---- 3.1,  Benchmark without Con-Coef ------------------------------------------\n")
mrmseBenchmark <- crossValid(vecKernal, listVecKernalValue, datfTrain, 10)
cat("mrmseBenchmark =", mrmseBenchmark, "\n")
cat("---- 3.2,  First Iteration -----------------------------------------------------\n")
listResult <- optimWindDirection(1, listVecKernalValue, vecKernalSeason, numConCoef, datfTrain)
vecCoef <- listResult$par
vecObj <- listResult$obj
rm(listResult)
if (wheOutput) {
    outputResult(vecCoef, outputSeries)
}
cat("aveImprove = ", (sqrt(sum((vecObj - mrmseBenchmark)^2)) / numConCoef * 100), "%\n", sep = "")
# The calculation of averaged improvement is the same as mse
# cat("vecCoef = [", paste(vecCoef, collapse = ", "), "]\n", sep = "")  # It's too long to print
# cat("vecObj = [", paste(vecObj, collapse = ", "), "]\n", sep = "")  # It's too long to print
cat("--------------------------------------------------------------------------------\n") # ----------------------------
# 3.2,  Further Iterations
if (wheFurIte) {
    cat("---- 3.3,  Further Iterations --------------------------------------------------\n")
    matCoef <- matrix(1, nrow = numIte, ncol = numConCoef)
    matObj <- matrix(1, nrow = numIte, ncol = numConCoef)
    matCoef[1,] <- vecCoef
    matObj[1,] <- vecObj
    for (ite in 2:numIte) {
        cat("----", ite, "-th Iteration ---------------------------------------------------\n", sep = "")
        # The speed.center should be updated before every further iteration
        datfTrain$speed.center <- updateWindSpeedCenter(matCoef[(ite - 1),], datfTrain, numConCoef)
        datfPred$speed.center <- updateWindSpeedCenter(matCoef[(ite - 1),], datfPred, numConCoef)
        listResult <- optimWindDirection(ite, listVecKernalValue, vecKernalSeason, numConCoef, datfTrain)
        matCoef[ite, 1:length(listResult$par)] <- listResult$par
        matObj[ite, 1:length(listResult$obj)] <- listResult$obj
        cat("aveImprove = ", (sqrt(sum((matObj[ite] - mrmseBenchmark)^2)) / numConCoef * 100), "%\n", sep = "")
        # cat("vecCoef = [", paste(matCoef[ite,], collapse = ", "), "]\n", sep = "")  # It's too long to print
        cat("--------------------------------------------------------------------------------\n")
    }; rm(listResult, ite)
    if (wheOutput) {
        outputResult(matCoef, outputSeries)
    }
    ## 3.3,  Get vecCoefProduct ----------------------------------------------------------------------------------------
    # The final optimal coefficient for every degree are the product of every iteration.
    vecCoefProduct <- rep(1, numConCoef)
    for (i in 1:numIte) {
        for (j in 1:numConCoef) {
            vecCoefProduct[j] <- vecCoefProduct[j] * matCoef[i, j]
        }
    }
}
cat("################################################################################\n") ##############################
cat("#### 4/6,  SALR Model and Centered Wind Speed ##################################\n")
## 4.1,  Center the wind speed using optimal par from wind direction model
if (wheFurIte) {
    datfTrain$speed.center <- updateWindSpeedCenter(vecCoefProduct, datfTrain, numConCoef)
    datfPred$speed.center <- updateWindSpeedCenter(vecCoefProduct, datfPred, numConCoef)
} else {
    datfTrain$speed.center <- updateWindSpeedCenter(vecCoef, datfTrain, numConCoef)
    datfPred$speed.center <- updateWindSpeedCenter(vecCoef, datfPred, numConCoef)
}
## 4.2,  Kernal for Adaptive Seasonal LR
kernalSeasonPred <- numTrain + deltaKernalSeasonPred  # [kernal of seasonal adaptive local regression]
cat("kernalSeasonPred = ", kernalSeasonPred, "\n", sep = "")
matWeightSeasonPred <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeasonPred)
## 4.3,  Calculate Kernal value
vecKernalValuePred <- calVecKernalValue(matWeightSeasonPred, datfTrain)
if (wheOutput) {
    outputResult(vecKernalValuePred, outputSeries)
}
cat("--------------------------------------------------------------------------------\n")
cat("vecKernalValuePred = [", paste(vecKernalValuePred, collapse = ", "), "]\n", sep = "")
cat("################################################################################\n") ##############################
cat("#### 5/6,  Prediction ##########################################################\n")
vecPowerPred <- predLocalReg(datfPred$speed.center, vecKernal, vecKernalValuePred)
if (wheOutput) {
    outputResult(vecPowerPred, outputSeries)
}
if (wheVali) {
    rmse <- calPredictionRMSE(vecPowerPred, datfVali$power)
    cat("rootMeanSquaredError = ", rmse, "%\n", sep = "")
}
cat("#### Calculation End ###########################################################\n") ##############################
