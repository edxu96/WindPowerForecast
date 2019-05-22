# DTU31761A3: Wind Power Output Prediction using Regression
# Test the adaptive seasonal local regression model
# author: Edward J. Xu
# date: May 22th, 2019
setwd("~/Desktop/WindPowerForecast_DTU31761A3_EDXU")
rm(list = ls())
library(lubridate)
source("Data.R")
source("FuncCrossVali.R")
source("FuncLocalReg.R")
source("FuncSeasonAdap.R")
source("FuncWindDirec.R")
cat("################################################################################\n") ##############################
cat("######## 1,  vecKernal and matWeight for Local Regression ########\n")
source("PreLocalReg.R")
cat("################################################################################\n") ##############################
cat("######## 2,  Center the Data according to Wind Direction ########\n")
vecOptimPar <- rep(1.0, 360)
vecOptimPar[1:359] <- read.csv("Data/VecOptimPar_1.csv")[,1]
datfTrain$speed.center <- updateWindSpeedCenter(vecOptimPar, datfTrain, numConCoef)
datfPred$speed.center <- updateWindSpeedCenter(vecOptimPar, datfPred, numConCoef)
cat("The wind speed is centered.\n")
cat("################################################################################\n") ##############################
cat("######## 3,  kernalSeason and vecKernalValue for Adaptive Seasonal LR ########\n")
## 2.1,  Kernal for Adaptive Seasonal LR
kernalSeason <- numTrain  # Kernal of adaptive seasonal local regression
cat("kernalSeason = ", kernalSeason, "\n", sep = "")
matWeightSeason <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, kernalSeason)
## 2.2,  Calculate kernal value
vecKernalValue <- calVecKernalValue(matWeightSeason, datfTrain)
cat("--------------------------------------------------------------------------------\n")
# outputVecKernalValue(vecKernalValue)
cat("vecKernalValue = [", paste(vecKernalValue, collapse = ", "), "]\n", sep = "")
cat("################################################################################\n") ##############################
cat("######## 4,  Prediction ########\n")
vecPredBenchMark <- predLinearInter(datfPred$speed.norm, vecKernal, vecKernalValue)
rmseBenchMark <- calPredictionRMSE(vecPredBenchMark, datfVali$power)
cat("rmseBenchMark = ", rmseBenchMark, "\n", sep = "")
##
vecPred <- predLinearInter(datfPred$speed.center, vecKernal, vecKernalValue)
# rmse <- calPredictionRMSE(vecPred, datfVali$power)
# cat("rmse = ", rmse, "\n", sep = "")
outputResult(vecPred, OutputSeries)
