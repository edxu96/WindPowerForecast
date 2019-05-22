# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Prediction
# author: Edward J. Xu
# date: May 22th, 2019
########################################################################################################################
## 2,  Functions for cross-validation of seasonal adaptive local regression model
crossValid <- function(vecKernal, listVecKernalValue, datf = datfTrain, numFold = 10){
    vecSquaredError <- rep(NA, numFold)
    for (i in 1:numFold) {
        datForTest <- datf[(datf$index == i),]
        vecPowerPred <- predLocalReg(datForTest$speed.center, vecKernal, listVecKernalValue[[i]])
        vecSquaredError[i] <- sum((datForTest$power - vecPowerPred)^2, na.rm = TRUE)  # [squared (prediction) error]
    }
    meanSquaredError <- sum(vecSquaredError) / numFold  # [mean squared (prediction) error]
    return(meanSquaredError)
}
########################################################################################################################
calPredictionRMSE <- function(vecPred, vecValid){
    rmse <- sqrt(sum((vecValid - vecPred)^2, na.rm = TRUE) / length(vecPred)) * 100
    return(rmse)
}
