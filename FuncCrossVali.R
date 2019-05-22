# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Prediction
# author: Edward J. Xu
# date: May 22th, 2019
########################################################################################################################
calPredictionRMSE <- function(vecPred, vecValid){
    lengthNoNa <- sum(!is.na(vecPred), na.rm = T)  # Because we don't count the NA value in the following calculation,
    # the length of the vecPred must minus the NA prediction. Usually, there is no NA in prediction.
    rmse <- sqrt(sum((vecValid - vecPred)^2, na.rm = TRUE) / lengthNoNa) * 100  # % must be used
    return(rmse)
}
########################################################################################################################
## 2,  Functions for cross-validation of seasonal adaptive local regression model
crossValid <- function(vecKernal, listVecKernalValue, datf = datfTrain, numFold = 10){
    vecRootMeanSquaredError <- rep(NA, numFold)
    for (i in 1:numFold) {
        datForVali <- datf[(datf$index == i),]
        vecPowerPred <- predLocalReg(datForVali$speed.center, vecKernal, listVecKernalValue[[i]])
        vecRootMeanSquaredError[i] <- calPredictionRMSE(vecPowerPred, datForVali$power)
    }
    aveRootMeanSquaredError <- sum(vecRootMeanSquaredError) / numFold  # [mean of mse]
    return(aveRootMeanSquaredError)
}
