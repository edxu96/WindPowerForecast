# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Prediction
# author: Edward J. Xu
# date: May 20th, 2019
########################################################################################################################
## 2,  Functions for cross-validation of seasonal adaptive local regression model
setIndexCrossVali <- function(num, numFold){
    # Index in 10 fold for cross validation
    numPerFold <- round(num / numFold)
    vecIndex <- rep(numFold, num)
    for (i in 1:(numFold - 1)) {
        vecIndex[(numPerFold * (i - 1) + 1):(numPerFold * i)] <- rep(i, numPerFold)
    }
    return(vecIndex)
}
crossValid <- function(dat, vecKernal, listVecKernalValue, numFold = 10){
    vecSquaredError <- rep(NA, numFold)
    for (i in 1:numFold) {
        datForTest <- dat[(dat$index == i),]
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
