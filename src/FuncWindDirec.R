# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Coef of Wind Direction
# author: Edward J. Xu
# date: May 22th, 2019
########################################################################################################################
updateWindSpeedCenter <- function(vecPar, datf, numConCoef = 360){
    for (i in 1:numConCoef) {
        datf$speed.center[datf$degree100 == i] <- datf$speed.center[datf$degree100 == i] * vecPar[i]
    }
    # strNameDatf = deparse(substitute(datf))
    # cat("The wind speed in ", strNameDatf, " is centered.\n", sep = "")  # It always prints all the contents
    cat("The wind speed is centered.\n", sep = "")
    return(datf$speed.center)
}
########################################################################################################################
validateCoefWindDirec <- function(coef, position, vecKernal, listVecKernalValue, numFold = numFold, datf = datfTrain){
    datf$speed.center[datf$degree100 == position] <- datf$speed.center[datf$degree100 == position] * coef
    meanRootMeanSquaredError <- crossValid(vecKernal, listVecKernalValue, datf, numFold)
    return(meanRootMeanSquaredError)
}
#' Function to calculate the vecCoef
optimWindDirection <- function(listVecKernalValue, vecKernalSeason, numConCoef = 360, datf = datfTrain){
    # cat(listVecKernalValue[[1]])
    vecCoef <- rep(1.0, numConCoef)
    vecObj <- rep(NA, numConCoef)
    lenInterval <- 360 / numConCoef
    invLenInterval <- 1 / lenInterval
    for (i in 1:numConCoef) {
        resultOptim <- optimize(validateCoefWindDirec, position = i, vecKernal = vecKernal,
            listVecKernalValue = listVecKernalValue, numFold = numFold, datf = datfTrain, lower = 0.6, upper = 1.1)
        vecCoef[i] <- resultOptim$minimum
        vecObj[i] <- resultOptim$objective
        if (i != numConCoef) {
            cat("[", (i / invLenInterval - lenInterval/2), ", ", (i / invLenInterval + lenInterval/2), "), coef = ",
                vecCoef[i], ", obj = ", vecObj[i], "\n", sep = "")
        } else {  # If i = 360, the half interval after i is 0 +
            cat("[", (360 - lenInterval/2), ", ", (0 + lenInterval/2), "), coef = ", vecCoef[i], ", obj = ", vecObj[i],
                "\n", sep = "")
        }
        # cat("--------------------------------------------------------------------------------\n")
    }
    return(listResult = list(par = vecCoef, obj = vecObj))
}
