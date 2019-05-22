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
optimWindDirection <- function(ite, listVecKernalValue, vecKernalSeason, numConCoef = 360, datf = datfTrain){
    vecCoef <- rep(1.0, numConCoef)
    vecObj <- rep(NA, numConCoef)
    for (i in 1:numConCoef) {
        resultOptim <- optimize(validateCoefWindDirec, position = i, vecKernal = vecKernal,
            listVecKernalValue = listVecKernalValue, numFold = numFold, datf = datfTrain, lower = 0.6, upper = 1.1)
        vecCoef[i] <- resultOptim$minimum
        vecObj[i] <- resultOptim$objective
        cat(ite, "-th Iteration. at ", i, ", optimPar = ", vecCoef[i], ", optimObj = ",
            vecObj[i], "\n", sep = "")
        # cat("--------------------------------------------------------------------------------\n")
    }
    return(listResult = list(par = vecCoef, obj = vecObj))
}
