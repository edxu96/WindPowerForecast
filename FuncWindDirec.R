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
    meanSquaredError <- crossValid(vecKernal, listVecKernalValue, datf, numFold)
    return(meanSquaredError)
}
#' Function to calculate the vecOptimPar
optimWindDirection <- function(ite, listVecKernalValue, vecKernalSeason, numConCoef = 360, datf = datfTrain){
    vecOptimPar <- rep(1.0, numConCoef)
    vecOptimObj <- rep(NA, numConCoef)
    for (i in 1:numConCoef) {
        resultOptim <- optimize(validateCoefWindDirec, position = i, vecKernal = vecKernal,
            listVecKernalValue = listVecKernalValue, numFold = numFold, datf = datfTrain, lower = 0.6, upper = 1.1)
        vecOptimPar[i] <- resultOptim$minimum
        vecOptimObj[i] <- resultOptim$objective
        cat(ite, "-th Iteration. at ", i, ", optimPar = ", vecOptimPar[i], ", optimObj = ",
            vecOptimObj[i], "\n", sep = "")
        # cat("--------------------------------------------------------------------------------\n")
    }
    return(listResult = list(par = vecOptimPar, obj = vecOptimObj))
}
