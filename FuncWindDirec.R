# DTU31761A3: Wind Power Output Prediction using Regression
# Functions for Coef of Wind Direction
# author: Edward J. Xu
# date: May 20th, 2019
########################################################################################################################
updateWindSpeedCenter <- function(vecPar, dat){
    for (i in 1:360) {
        dat$speed.center[dat$degree100 == i] <- dat$speed.center[dat$degree100 == i] * vecPar[i]
    }
    return(dat$speed.center)
}
########################################################################################################################
validateCoefWindDirec <- function(coef, position, vecKernal, listVecKernalValue, numFold = numFold, dat = datfTrain){
    dat$speed.center[dat$degree100 == position] <- dat$speed.center[dat$degree100 == position] * coef
    meanSquaredError <- crossValid(dat, vecKernal, listVecKernalValue, numFold)
    return(meanSquaredError)
}
#' Function to calculate the vecOptimPar
optimWindDirection <- function(ite, listVecKernalValue, vecKernalSeason, dat = datfTrain){
    vecOptimPar <- rep(1.0, 360)
    vecOptimObj <- rep(NA, 360)
    for (i in 1:360) {
        resultOptim <- optimize(validateCoefWindDirec, position = i, vecKernal = vecKernal,
            listVecKernalValue = listVecKernalValue, numFold = numFold, dat = datfTrain, lower = 0.6, upper = 1.1)
        vecOptimPar[i] <- resultOptim$minimum
        vecOptimObj[i] <- resultOptim$objective
        cat(ite, "-th Iteration. at ", i, ", optimPar = ", vecOptimPar[i], ", optimObj = ",
            vecOptimObj[i], "\n", sep = "")
        # cat("--------------------------------------------------------------------------------\n")
    }
    return(listResult = list(par = vecOptimPar, obj = vecOptimObj))
}
