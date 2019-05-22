# DTU31761A3: Wind Power Output Prediction using Regression
# 2/6,  Prepare Seasonal Adaptive Model
# author: Edward J. Xu
# date: May 22th, 2019
## 1.1,  Kernal of local regression
cat("#### 2.1/6,  kernalSeason and matWeightSeason for Cross Validation #############\n")
vecKernalSeason <- rep(NA, numFold)  # Vector of kernals for seasonal adaptive local regression
listMatWeightSeason <- vector("list", numFold)
for (i in 1:numFold) {
    minSeriesVali <- min(datfTrain[(datfTrain$index == i),]$series)  # [beginning of the validation period]
    maxSeriesVali <- max(datfTrain[(datfTrain$index == i),]$series)  # [end of the validation period]
    vecKernalSeason[i] <- (minSeriesVali + maxSeriesVali) / 2  # Set the center time of validation period as the kernal
    listMatWeightSeason[[i]] <- calMatWeightSeasonGaussian(matWeight, datfTrain$series, vecKernalSeason[i])
    listMatWeightSeason[[i]][minSeriesVali:maxSeriesVali,] <- 0
}
cat("vecKernalSeason = [", paste(vecKernalSeason, collapse = ", "), "]\n", sep = "")
cat("--------------------------------------------------------------------------------\n")
cat("#### 2.2/6,  Calculate kernal value for every kernalSeason #####################\n")
# Get the local regression model for every fold in cross-validation
listVecKernalValue <- vector("list", numFold)
for(i in 1:numFold) {
    cat("---- Calculate kernal value for ", i, "-th kernalSeason -----------------------------\n", sep = "")
    listVecKernalValue[[i]] <- calVecKernalValue(listMatWeightSeason[[i]], datfTrain)
    cat(i, "-th vecKernalValue = [", paste(listVecKernalValue[[i]], collapse = ", "), "]\n", sep = "")
    cat("--------------------------------------------------------------------------------\n")
}
if (wheOutput) {
    outputlistVec(listVecKernalValue, OutputSeries)
}
