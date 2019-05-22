# DTU31761A3: Wind Power Output Prediction using Regression
# 1/6,  vecKernal and matWeight for Local Regression
# author: Edward J. Xu
# date: May 22th, 2019
## 1.1,  Kernal of local regression
vecKernal <- rep(NA, 25)
vecKernal[1:21] <- seq(0, 0.7, by = 0.7 / 20)
vecKernal[21:25] <- seq(0.7, 1.2, by = 0.5 / 4)
cat("vecKernal = [", paste(vecKernal, collapse = ", "), "]\n", sep = "")
numKernal <- length(vecKernal)
## 1.2,  Calculate weight matrix
matWeight <- matrix(NA, nrow = numTrain, ncol = numKernal)
for (i in 1:numKernal) {
    matWeight[,i] <- calVecWeightLocalGaussian(0.05, datfTrain$speed.center, vecKernal[i], numTrain)
    # numRow of matWeight is numTrain
    # numCol of matWeight is numKernal
}
