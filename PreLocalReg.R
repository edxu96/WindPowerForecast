# DTU31761A3: Wind Power Output Prediction using Regression
# 1/6,  vecKernal and matWeight for Local Regression
# author: Edward J. Xu
# date: May 22th, 2019
## 1.1,  Kernal of local regression
vecKernal <- c(seq(0, 0.79, by = 0.8 / 22), seq(0.8, 1.0, by = 0.2 / 2))
# Longer span is more robust to bad-behaved data
cat("vecKernal = [", paste(vecKernal, collapse = ", "), "]\n", sep = "")
numKernal <- length(vecKernal)
## 1.2,  Calculate weight matrix
matWeight <- matrix(NA, nrow = numTrain, ncol = numKernal)
for (i in 1:numKernal) {
    matWeight[,i] <- calVecWeightLocalGaussian(0.05, datfTrain$speed.center, vecKernal[i], numTrain)
    # numRow of matWeight is numTrain
    # numCol of matWeight is numKernal
}
