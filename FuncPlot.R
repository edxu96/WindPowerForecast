# DTU31761A3: Wind Power Output Prediction using Regression
# Functions to ouput the result
# author: Edward J. Xu
# date: May 22th, 2019
########################################################################################################################
plotVecWeight <- function(vecWeight, vecWeightSeason){
    numTrain <- length(vecWeight)
    plot(seq(1, numTrain), vecWeight, xlab = 'Series', ylab = 'Weight Value', bty = "n", col = "lightgrey")
    points(vecKernal, vecKernalValue_2, col = "blue", lwd = 2, type = "b", pch = 18)
    title(main = "Wind Power Forecast Training Data and Adaptive Model with Sliding SliWin")
    legend("bottomright", inset = .02, legend = c("Training Data", "Current Model", "One Year Back", "Two Years Back"),
        col = c("lightgrey", "blue", "red", "forestgreen"), pch = c(1, 18, 18, 18), lty = c(NA, 1, 2, 3),
        lwd = c(NA, 2, 2, 2))
}
