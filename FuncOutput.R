# DTU31761A3: Wind Power Output Prediction using Regression
# Functions to ouput the result
# author: Edward J. Xu
# date: May 21th, 2019
########################################################################################################################
#' To output the result in a csv with a specific name
outputResult <- function(result, outputSeries = 1){
    strFileName <- paste("Output/", deparse(substitute(test)), "_", outputSeries, ".csv", sep = "")
    write.table(result, file = strFileName, sep = ",",  dec = ".", row.names = F,
                col.names = F, quote = FALSE)
}
#' To convert a list of vectors in same length to a matrix, and then output
outputlistVec <- function(listVec, outputSeries = 1){
    numList <- length(listVec)
    num <- length(listVec[[1]])
    mat <- matrix(NA, col = numList, nrow = num)
    for (i in 1:numList) {
        mat[,i] <- listVec[[i]]
    }
    outputResult(mat, outputSeries)
}
