# DTU31761A3: Wind Power Output Prediction using Regression
# Functions to ouput the result
# author: Edward J. Xu
# date: May 20th, 2019
outputListVecKernalValue <- function(listVecKernalValue){
    matKernalValue <- matrix(NA, ncol = 10, nrow = 25)
    for (i in 1:10) {
        matKernalValue[,i] <- listVecKernalValue[[i]]
    }
    write.table(matKernalValue, file = "Output/MatKernalValue_1.csv", sep = ",",  dec = ".", row.names = F,
                col.names = F, quote = FALSE)
}
outputVecOptimPar <- function(vecOptimPar){
    write.table(vecOptimPar, file = "Output/vecOptimPar_1.csv", sep = ",",  dec = ".", row.names = F,
                col.names = F, quote = FALSE)
}
outputVecKernalValue <- function(vecKernalValue){
    write.table(vecKernalValue, file = "Output/vecKernalValue_1.csv", sep = ",",  dec = ".", row.names = F,
                col.names = F, quote = FALSE)
}
outputMatOptimPar <- function(matOptimPar){
    write.table(matOptimPar, file = "Output/MatOptimPar_3.csv", sep = ",",  dec = ".", row.names = F,
                col.names = F, quote = FALSE)
}
