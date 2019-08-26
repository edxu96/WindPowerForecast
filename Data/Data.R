# DTU31761A3: Wind Power Output Prediction using Regression
# Get Data for Training, Prediction and Validation
# author: Edward J. Xu
# date: May 20th, 2019
########################################################################################################################
source("FuncData.R")
########################################################################################################################
## 1,  Data for Training
datfTrain <- frameDataTrain(strNameTrain, numConCoef)
numTrain <- length(datfTrain$series)
datfTrain["index"] <- setIndexCrossVali(numTrain, numFold)  # Set index for datfTrain according to fold
datfTrain["speed.norm"] <- calNorminalSpeed(0.99, datfTrain) / 35
datfTrain["speed.center"] <- datfTrain$speed.norm
# datfTrain["speed.2"] <- datfTrain$speed.norm^2
# datfTrain["speed.3"] <- datfTrain$speed.norm^3
########################################################################################################################
## 2,  Data for Prediction
datfPred <- frameDataPred(strNamePred, numConCoef)
numPred <- length(datfPred$series)
datfPred["speed.norm"] <- calNorminalSpeed(0.99, datfPred) / 35
datfPred["speed.center"] <- datfPred$speed.norm
# datfPred["speed.2"] <- datfPred$speed.norm^2
# datfPred["speed.3"] <- datfPred$speed.norm^3
########################################################################################################################
## 3,  Data for Validation
if (wheVali) {
    # data for validation is the tail data in training data in next session
    datfVali <- tail(frameDataTrain(strNameVali, numConCoef), 24 * 28)
    numVali <- length(datfVali$series)
    cat("Whether lengths of prediction and validation date are the same?", (numVali == numPred), "\n")
    datfVali["speed.norm"] <- calNorminalSpeed(0.99, datfVali) / 35
    datfVali["speed.center"] <- datfVali$speed.norm
}
rm(frameDataTrain, frameDataPred, calNorminalSpeed)
