# WindPowerPrediction

Wind Power Prediction using Statistical Learning with ECMWF Data as Input.

## 1. Introduction

Because of many non-linear factors, the modelling of wind power output of a wind farm and ECMWF data is difficult. Based on the relationship, we can predict the wind power output using ECMWF forecast of the future wind. Then, the prediction can be used in the trading in electricity market. With more and more penetration of wind power in electricity production, the modelling and prediction becomes more and more essential. Regression and tree-based statistical learning methods are used in this project, which are both powerful and easy to interpret.

If there are ready-made data in `Output` folder, the file `sub.R` can be used to predict directly.

## 2. Local Regression of Wind Speed

The relationship between wind speed and wind power out is non-linear.

![Local Regression of Wind Speed and Power Output](./images/103.png)

The spans between data points with nominal wind speed lower than 0.7 are shorter than those greater than 0.7. Short spans can model more sensitive relationship, while longer spans are more robust to bad-behaved data.

## 3. Seasonal Adaptive Model

The relationship is changing. It may change gradually or seasonally. Different kernals for adaptive model and weight function can be used.

The easiest weight function is to use the sliding window. The observations outside the time window are not accounted.

![Adaptive Local Regression with Sliding Window of Wind Speed and Power Output](./images/102.png)

The seasonal effect on the relationship can also be very significant.

![Seasonal Adaptive Local Regression of Wind Speed and Power Output](./images/101.png)

## 4. Tree-Based Concentration Coefficient of Wind Direction

Wind from different directions can make a difference in the wind power output. A tree with 360 leaves representing every degree all around the wind farm.

```R
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
```

![Wind Rose Diagram of Wind Speed all around the Wind Farm](./images/104.png)

### 4.2 Concentrate the Wind Speed

The wind speed in training data and prediction data can be concentrated to make more accurate prediction using the following function.

```R
updateWindSpeedCenter <- function(vecPar, dat){
    for (i in 1:360) {
        dat$speed.center[dat$degree100 == i] <- dat$speed.center[dat$degree100 == i] * vecPar[i]
    }
    return(dat$speed.center)
}
```
