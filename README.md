# WindPowerPrediction
Wind Power Prediction with ECMWF Data as Input using Statistical Learning.

## 1. Introduction

## 2. Local Regression of Wind Speed

The relationship between wind speed and wind power out is non-linear.

![Local Regression of Wind Speed and Power Output](/Image/103.png)

The spans between data points with nominal wind speed lower than 0.7 are shorter than those greater than 0.7. Short spans can model more sensitive relationship, while longer spans are more robust to bad-behaved data.

## 3. Seasonal Adaptive Model

The relationship is changing. It may change gradually or seasonally. Different kernals for adaptive model and weight function can be used.

The easiest weight function is to use the sliding window. The observations outside the time window are not accounted.

![Adaptive Local Regression with Sliding Window of Wind Speed and Power Output](/Image/102.png)

The seasonal effect on the relationship can also be very significant.

![Seasonal Adaptive Local Regression of Wind Speed and Power Output](/Image/101.png)

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

![Wind Rose Diagram of Wind Speed all around the Wind Farm](/Image/104.png)
