rm(list = ls())

#====================================================
# Author : Yang Cao
# Date: September 28, 2017
# Purpose: Demostrate multiple linear regression in R
#====================================================

library(MASS)
attach(Boston)

# Fit simple linear regression
m1 <- lm(medv ~ lstat)
summary(m1)
coef(m1)

# Fit multiple linear regression
m2 <- lm(medv ~ lstat + rm + age)
summary(m2)
coef(m2)

predict(m2, data.frame(rm = c(4,6,8), lstat = mean(lstat), age = mean(age)), interval = "confidence")
predict(m2, data.frame(rm = c(4,6,8), lstat = mean(lstat), age = mean(age)), interval = "prediction")
predict(m2, data.frame(rm = c(min(rm), mean(rm), max(rm)), lstat = mean(lstat), age = mean(age)), interval="confidence")

## Exercise
# (1.) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)

# (2.) Compute the matrix of correlations between the variables using the function "cor()". You will need to exclude the "name" variable, which is qualitative.
cor(subset(Auto, select = -name))

# (3.) Use the "lm()" function to perform a multiple linear regression with "mpg" as the response and all other variables except "name" as the predictors. Use the "summary()" function to print the results. Comment on the output. For instance:
m <-  lm(mpg ~ .-name, data=Auto)
summary(m)

# (a.) Is there a relationship between the predictors and the response?
# -> Yes, there is a relationship between the predictors and the response by testing the null hypothesis of whether all the regression coefficients are zero. The F-statistic is far from 1 (with a small p-value), indicating evidence against the null hypothesis.

# (b.) Which predictors appear to have a statistically significant relationship to the response?
# -> Looking at the p-values associated with each predictor's t-statistic, we see that displacement, weight, year, and origin have a statistically significant relationship, while cylinders, horsepower, and acceleration do not.**

# (c.). What does the coefficient for the "year" variable suggest?
# -> The regression coefficient for year is 0.75, This suggests that for every additional year, mpg increases by the 0.75. In other words, cars become more fuel efficient every year by almost 1 mpg.

# (4.) Use the "plot()" function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
par(mfrow=c(2,2))
plot(m)
par(mfrow=c(1,1))
# -> Seems to be non-linear pattern, linear model not the best fit. From the leverage plot, point 14 appears to have high leverage, although not a high magnitude residual.**

