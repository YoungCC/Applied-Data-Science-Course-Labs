rm(list = ls())

#============================================
# Author: Yang Cao
# Date: Sep.21, 2017
# Purpose: Demonstrate linear regression in R
#============================================

# Load the MASS and ISLR libraries, which contain data sets and functions we need
library(MASS)
library(ISLR)

# Look at the content of the Boston data set
View(Boston)
dim(Boston)
names(Boston)
head(Boston)

# Data inspection
summary(Boston)
summary(Boston$lstat)

# Use attach to load the data set into the workspace. This way you can call columns in the data set directly without using the “$” operator
attach(Boston)
summary(lstat)
par(mfrow=c(1,1))
plot(lstat, medv)
boxplot(lstat)
boxplot(medv)
par(mfrow=c(1,2))
boxplot(lstat, main="Socioeconomic status")
boxplot(medv, main="Median house value")
#par(mfrow=c(1,2))
hist(medv)
hist(lstat)
par(mfrow=c(1,1))

# Supervised learning with linear regression
lm(medv~lstat)
lm.fit <- lm(medv~lstat)
print(lm.fit)
summary(lm.fit)

# Interpretation: For a one percentage points increase in low socioeconomic status of a market, 
# median house value decreases on average by $950.
# estimate 95% confidence intervals
confint(lm.fit)

# Visualize results
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)
plot(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)

abline(2,3)
abline(lm.fit, col="red",lw =3)

# Prediction 'by hand' for lstat ={5, 10, 15}
x <- c(5,10,15)
y <- 34.55 + (-0.95*x)
y <- 34.55 + (-0.95*5)
y <- coef(lm.fit)[1] + (coef(lm.fit)[2]*x) 

# Automated predition 
predict(lm.fit,data.frame(lstat = c(5,10,15)))
predict(lm.fit,data.frame(lstat = c(50,100,150)))
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
