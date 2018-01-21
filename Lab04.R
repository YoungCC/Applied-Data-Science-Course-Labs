rm(list =ls())

library(ISLR)

#========================================
# Author : Yang Cao
# Date: October 5, 2017
# Purpose: Demostrate logistic regression
#========================================

attach(Default)
summary(Default)


#Logistic regression with one quantitative predictor
m.balance <- glm(default ~ balance, family = binomial)
summary(m.balance)

pred.data <- data.frame(balance=c(1000, 2000, 3000))
pred.data
predict(m.balance, pred.data, type="response")
pred.data <- data.frame(balance=c(1000, 2000, 3000, 4000, 5000, 6000))
summary(balance)
hist(balance)

# Logistic regression with multiple predictors
m <- glm(default ~ balance + income + student, family = binomial)
summary(m)
pred.data <- data.frame(balance=c(1000, 2000, 3000), income=1000, student="Yes")
predict(m, pred.data, type="response")
pred.data
pred.data <- data.frame(balance=c(min(balance), mean(balance), max(balance)), income=mean(income), student="Yes")
pred.data <- data.frame(balance=seq(1000, 10000, 1000), income=1000, student="Yes")

# Classification
pred.probs <- predict (m, type = "response")

pred.default <- rep("No", nrow(Default))
pred.default[pred.probs > 0.5] <- "Yes"

confusion.matrix <- table(default, pred.default)
addmargins(confusion.matrix)

