rm(list =ls())

library(ISLR)

#========================================
# Author : Yang Cao
# Date: October 10, 2017
# Purpose: Demonstrate resampling methods
#========================================

attach(Default)
summary(Default)
set.seed(42)

# Using the sample() function, split the set of observations into a training set and a validation set
N <- nrow(Default)
train <- sample(N, N/2)
length(train)

# Estimate a logistic regression on the training data
glm.fit <- glm(default ~ balance + income, family = binomial, subset = train)
glm.fit2 <- glm(default ~ balance + income, data = Default[train,], family = binomial)

# Estimate predicted probabilities for each observation
pred.probs <- predict (glm.fit, Default[-train,], type = "response")

# Classify observations using a 0.5 threshold
pred.default <- rep("No", N/2)
pred.default[pred.probs > 0.5] <- "Yes"
table(pred.default)
table(default[-train],pred.default)

# Calculate the missclasification rate
error.rate <- mean(pred.default != default[-train])
error.rate

# k-fold CV and LOOCV
library(boot)
glm.fit <- glm(default ~ balance + income, family = binomial)
cost <- function(r, pi = 0) {
    mean(abs(r-pi) > 0.5)
}

cv.error.10 <- cv.glm(Default, glm.fit, cost=mycost, K=10)
cv.error.10$delta
cv.glm(data, glmfit, cost, K)
