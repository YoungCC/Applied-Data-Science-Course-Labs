rm(list=ls())

#========================================
# Author : Yang Cao
# Date: November 9, 2017
# Purpose: Demonstrate Non-Linear Models
#========================================

library(ISLR)
attach(Wage)
View(Wage)

# Polynomial Linear Regression
par(mfrow=c(1,1))
plot(age, wage)
fit <- lm(wage ~ poly(age,4))
summary(fit)
summary(age)

age.grid <- seq(from = min(age), to = max(age))
age.grid
length(age.grid)
pred <- predict(fit, newdata = list(age = age.grid))
pred
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
pred
length(pred$fit)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands
predict(fit, data.frame(age = age.grid), interval = "confidence")
predict(fit, data.frame(age = age.grid), interval = "prediction")

plot(age, wage, cex=.5, col="darkgray", main="Degree-4Polynomial")
lines(age.grid, pred$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)


plot(age, wage, cex=.5, col="darkgray", main="Degree-4Polynomial")
colors <- c("red", "blue", "green", "black")
for (i in seq(1,4)){
  fit <- lm(wage ~ poly(age,i))
  pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
  se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  lines(age.grid, pred$fit, lwd=2, col=colors[i])
  matlines(age.grid, se.bands, lwd=1, col=colors[i], lty=3)
}

# Polynomial Logistic Regression
I(wage > 250)
fit <- glm(I(wage > 250) ~ poly(age, 4), family=binomial)
summary(fit)
pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
pred
pred2 <-predict(fit, newdata = list(age = age.grid), type="response")
pred2
pfit <- exp(pred$fit) / (1+ exp(pred$fit))
pfit
se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

plot(age, I(wage>250), type="n", ylim=c(0,1))
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)
rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)

plot(age, I(wage>250), type="n", ylim=c(0,.2), main="Degree-4Polynomial")
colors <- c("red", "blue", "green", "black")
rug(jitter(age[wage<=250]), side = 1, ticksize = 0.02)
rug(jitter(age[wage>250]), side = 3, ticksize = 0.02)
for (i in seq(1,4)){
  fit <- glm(I(wage > 250) ~ poly(age, i), family=binomial)
  pred <- predict(fit, newdata = list(age = age.grid), se = TRUE)
  pfit <- exp(pred$fit) / (1+ exp(pred$fit))
  se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
  se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
  lines(age.grid, pfit, lwd=2, col=colors[i])
  matlines(age.grid, se.bands, lwd=1, col=colors[i], lty=3)
}

# Step Functions
table(cut(age, 4))
fit <- lm(wage ~ cut(age, 4))
summary(fit)
plot(fit)
pred <- predict(fit, newdata = list(age = age.grid), type="response")
plot(age, wage)
lines(age.grid, pred, lwd=2, col="red")

# Splines
library(splines)
knot.position <- c(25, 40, 60)
fit <- lm(wage ~ bs(age, knots=knot.position), data=Wage)
summary(fit)
pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, cex=.5, col="darkgray")
lines(age.grid, pred$fit, lwd=2, col="red")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="red")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="red")
abline(v = knot.position, lty="dashed")

summary(age)
knot.position <- c(summary(age))
fit <- lm(wage ~ bs(age, knots=knot.position), data=Wage)
summary(fit)
pred <- predict(fit, newdata=list(age=age.grid), se=TRUE)
plot(age, wage, cex=.5, col="darkgray")
lines(age.grid, pred$fit, lwd=2, col="red")
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", col="red")
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", col="red")
abline(v = knot.position, lty="dashed")

# Smoothing Splines
fit <- smooth.spline(age, wage, df=16)
summary(fit)
fit2 <- smooth.spline(age, wage, cv=TRUE)
summary(fit2)
fit
fit2
plot(age, wage, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF (LOOCV)"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)

advertising <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
attach(advertising)
View(advertising)
fit <- smooth.spline(TV, sales, cv=TRUE)
fit
plot(TV,sales)
lines(fit, col="red")
fit2 <- lm(sales~TV)
abline(fit2,col="blue")

# Generalized Additive Models (GAM)
gam1 <- lm(wage ~ ns(year, 4) + ns(age,5) + education)
library(splines)
library(foreach)
library(gam)
gam2 <- gam(wage ~ s(year, 4) + s(age, 5) + education)
plot(gam1)
par(mfrow = c(1,3))
plot(gam2, se=TRUE, col="blue")
par(mfrow = c(1,1))
