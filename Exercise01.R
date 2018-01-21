rm(list=ls())

#====================================
# Author: Yang Cao
# Date: August 31, 2017
# Purpose: Introduction to R
#====================================

# Read the data into R
college <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/College.csv")
View(college)
# Set the first column as the row names
rownames(college) <- college[ ,1]
college <- college[ ,-1]
View(college)
head(college[, 1:5])
# Produce a numerical summary of the variables in the data set
summary(college$Apps)
summary(college)
college[,1:10]
# Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 
pairs(college[,1:10])
# Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
plot(college$Private, college$Outstate, xlab = "Private University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# Create a new variable
college$Elite <- rep("No", nrow(college))
college$Elite[college$Top10perc > 50] <- "Yes"
college$Elite <- as.factor(college$Elite)
plot(college$Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab ="Out of State tuition in USD", main = "Outstate Tuition Plot")
# Divide the print window into four regions
par(mfrow = c(2,2))
hist(college$Books, col = 2, xlab = "Books", ylab = "Count")
hist(college$PhD, col = 3, xlab = "PhD", ylab = "Count")
hist(college$Grad.Rate, col = 4, xlab = "Grad Rate", ylab = "Count")
hist(college$perc.alumni, col = 6, xlab = "% alumni", ylab = "Count")
summary(college$PhD)

