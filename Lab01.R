#====================================
# Author: Yang Cao
# Date: August 31, 2017
# Purpose: Introduction to R
#====================================

rm(list = ls())

# Assigning data to an object
x <- c(4,8,15,16,23,42)

# Basic summary statistics
length(x)
mean(x) 
min(x)
max(x)
sd(x)
summary(x)

# Create more objects
y <- c(8,27,34,4,19,10)
z <- c(1,2)

# Combine objects
a <-x+y
b <-x+z

# Check what is currently in the workspace
ls()

# Remove objects
rm(x)
rm(a,b)

# Help files
?matrix
help(matrix)

# Create a matrix
x <- matrix(data=c(1,2,3,4),nrow=2, ncol=2)
# Create the same matrix but with shorter notation
x <- matrix(c(1,2,3,4),2,2)
View(as.matrix(1:10))

# Simple operations on the elements of a matrix
x^2
sqrt(x)

# Indexing
x[1,]
x[,1]
x[1,2]

# Generate two vectors with random numbers
set.seed(42)
x <- rnorm(50)
View(x)
y <- x + rnorm(50, mean=50, sd=.1)

# Scatterplot of two vectors
plot(x,y)
plot(x ,y , xlab="Advertising budget", ylab="Sales", main="Simulated data")

# Adding a regression line
abline(lm(y ~ x), col="red")

# Read a data file
Auto <- read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data")

# Look at the content of the file
dim(Auto)
head(Auto)
View(Auto)

# Read the data file again
Auto2 <- read.table("http://www-bcf.usc.edu/~gareth/ISL/Auto.data", header=TRUE, na.strings="?")
View(Auto2)
head(Auto2)

# Check the column headers
names(Auto)
names(Auto2)

# Remove missing values
Auto3 <- na.omit(Auto2)
View(Auto3)

# Indexing a data frame
Auto3$year
table(Auto3$year)

# look at the column cylinders
Auto3$cylinders
summary(Auto3$cylinders)
table(Auto3$cylinders)
plot(Auto3$cylinders)

# Convert column cylinders to a factor variable
Auto3[Auto3$cylinders==5,]
Auto3$cylinders <- as.factor(Auto3$cylinders)
typeof(Auto3$cylinders)
plot(Auto3$cylinders)
is.factor(Auto3$cylinders)
Auto3$cylinders <- as.vector(Auto3$cylinders)
plot(Auto3$cylinders)
