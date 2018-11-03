# clear workspace
rm(list=ls())

# include all the necessary functions
source("GradientUtils.R")

# read data from the current dir
data = read.csv("mass_boston.csv")

# choose number of inputs and transpose the table
# e.g. x has 8 rows (predictors) and 506 columns (data points)
x <- t(data.matrix(data[, c(1:8)]))

# Standardize the features
x_standart <- zscore(x)

# y has 1 row and 506 columns
y <- t(matrix(data$medv, ncol=1))

# set the parameters for training the model
epochs <- 10000
learning_rate <- 0.01
# get model using gradient descent with RMSE
Out<- GradientDescent(x_standart,y,epochs, learning_rate)

# get linear model using euclidean distance 
fit <- lm(y[1,] ~ t(x_standart))
Regression_res <- fit$coefficients
summary(fit)

# plot solution 
W <- Out[["w"]]
L_plot <- Out[["l"]]
par(mfrow = c(2, 2))
plot(L_plot, col = "blue", pch = 20, xlab = "iterations", ylab = "Loss")
title("Loss function")


for (i in 1:3){
  X <- x_standart[i,]
  # calculate bias
  b <- W[1]%*%t(data.matrix(rep(1, ncol(x))))
  Y <- W[i+1]*X+b
  # plot(X1,Y)
  plot(X,y[1,],col = "blue", pch = 20, xlab = "predictor", ylab = "Y (response)")
  abline(b[1],W[i+1],col="red",lwd = 5)
  abline(Regression_res[1],Regression_res[i+1],col="green")
  title(sprintf("Models for predictor %i",i))
  legend("top", legend = c("Neuron","Regression"), col=c("red","green"), lty = 1, box.lty =0)
}

