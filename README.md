# Solution to the Exercise 2.
 
The **GradientUtils.R** is a set of functions designed to perform multivariate linear regression using gradient descent. 

The implementation is training a linear model using root of mean squares (RMSE) as a loss function.

The purpose of this code is to show that minimization of the root of mean squares 
of the neuron corresponds to the minimization of the Euclidean distance in multivariate linear regression. 


The training is performed on "mass_boston.csv" datasets. The last column in the table (**medv**)
is the output of the model. The training can be performed on one or several predictors.


## Content   

The **GradientUtils.R**  containes functions:

* `GadientDescent` - is a function that minimizes `RMSE` in order to obtain optimal linear model parameters (slope and bias),
* `RMSE` - is a loss (cost) fucntion calcualted as a root of mean squares,
* `grad` - is a partial derivative of `RMSE` by weights,
* `zscore` - a function that normalizes rowwise predictors values. 

## Argumnets of the functions: 

`GradientDescent(x_standart,y,epochs, learning_rate)`:

* `x_standart` - is a matrix of predictors with normalized values. Rows correspond to number of predictors and columns to data points.
* `y` - is a response matrix. It must have 1 row and similar number of columns as `x_standart`.
* `epochs` - number of iterations of the algorithm that are needed for gradient descent to converge. This value should be specified by the user. 
* `learning_rate` - is a step of the gradient descent. 


## Example:  

```
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
W <- Out[["w"]] # weights
L_plot <- Out[["l"]] # loss
par(mfrow = c(2, 2))
plot(L_plot, col = "blue", pch = 20, xlab = "iterations", ylab = "Loss")
title("Loss function")

# only 3 predictors are ploted
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

```

## Implementation details

* the code uses matrix operations
* the bias is accounted for by concatenating a unit vector with the input matrix
* no activation function is used

**NOTE:** the code uses only *R* built-in functionality

### Contributor

[Natalia Chicherova](https://www.linkedin.com/in/natalia-chicherova-/)

2018
