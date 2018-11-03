

# Function to standardize input values
zscore <- function(x, mean.val=NA) {
  sd <- data.matrix(apply(x,1, sd))
  if(all(sd == 0)) return(x) # if all the values are the same
  x_norm <- apply((x - rowMeans(x)),2,"/",sd)
  return(x_norm)
}

# Derivative of the loss function
grad <- function(x, y, y_new) {
  gradient <- (1 / sqrt(ncol(y))) * ((y_new - y)%*%t(x))/sqrt(sum((y_new-y)^2))
  return(gradient)
}
# to check numerically that partial derivative is correct for 3 inputs
ngrad<- function(x_standart, y, w) {
  h <- 1e-6
  loss <- sqrt(mean((w%*%x - y)^2))
  
  w1 <- w+c(h, 0, 0, 0)
  loss1 <- sqrt(mean((w1%*%x - y)^2))
  w2 <- w+c(0, h, 0, 0)
  loss2 <- sqrt(mean((w2%*%x - y)^2))
  w3 <- w+c(0, 0, h, 0)
  loss3 <- sqrt(mean((w3%*%x - y)^2))
  w4 <- w+c(0, 0, 0, h)
  loss4 <- sqrt(mean((w4%*%x - y)^2))
  
  gradient <- c((loss1 - loss) / h, (loss2 - loss) / h, (loss3 - loss) / h, (loss4 - loss) / h)
  return(gradient)
}
# loss function for multiple input 
RMSE <- function(y,y_new){
  loss <- sqrt(mean((y_new - y)^2))
  return(loss) 
}

GradientDescent <- function(x,y,epochs, learning_rate){
  x <- rbind(rep(1, ncol(x)), x)
  m <- nrow(x) 
  n <- ncol(x)
  w<- matrix(rep(0, m), nrow=nrow(y),ncol=nrow(x) )
  Loss= list(l=numeric(epochs))
  
  for (i in 1:epochs){
   # browser()
    y_new <- w%*%x
    loss = RMSE(y,y_new)
    # Partial derivative of RMSE with respect to W
    w_grad <- grad(x, y, y_new)
    # check
    #w_grad2 <- ngrad(x, y, w)
    w <- w - (learning_rate * w_grad)
    Loss[["l"]][i] =loss  
  }
  #browser()
  Out <- list(w=nrow(w),l=numeric(epochs))
  l_plot <- unlist(Loss[["l"]])
  Out[["w"]]<-w
  Out[["l"]]<-l_plot
  return(Out)
}
