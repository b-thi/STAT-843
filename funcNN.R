## Final Project NN From Scratch

### Libraries
library(tidyverse)
library(fda)
library(refund)
library(fda.usc)

### Setting seed
set.seed(25)

### Looking at error rate
mean((lm_fda_bike$yhatfdobj - y)^2)

############## Functional Neural Network ############## 

## Activation Function
reLu <- function(x) {
  return(max(0, x))
}

## Derivative of the activation
reLu_deriv <- function(x) {
  return()
}

## Loss Function
MSE <- function(neural_net) {
  return(mean((neural_net$y - round(neural_net$output))^2))
}

## Initializing weights

#### W_1(t) Weights
layer_weights_1 <- matrix(data = runif(11*35), nrow = 35, ncol = 5)

### OTHER WEIGHTS
layer_weights_2 <- c(runif(length(annualprec)))
layer_bias_2 <- c(runif(length(length(annualprec))))
layer_bias_1 <- c(runif(length(length(annualprec))))

## Setting up neural network list
neuralnet_info <- list(
  
  # Input observations
  input = temp_fd,
  
  #### W_1(t) Weights
  layer_weights_1 = layer_weights_1,
  
  ## Otherweights
  layer_bias_1 = layer_bias_1,
  layer_weights_2 = layer_weights_2,
  layer_bias_2 = layer_bias_2,
  
  ## Output
  y = annualprec,
  
  ## Predictions
  output = matrix(rep(0, length(annualprec)), ncol = 1)
)


## Forward pass
forward_pass <- function(neural_net, nobs) {
  
  for (i in 1:nobs) {
    
    # Layer 1 activations
    neural_net$layer1[i] <- c(fda_integ_approximator(betabasis5, temp_fd, i)%*%c(neural_net$layer_weights_1[i,]) + 
                                     layer_bias_1)
  }
  
  # Output activations
  neural_net$output <- c(neural_net$layer1 * neural_net$layer_weights_2 + 
                                   layer_bias_2)
  
  return(neural_net)
}

deriv_weights1 <- matrix(ncol = 5, nrow = 35)


## Backpropagation
grad_descent <- function(neural_net, nobs){
  
  ## Easier derivative first
  # weights closer to the output layer
  deriv_weights2 <- 2*(neural_net$y - neural_net$output)*neural_net$layer1
  
  ## Backpropagating to first layer
  # Applied chain rule here
  for (i in 1:35) {
    
    # Layer 1 activations
    deriv_weights1[i,] <- 2*(neural_net$y - neural_net$output)[i]*fda_integ_approximator(betabasis5, temp_fd, i)
    
  }
  
  
  
  ## Now need to do bias derivatives
  deriv_bias2 <- 2*(neural_net$y - neural_net$output)
  deriv_bias1 <- 2*(neural_net$y - neural_net$output)*layer_weights_2*neural_net$layer1
  
  # Weight update using derivative
  learn_rate = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001
  
  for (i in 1:nobs) {
    # Layer 1 activations
    neural_net$layer_weights_1[i,] <- neural_net$layer_weights_1[i,] + learn_rate*deriv_weights1[i,]
    
  }
  
  neural_net$layer_weights_2 <- neural_net$layer_weights_2 - learn_rate*deriv_weights2
  neural_net$layer_bias_1 <- neural_net$layer_bias_1 - learn_rate*deriv_bias1
  neural_net$layer_bias_2 <- neural_net$layer_bias_2 - learn_rate*deriv_bias2
  
  # Returning updated information
  return(neural_net)
  
}

## Error Rate after no iterations
mean((neuralnet_info$output - annualprec)^2)

## Epochs
epoch_num <- 5

## Initializing loss vector
lossData <- data.frame(epoch = 1:epoch_num, MSE = rep(0, epoch_num))

## Training Neural Net
for (i in 1:epoch_num) {
  
  # Foreward iteration
  neuralnet_info <- forward_pass(neuralnet_info, 35)
  
  # Backward iteration
  neuralnet_info <- grad_descent(neuralnet_info, 35)
  
  # Storing loss
  lossData$MSE[i] <- MSE(neuralnet_info)
  
}

## Error Rate after 50 iterations
mean((neuralnet_info$output - annualprec)^2)

## Plotting Loss
lossData %>% 
  ggplot(aes(x = epoch, y = MSE)) + 
  geom_line(size = 1.25, color = "red") +
  theme_bw() +
  labs(x = "Epoch #", y = "MSE") +
  ggtitle("Change in Loss - Functional Neural Net") +
  theme(plot.title = element_text(hjust = 0.5))

