## Final Project NN From Scratch

### Libraries
library(tidyverse)
library(fda)
library(refund)
library(fda.usc)

### Setting seed
set.seed(25)

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
  return(mean((neural_net$y - neural_net$output)^2))
}

## Initializing weights

#### W_1(t) Weights
layer_weights_1 <- matrix(data = runif(11*1), nrow = 34, ncol = 11)

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
  y = annualprec[-35],
  
  ## Predictions
  output = matrix(rep(0, 34), ncol = 34)
)

neuralnet_info$output
neuralnet_info$y

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

deriv_weights1 <- matrix(ncol = 11, nrow = 34)


## Backpropagation
grad_descent <- function(neural_net, nobs, f){
  
  ## Easier derivative first
  # weights closer to the output layer
  deriv_weights2 <- 2*(neural_net$y - neural_net$output)*neural_net$layer1
  print(deriv_weights2)
  ## Backpropagating to first layer
  # Applied chain rule here
  for (i in 1:nobs) {
    
    # Layer 1 activations
    deriv_weights1[i,] <- 2*(neural_net$y - neural_net$output)[i]*fda_integ_approximator(betabasis5, temp_fd, i)*layer_weights_2[i]
    
  }
  print(deriv_weights1)
  
  
  
  ## Now need to do bias derivatives
  deriv_bias2 <- 2*(neural_net$y - neural_net$output)
  print(deriv_bias2)
  deriv_bias1 <- 2*(neural_net$y - neural_net$output)*layer_weights_2*neural_net$layer1
  print(deriv_bias1)
  # Weight update using derivative
  if(f < 6){learn_rate = 0.1}
  if(f >= 6){learn_rate = 0.01}
  if(f > 11){learn_rate = 0.001}

  for (i in 1:nobs) {
    # Layer 1 activations
    neural_net$layer_weights_1[i,] <- neural_net$layer_weights_1[i,] + learn_rate*c(scale(deriv_weights1[i,]))
    
  }
  
  neural_net$layer_weights_2 <- neural_net$layer_weights_2 + learn_rate*c(scale(deriv_weights2))
  neural_net$layer_bias_1 <- neural_net$layer_bias_1 + learn_rate*c(scale(deriv_bias1))
  neural_net$layer_bias_2 <- neural_net$layer_bias_2 + learn_rate*c(scale(deriv_bias2))
  
  # Returning updated information
  return(neural_net)
  
}

## Error Rate after no iterations
mean((neuralnet_info$output - annualprec[-35])^2)

## Epochs
epoch_num <- 10

## Initializing loss vector
lossData <- data.frame(epoch = 1:epoch_num, MSE = rep(0, epoch_num))

## Training Neural Net
for (f in 1:epoch_num) {
  
  # Foreward iteration
  neuralnet_info <- forward_pass(neuralnet_info, 34)
  
  # Backward iteration
  neuralnet_info <- grad_descent(neuralnet_info, 34, f)
  
  # Storing loss
  lossData$MSE[f] <- MSE(neuralnet_info)
  
}


neuralnet_info$output



## Error Rate after 50 iterations
mean((neuralnet_info$output - annualprec[-35])^2)

# mean((neuralnet_info$output - annualprec)/annualprec)

## Plotting Loss
lossData %>% 
  ggplot(aes(x = epoch, y = MSE)) + 
  geom_line(size = 1.25, color = "steelblue2") +
  theme_bw() +
  labs(x = "Epoch #", y = "MSE") +
  ggtitle("Change in Loss - Functional Neural Net") +
  theme(plot.title = element_text(hjust = 0.5))

## PREDICTING RESOLUTE
forward_pass <- function(neural_net, nobs) {
  
  for (i in 1:1) {
    
    # Layer 1 activations
    neural_net$layer1[i] <- c(fda_integ_approximator(betabasis5, temp_fd, i)%*%c(neural_net$layer_weights_1[i,]) + 
                                layer_bias_1)
  }
  
  # Output activations
  neural_net$output <- c(neural_net$layer1 * neural_net$layer_weights_2 + 
                           layer_bias_2)
  
  return(neural_net)
}


# Layer 1 activations
test11 <- c(fda_integ_approximator(betabasis5, temp_fd, 35)%*%colMeans(neuralnet_info$layer_weights_1) + 
                            mean(neuralnet_info$layer_bias_1))


test12 <- c(test11*mean(neuralnet_info$layer_weights_2) + 
              mean(neuralnet_info$layer_bias_2))

tested_pred2 <- (test12 - mean(dailydatNew))^2

plot(neuralnet_info$output[-35], neuralnet_info$y - neuralnet_info$output[-35])



data.frame(yhat = neuralnet_info$output[-35], 
                             res = neuralnet_info$y - neuralnet_info$output[-35]) %>% 
  ggplot(aes(x = yhat, y = res)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "steelblue2") +
  theme_bw() +
  ggtitle("Residual Plot") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(plot.title = element_text(hjust = 0.5))
  



#### Plotting coefficient function for the first layer
final_beta_function <- function(x){
  value <- 0.9472645 + 0.2540166*sin(365*x) - 0.1838267*cos(365*x) +
    0.4881373*sin(2*365*x) + 0.4477605*cos(2*365*x) + 0.4955367*sin(3*365*x) +
    0.4886242*cos(3*365*x) + 0.5034172*sin(4*365*x) + 0.47760322*cos(4*365*x) +
    0.4931472*sin(5*365*x) + 0.4954288*cos(5*365*x)
  return(value)
}

beta_coef <- data.frame(time = seq(1, 365, 15), 
                        beta_evals = final_beta_function(seq(1, 365, 15)))

beta_coef %>% 
  ggplot(aes(x = time, y = beta_evals)) +
  geom_smooth(method = "loess", se = F) +
  theme_bw() +
  ggtitle("Coefficient Function") +
  xlab("Day") +
  ylab("Beta for Temperature") +
  theme(plot.title = element_text(hjust = 0.5))

