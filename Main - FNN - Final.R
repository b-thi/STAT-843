#################################################
#############     Weather Data    ###############
##### Barinder Thind & Jiguo Cao ################
#### Functional Neural Networks #################
#################################################

###### MAIN CODE #######

##### Libraries #####
library(fda)
library(fda.usc)
library(tidyverse)
library(gridExtra)
library(ggpubr)
source("Functions - FNN - Final.R")

### Set.Seed
set.seed(18)

#################################################
# Initial Data Set Up
#################################################

# obtain the annual precipitation for 35 cities
annualprec = apply(daily$precav,2,mean)

# Define the 65 Fourier basis functions
tempbasis65  = create.fourier.basis(c(0,365), 65)
timepts = seq(1, 365, 1)
temp_fd = Data2fd(timepts, daily$tempav, tempbasis65)
temp_fd$coefs <- scale(temp_fd$coefs)

# Creating vector to store information
templist = vector("list",2)
templist[[1]] = rep(1,35) 
templist[[2]] = temp_fd 

# create a constant basis for the intercept
conbasis = create.constant.basis(c(0,365))
nbasis = 11
betabasis5 = create.fourier.basis(c(0,365), nbasis)
betalist1  = vector("list",2)
betalist1[[1]] = conbasis
betalist1[[2]] = betabasis5

#################################################
# Network Input Set up
#################################################

# W_1(t) Weights
layer_weights_1 <- runif(11*1)

# OTHER WEIGHTS
layer_weights_2 <- runif(1)
layer_bias_2 <- runif(1)
layer_bias_1 <- runif(1)

# Setting up neural network list
fnn_net <- list(
  
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

#################################################
# Building the model
#################################################

# Model Building
model <- FNN_weather(10, print_iter = T, gamma = 0.25)

model$output
model$y

#################################################
# Functional Neural Coefficient
#################################################

# FNC
neural_coef(model, print_iter = F)

#################################################
# Convergence
#################################################

# Saving plots
saved_plots <- list()

# Neural coefficients
for (i in 1:9) {
  saved_plots[[i]] <- fnn_neural_iterations(i)
  print(paste0("Done Run: ", i))
}

# Plotting
ggarrange(plotlist = saved_plots, ncol = 3, nrow = 3)


#################################################

#################################################


