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

### Set.Seed
set.seed(1)

#################################################

# First, let's read in the data #

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

#################################################

# Setting up

# W_1(t) Weights
layer_weights_1 <- matrix(data = runif(11*1, min = 0, max = 1), nrow = 34, ncol = 11)

# OTHER WEIGHTS
layer_weights_2 <- c(runif(length(annualprec) - 1))
layer_bias_2 <- c(runif(length(length(annualprec) - 1)))
layer_bias_1 <- c(runif(length(length(annualprec) - 1)))

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


# Running function
model <- FNN_weather(10, print_iter = F)
neural_coef(model)

# Saving plots
plot_list <- list()

for (i in 1:5) {
  model <- FNN_weather(i, print_iter = F)
  plot_list[[i]] <- neural_coef(model, print_iter = F)
}

ggarrange(plotlist = plot_list, ncol= 5, nrow = 1)
