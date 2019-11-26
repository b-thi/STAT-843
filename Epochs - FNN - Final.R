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
set.seed(1994)

####### Cross-validating to find optimal number of epochs #######

# Initializing
# MSPEs_FNN <- c()
# sMSEs_FNN <- c()

MSPEs_FNN_t <- c()
sMSEs_FNN_t <- c()

for (j in 1:25) {
  
  # Initializing
  fnn_LOOCV_preds <- c()
  fnn_sMSE <- c()
  
  # RUNNING FNN LOOCV
  for (i in 1:35) {
    
    # Setting Up
    
    # obtain the annual precipitation for 35 cities
    annualprec = apply(daily$precav,2,mean)
    
    # Define the 65 Fourier basis functions
    tempbasis65  = create.fourier.basis(c(0,365), 65)
    timepts = seq(1, 365, 1)
    temp_fd = Data2fd(timepts, daily$tempav, tempbasis65)
    temp_fd$coefs <- scale(temp_fd$coefs)
    
    # Editing temp_fd order
    temp_coefs <- temp_fd$coefs[,35]
    temp_coef_name_2 <- colnames(temp_fd$coefs)[35]
    temp_coef_name_1 <- colnames(temp_fd$coefs)[i]
    temp_fd$coefs[,35] <- temp_fd$coefs[,i]
    colnames(temp_fd$coefs)[35] <- temp_coef_name_1
    temp_fd$coefs[,i] <- temp_coefs
    colnames(temp_fd$coefs)[i] <- temp_coef_name_2
    
    # Editing annual prec order
    temp_prec <- annualprec[35]
    annualprec[35] <- annualprec[1]
    annualprec[1] <- temp_prec
    names(annualprec)[35] <- temp_coef_name_1
    names(annualprec)[i] <- temp_coef_name_2
    
    # Setting up
    
    # Setting seed
    # set.seed(1)
    
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
    
    # Running model
    model <- FNN_weather(j, print_iter = F, gamma = 0.22)
    
    # Predicting
    fnn_sMSE[i] <- fnn_pred(model)
    fnn_LOOCV_preds[i] <- fnn_pred(model, new_obs = temp_fd$coefs[,35])
    
  }
  
  # Printing where the loop is at
  print(paste0("Epoch Tests Finished: ", j))
  
  # Predictions
  MSPEs_FNN_t[j] <- mean((fnn_LOOCV_preds - apply(daily$precav, 2, mean))^2)
  sMSEs_FNN_t[j] <- mean(fnn_sMSE)
}

# # Creating Data Frames
# sMSE_ds <- data.frame(Epoch = c(1:20),
#                       sMSE = c(sMSEs_FNN))
# 
# MSPE_ds <- data.frame(Epoch = 1:20,
#                       MSPE = MSPEs_FNN)
# 
# # Plotting sMSE
# ggplot(sMSE_ds, aes(x = Epoch, y = sMSE)) +
#   geom_point(color = "blue") +
#   theme_bw()
# 
# # Plotting MSPE
# ggplot(MSPE_ds, aes(x = Epoch, y = MSPE)) +
#   geom_point(color = "blue") +
#   theme_bw()


# TEST RUNS
sMSE_ds_t <- data.frame(Epoch = 1:21,
                        sMSE = c(sMSEs_FNN_t))

MSPE_ds_t <- data.frame(Epoch = 1:21,
                        MSPE = MSPEs_FNN_t)

# Plotting sMSE
ggplot(sMSE_ds_t) +
  geom_smooth(aes(x = Epoch, y = sMSE), color = "blue", 
              method = "lm",
              formula = y ~ poly(x, 5)) +
  theme_bw() +
  labs(x = "Epoch", y = "sMSE")

# Plotting MSPE
ggplot(MSPE_ds_t, aes(x = Epoch, y = MSPE)) +
  geom_smooth(data = MSPE_ds_t, aes(x = Epoch, y = MSPE), color = "blue", 
              method = "lm",
              formula = y ~ poly(x, 10)) +
  ylim(c(0, 4)) +
  theme_bw() +
  labs(x = "Epoch", y = "MSPE")
