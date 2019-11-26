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

# Initializing
fnn_LOOCV_preds <- c()
fnn_sMSE <- c()
fnn_weights_run <- list()
# fnn_weights_initial <- list()

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
  set.seed(10*i)
  
  # W_1(t) Weights
  layer_weights_1 <- runif(11*1)
  # fnn_weights_initial[[i]] <- layer_weights_1
  
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
  model <- FNN_weather(15, print_iter = T, gamma = 0.2)
  
  # Predicting
  fnn_sMSE[i] <- fnn_pred(model)
  fnn_LOOCV_preds[i] <- fnn_pred(model, new_obs = temp_fd$coefs[,35])
  
  # Returning weights
  fnn_weights_run[[i]] <- neural_coef_weights(model)
  
}

# Predictions
FNN_LOOCV_MSPE <- mean((fnn_LOOCV_preds - apply(daily$precav, 2, mean))^2)
FNN_LOOCV_sMSE <- mean(fnn_sMSE)

#### Getting Coefficient with Error Bars ####

# # Getting list into data sets
# df_fnc_untrained <- t(do.call(cbind, fnn_weights_initial))
# 
# # Getting functional coef
# final_beta_fnn <- function(c, x){
#   value <- c[1] + c[2]*sin(1*2*pi/365*x) + c[3]*cos(1*2*pi/365*x) +
#     c[4]*sin(2*2*pi/365*x) + c[5]*cos(2*2*pi/365*x) + c[6]*sin(3*2*pi/365*x) +
#     c[7]*cos(3*2*pi/365*x) + c[8]*sin(4*2*pi/365*x) + c[9]*cos(4*2*pi/365*x) +
#     c[10]*sin(5*2*pi/365*x) + c[11]*cos(5*2*pi/365*x)
#   return(value)
# }
# 
# # Getting values
# df_ci_untrained <- apply(df_fnc_untrained, 1, final_beta_fnn, seq(1, 365, 1))
# 
# # Getting quantile levels
# mean_neural <- apply(df_ci_untrained, 1, mean)
# lower_ci <- mean_neural - 1.96*(apply(df_ci_untrained, 1, sd)/sqrt(35))
# upper_ci <- mean_neural + 1.96*(apply(df_ci_untrained, 1, sd)/sqrt(35))
# 
# 
# # Putting together
# ci_neural_df <- data.frame(time = seq(1, 365, 1),
#                            mean_val = mean_neural,
#                            lower_ci = lower_ci,
#                            upper_ci = upper_ci)
# 
# 
# # Plotting 
# ggplot(data = ci_neural_df, aes(x = time, y = mean_val)) +
#   geom_line(size = 1) +
#   geom_line(aes(x = time, y = upper_ci), color = "skyblue") +
#   geom_line(aes(x = time, y = lower_ci), color = "skyblue") +
#   theme_bw() +
#   ggtitle("Functional Neural Coefficient Function - Untrained") +
#   xlab("Time") +
#   ylab("beta(t)") +
#   theme(plot.title = element_text(hjust = 0.5))

# Getting list into data sets
df_fnc <- t(do.call(cbind, fnn_weights_run))

# Getting functional coef
final_beta_fnn <- function(c, x){
  value <- c[1] + c[2]*sin(1*2*pi/365*x) + c[3]*cos(1*2*pi/365*x) +
    c[4]*sin(2*2*pi/365*x) + c[5]*cos(2*2*pi/365*x) + c[6]*sin(3*2*pi/365*x) +
    c[7]*cos(3*2*pi/365*x) + c[8]*sin(4*2*pi/365*x) + c[9]*cos(4*2*pi/365*x) +
    c[10]*sin(5*2*pi/365*x) + c[11]*cos(5*2*pi/365*x)
  return(value)
}

# Getting values
df_ci_trained <- apply(df_fnc, 1, final_beta_fnn, seq(1, 365, 1))

# Getting quantile levels
mean_neural <- apply(df_ci_trained, 1, mean)
lower_ci <- mean_neural - 1.96*(apply(df_ci_trained, 1, sd)/sqrt(35))
upper_ci <- mean_neural + 1.96*(apply(df_ci_trained, 1, sd)/sqrt(35))

# Putting together
ci_neural_df <- data.frame(time = seq(1, 365, 1),
                           mean_val = mean_neural,
                           lower_ci = lower_ci,
                           upper_ci = upper_ci)


# Plotting 
ggplot(data = ci_neural_df, aes(x = time, y = mean_val)) +
  geom_line(size = 1.5) +
  geom_line(aes(x = time, y = upper_ci), color = "blue") +
  geom_line(aes(x = time, y = lower_ci), color = "blue") +
  theme_bw() +
  ggtitle("Functional Neural Coefficient Function - Trained") +
  xlab("Time") +
  ylab("beta(t)") +
  theme(plot.title = element_text(hjust = 0.5))


# LM

# Initializing
functional_LM_LOOCV_preds <- c()
sMSE_LM <- c()

# Looping
for (i in 1:35) {
  
  # Setting up
  daily_prec <- daily$precav
  daily_temp <- daily$tempav
  
  # Swapping
  temp_data <- daily_prec[,35]
  daily_prec[,35] <- daily_prec[,i]
  daily_prec[,i] <- temp_data
  
  temp_data <- daily_temp[,35]
  daily_temp[,35] <- daily_temp[,i]
  daily_temp[,i] <- temp_data
  
  # colnames(daily$precav)[35] <- temp_coef_name_1
  # colnames(daily$tempav)[35] <- temp_coef_name_1
  # colnames(daily$precav)[i] <- temp_coef_name_2
  # colnames(daily$tempav)[i] <- temp_coef_name_2
  
  # pick out data and 'new data'
  dailydat <- daily_prec[,1:34]
  dailytemp <- daily_temp[,1:34]
  dailydatNew <- daily_prec[ ,35]
  dailytempNew <- daily_temp[ ,35]
  
  # set up response variable
  annualprec <- apply(dailydat, 2, mean)
  
  # Setting up test obs
  full_data <- apply(daily$precav, 2, mean)
  val_obs <- full_data[35]
  
  # create basis objects for and smooth covariate functions
  tempbasis <- create.fourier.basis(c(0,365),65)
  tempSmooth <- smooth.basis(day.5, dailytemp, tempbasis)
  tempfd <- tempSmooth$fd
  
  # create design matrix object
  templist <- vector("list",2)
  templist[[1]] <- rep(1,34)
  templist[[2]] <- tempfd
  
  # create constant basis (for intercept) and
  # fourier basis objects for remaining betas
  conbasis <- create.constant.basis(c(0,365))
  betabasis <- create.fourier.basis(c(0,365), 11)
  betalist <- vector("list",2)
  betalist[[1]] <- conbasis
  betalist[[2]] <- betabasis
  
  # regress
  annPrecTemp <- fRegress(annualprec, templist, betalist)
  
  # Checking mse
  annualprechat1 = annPrecTemp$yhatfdobj
  sMSE_LM[i] <- mean((annualprechat1 - annualprec)^2)
  
  # create basis objects for and smooth covariate functions for new data
  tempSmoothNew <- smooth.basis(day.5,dailytempNew,tempbasis)
  tempfdNew <- tempSmoothNew$fd
  
  # create design matrix object for new data
  templistNew <- vector("list",2)
  templistNew[[1]] <- rep(1,1)
  templistNew[[2]] <- tempfdNew
  
  # convert the intercept into an fd object
  onebasis <- create.constant.basis(c(0,365))
  templistNew[[1]] <- fd(matrix(templistNew[[1]],1,1), onebasis)
  
  # set up yhat matrix (in our case it's 1x1)
  yhatmat <- matrix(0,1,1)
  
  # loop through covariates
  p <- length(templistNew)
  for(j in 1:p){
    xfdj       <- templistNew[[j]]
    xbasis     <- xfdj$basis
    xnbasis    <- xbasis$nbasis
    xrng       <- xbasis$rangeval
    nfine      <- max(501,10*xnbasis+1)
    tfine      <- seq(xrng[1], xrng[2], len=nfine)
    deltat     <- tfine[2]-tfine[1]
    xmat       <- eval.fd(tfine, xfdj)
    betafdParj <- annPrecTemp$betaestlist[[j]]
    betafdj    <- betafdParj$fd
    betamat    <- eval.fd(tfine, betafdj)
    # estimate int(x*beta) via trapezoid rule
    fitj       <- deltat*(crossprod(xmat,betamat) - 
                            0.5*(outer(xmat[1,],betamat[1,]) +
                                   outer(xmat[nfine,],betamat[nfine,])))
    yhatmat    <- yhatmat + fitj
  }
  
  ## Checking prediction accuracy
  functional_LM_LOOCV_preds[i] <- (yhatmat - val_obs)^2
  
  ## 
  
}


LM_LOOCV_MSPE <- mean((functional_LM_LOOCV_preds))
LM_LOOCV_sMSE <- mean(sMSE_LM)


## Functional LM Coefficient Function ##

# obtain the annual precipitation for 35 cities
annualprec = apply(daily$precav,2,mean)

# Define the 65 Fourier basis functions
tempbasis65  = create.fourier.basis(c(0,365), 65)
timepts = seq(1, 365, 1)
temp_fd = Data2fd(timepts, daily$tempav, tempbasis65)

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

# regress
annPrecTemp <- fRegress(annualprec, templist, betalist)

# Getting coefficients
coef_lm <- annPrecTemp$betaestlist[[2]]$fd$coefs

# Getting functional coef
beta_coef_lm <- data.frame(time = seq(1, 365, 1), 
                            beta_evals = final_beta_fnn(coef_lm,
                                                        seq(1, 365, 1)))

# Plotting functional lm
beta_coef_lm %>% 
  ggplot(aes(x = time, y = beta_evals)) +
  geom_line(size = 1.5, color = "blue") +
  theme_bw() +
  ggtitle("Functional LM Coefficient Function") +
  xlab("Time") +
  ylab("beta(t)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(c(-3.5, 3.5))

# Plotting FNC
ggplot(data = ci_neural_df, aes(x = time, y = mean_val)) +
  geom_line(size = 1.5, color = "blue") +
  theme_bw() +
  ggtitle("Functional Neural Coefficient Function - Trained") +
  xlab("Time") +
  ylab("beta(t)") +
  theme(plot.title = element_text(hjust = 0.5))


