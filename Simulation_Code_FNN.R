#################################################
############# Simulatiton Studies ###############
##### Barinder Thind & Jiguo Cao ################
#### Functional Neural Networks #################
#################################################

##### Libraries
library(fda)
library(fda.usc)
library(tidyverse)

### Set.Seed
set.seed(25)

#################################################

### Let's first create a function, x_i(t)
sophmore_dream <- function(s, r, alpha, k){
  value <- alpha + cos(k^(k*s))*(s^{-s*r})
}

### Evaluating function
soph_data <- data.frame(s = seq(0, 1, 0.01), soph_value = sophmore_dream(seq(0, 1, 0.01), 1, alpha = 0, k = 2))
  
### Plotting function values
plot(soph_data$s, soph_data$soph_value, type = "l")

### Plotting beta
plot(soph_data$s, cos(2^(seq(0, 1, 0.01)*2)))

### Test integrating this function
integrate(sophmore_dream, r = 1, alpha = 0, k = pi, lower = 0, upper = 1)[1]

### Generating integral values
y <- c()
r_values <- c()
alpha_values <- c()
for (i in 1:150) {
  
  # generating r
  val_r = rnorm(1, 1, 1.5)
  
  # storing r
  r_values[i] <- val_r
  
  # generating alpha
  val_alpha = rnorm(1, 0, 1.5)
  
  # storing alpha
  alpha_values[i] <- val_alpha
  
  # Integrating to get y
  y[i] <- integrate(sophmore_dream, r = val_r, alpha = val_alpha, k = 2, lower = 0, upper = 1)[1]
  
}

### Turning into vector
y <- unlist(y)

### Adding noise
y_star <- y[1:100] + rnorm(100, 0, 0.1)

### Generating data for each of observations
sophmore_obs <- matrix(nrow = 150, ncol = 100)
for (j in 1:150) {
  for (i in 1:100) {
    sophmore_obs[j, i] <- sophmore_dream(seq(0, 1, 0.01)[i], 
                                         r = r_values[j],
                                         alpha = alpha_values[j],
                                         k = 2) + rnorm(1, 0, 2)
    }
}

### Normalizing
sophmore_obs2 <- sophmore_obs
#sophmore_obs <- abs(sophmore_obs)

##### Creating functional observations #####

### Creating fourier basis
sophmore_basis <- create.fourier.basis(c(0, 1), 65)
s_vals <- seq(0, 0.99, 0.01) + 0.005
s_vals[c(1, 100)] <- c(0, 0.1)
sophSmooth <- smooth.basis(s_vals, 
                           t(sophmore_obs[1:100,]), 
                           sophmore_basis)
soph_fd <- Data2fd(s_vals, sophmore_obs[1:100,], sophmore_basis)
soph_fd$coefs <- scale(soph_fd$coefs)

### Plotting
plot(soph_fd, xlab = "Space", ylab = "Modified Sophmore Value")

# create design matrix object
templist <- vector("list", 2)
templist[[1]] <- rep(1, 100)
templist[[2]] <- sophSmooth$fd

# create constant basis (for intercept) and
# fourier basis objects for remaining betas
conbasis <- create.constant.basis(c(0, 1))
betabasis <- create.fourier.basis(c(0, 1), 11)
betalist <- vector("list", 2)
betalist[[1]] <- conbasis
betalist[[2]] <- betabasis

# Regression
soph_integral_test <- fRegress(y_star, templist, betalist)

# Plotting Beta
soph_lm_beta = soph_integral_test$betaestlist[[2]]$fd
plot(soph_lm_beta, xlab = "Space", ylab = "beta(s)")

#### Plotting coefficient function for lm
final_beta_lm <- function(x, c){
  value <- c[1] + c[2]*sin(1*x) - c[3]*cos(1*x) 
              c[4]*sin(2*1*x) + c[5]*cos(2*1*x) + c[6]*sin(3*1*x) +
              c[7]*cos(3*1*x) + c[8]*sin(4*1*x) + c[9]*cos(4*1*x) +
              c[10]*sin(5*1*x) + c[11]*cos(5*1*x)
  return(value)
}

beta_coef_lm <- data.frame(time = seq(0, 1, 0.05), 
                        beta_evals = final_beta_lm(seq(0, 1, 0.05),
                                                   c(soph_integral_test$betaestlist[[2]]$fd$coefs)
                        ))

beta_coef_lm %>% 
  ggplot(aes(x = time, y = beta_evals)) +
  geom_smooth(method = "loess", se = F) +
  theme_bw() +
  ggtitle("Functional Coefficient Function") +
  xlab("Space") +
  ylab("b(s) for Modified Sophmore") +
  theme(plot.title = element_text(hjust = 0.5))

# Comparing
beta_coef_true <- data.frame(time = soph_data$s,
                           beta_evals = cos(2^(2*soph_data$s)))

beta_coef_true %>% 
  ggplot(aes(x = time, y = beta_evals)) +
  geom_smooth(method = "loess", se = F) +
  theme_bw() +
  ggtitle("True Coefficient Function") +
  xlab("Space") +
  ylab("b(s) for Modified Sophmore") +
  theme(plot.title = element_text(hjust = 0.5))

# Predictions
soph_pred = soph_integral_test$yhatfdobj

### Plotting predictions
data.frame(soph_pred = soph_pred, y = y[1:100]) %>% 
  ggplot(aes(x = soph_pred, y = y)) +
  geom_point() +
  theme_bw() +
  xlab("Sophmore Predictions (yhat)") + 
  ylab("Actual Values (y)") +
  ggtitle("Fitted vs. Actual") + 
  theme(plot.title = element_text(hjust = 0.5))

### Error
mean((soph_pred - y[1:100])^2)

### looking at functional lm intercept
soph_integral_test$betaestlist[[1]]

### Getting values for functional lm beta
c(soph_integral_test$betaestlist[[2]]$fd$coefs)

### Doing predictions outside of the box
final_pred_functionLM <- function(x, r, c){
  value <- (c[1] + c[2]*sin(1*x) - c[3]*cos(1*x) +
              c[4]*sin(2*1*x) + c[5]*cos(2*1*x) + c[6]*sin(3*1*x) +
              c[7]*cos(3*1*x) + c[8]*sin(4*1*x) + c[9]*cos(4*1*x) +
              c[10]*sin(5*1*x) + c[11]*cos(5*1*x))*(x^{-x*r})
  return(value)
}

y_test_lm_pred <- c()

for (i in 1:50) {
  
  y_test_lm_pred[i] <- integrate(final_pred_functionLM, r = r_values[i+100],
                                 c = c(soph_integral_test$betaestlist[[2]]$fd$coefs), 
                                 lower = 0, upper = 1)[1]
  
}

## Testing error
mean((unlist(y_test_lm_pred) - y[101:150])^2)

###### Now running with functional neural networks ######
### Setting seed
set.seed(25)

############## Functional Neural Network ############## 

# Creating simpsons rule function
composite_approx <- function(f, a, b, n) {
  
  ## Error checking code
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  ## General formula
  h <- (b - a)/n
  
  ## Setting parameters
  xn <- seq.int(a, b, length.out = n + 1)
  xn <- xn[-1]
  xn <- xn[-length(xn)]
  
  ## Approximating
  integ_approx <- (h/3)*(f(a) + 2*sum(f(xn[seq.int(2, length(xn), 2)])) + 
                           4*sum(f(xn[seq.int(1, length(xn), 2)])) + 
                           f(b))
  
  ## Returning result
  return(integ_approx)
  
}

fda_integ_approximator <- function(beta_basis, func_obs, k) {
  ### First, I create formulas for the functional observation, x(t)
  
  ## Initializing
  updated_names <- NULL
  updated_internal_name <- NULL
  
  ## Looping to create coefficients
  for (i in 1:length(func_obs$coefs[1,])) {
    for (j in 1:length(func_obs$basis$names)) {
      if(func_obs$basis$names[j] == "const"){
        updated_internal_name <- paste0(updated_internal_name, " + ", func_obs$coefs[j,i])
      } else {
        updated_internal_name <- paste0(updated_internal_name, " + ", func_obs$coefs[j,i], "*", func_obs$basis$names[j])
      }
    }
    updated_internal_name <- substring(updated_internal_name, 4)
    updated_names[i] <- updated_internal_name
    updated_internal_name <- NULL
  }
  
  ### Next, I create an update on the beta names
  
  ## Initializing
  updated_beta_names <- NULL
  
  ## Looping
  for (i in 1:length(beta_basis$names)) {
    updated_beta_names <- paste0(beta_basis$names)
  }
  
  ### Next, I put together the previous results
  
  ## Cleaning up data set after expanding
  basis_combinations = expand.grid(updated_beta_names, updated_names[k])
  basis_combinations = basis_combinations[basis_combinations$Var1 != "const", ]
  basis_combinations = basis_combinations[basis_combinations$Var2 != "const", ]
  
  ## Initializing updated form
  final_form <- NULL
  
  ### Now, getting proper combinations
  for (i in 1:length(basis_combinations$Var1)) {
    final_form[i] <- paste0(basis_combinations[i, 1], "*", "(", updated_names[k], ")")
  }
  
  ### Replacing sin1, cos1 type of things
  for (j in 1:length(basis_combinations$Var1)) {
    for (i in 1:100) {
      if(grepl(paste0("sin", 100 - i), final_form[j])){
        final_form[j] <- gsub(paste0("sin", 100 - i), paste0("sin", "(", 100 - i, "*", "1", "*", "x)"), final_form[j])
      }
      if(grepl(paste0("cos", 100 - i), final_form[j])){
        final_form[j] <- gsub(paste0("cos", 100 - i), paste0("cos", "(", 100 - i, "*", "1", "*", "x)"), final_form[j])
      }
    }
  }
  
  ## Taking care of const
  const_calc <- updated_names[k]
  for (i in 1:100) {
    if(grepl(paste0("sin", 100 - i), const_calc)){
      const_calc <- gsub(paste0("sin", 100 - i), paste0("sin", "(", 100 - i, "*", "1", "*", "x)"), const_calc)
    }
    if(grepl(paste0("cos", 100 - i), const_calc)){
      const_calc <- gsub(paste0("cos", 100 - i), paste0("cos", "(", 100 - i, "*", "1", "*", "x)"), const_calc)
    }
  }
  
  # Creating function
  const_func <- function(x){
    a = eval(parse(text = const_calc))
    return(a)
  }
  
  # Doing integral
  const_int <- composite_approx(const_func, 0, 1, 5000)
  
  # Initializing
  integral_evals <- c()
  
  # Looping to get all evals
  for (r in 1:length(basis_combinations$Var1)) {
    
    final_func <- function(x){
      a = eval(parse(text = final_form[r]))
      return(a)
    }
    
    integral_evals[r] <- composite_approx(final_func, 0, 1, 5000)
  }
  
  # Putting together
  final_results <- c(const_int, integral_evals)
  
  final_results <- c(scale(final_results))
  
  return(final_results)
  
}

## Updated beta
betabasis <- create.fourier.basis(c(0, 1), 11)

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
layer_weights_1 <- matrix(data = rnorm(11*1, mean = 0, sd = 3), nrow = 99, ncol = 11)

### OTHER WEIGHTS
layer_weights_2 <- c(runif(length(y_star) - 1))
layer_bias_2 <- c(runif(length(length(y_star) - 1)))
layer_bias_1 <- c(runif(length(length(y_star) - 1)))

## Setting up neural network list
neuralnet_info <- list(
  
  # Input observations
  input = soph_fd,
  
  #### W_1(t) Weights
  layer_weights_1 = layer_weights_1,
  
  ## Otherweights
  layer_bias_1 = layer_bias_1,
  layer_weights_2 = layer_weights_2,
  layer_bias_2 = layer_bias_2,
  
  ## Output
  y = y_star[-100],
  
  ## Predictions
  output = matrix(rep(0, 99), ncol = 99)
)


## Forward pass
forward_pass <- function(neural_net, nobs) {
  
  for (i in 1:nobs) {
    
    # Layer 1 activations
    neural_net$layer1[i] <- c(fda_integ_approximator(betabasis, soph_fd, i)%*%c(neural_net$layer_weights_1[i,]) + 
                                layer_bias_1)
  }
  
  # Output activations
  neural_net$output <- c(neural_net$layer1 * neural_net$layer_weights_2 + 
                           layer_bias_2)
  
  return(neural_net)
}

deriv_weights1 <- matrix(ncol = 11, nrow = 99)


## Backpropagation
grad_descent <- function(neural_net, nobs, f){
  
  ## Easier derivative first
  # weights closer to the output layer
  deriv_weights2 <- 2*(neural_net$y - neural_net$output)*neural_net$layer1
  
  ## Backpropagating to first layer
  # Applied chain rule here
  for (i in 1:nobs) {
    
    # Layer 1 activations
    deriv_weights1[i,] <- 2*(neural_net$y - neural_net$output)[i]*fda_integ_approximator(betabasis, soph_fd, i)*layer_weights_2[i]
    
  }
  
  
  
  
  ## Now need to do bias derivatives
  deriv_bias2 <- 2*(neural_net$y - neural_net$output)
  
  deriv_bias1 <- 2*(neural_net$y - neural_net$output)*layer_weights_2*neural_net$layer1
  
  ## Adaptive learning rate
  if(f < 6){learn_rate = 0.1}
  if(f >= 6){learn_rate = 0.01}
  if(f > 11){learn_rate = 0.001}
  
  ## Weight update using derivative
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

### Errorr rate before
mean((neuralnet_info$output - y[-c(100:150)])^2)

## Epochs
epoch_num <- 7

## Initializing loss vector
lossData <- data.frame(epoch = 1:epoch_num, MSE = rep(0, epoch_num))

## Training Neural Net
for (f in 1:epoch_num) {
  
  # Foreward iteration
  neuralnet_info <- forward_pass(neuralnet_info, 99)
  
  # Backward iteration
  neuralnet_info <- grad_descent(neuralnet_info, 99, f)
  
  # Storing loss
  lossData$MSE[f] <- MSE(neuralnet_info)
  
  print(f)
  
}

## Error Rate after 10 iterations
FunctionalNN_MSE_soph <- mean((neuralnet_info$output - y[1:99])^2)


## Plotting Loss
lossData %>% 
  ggplot(aes(x = epoch, y = MSE)) + 
  geom_line(size = 1.25, color = "steelblue2") +
  theme_bw() +
  labs(x = "Epoch #", y = "MSE") +
  ggtitle("Change in Loss - Functional Neural Net") +
  theme(plot.title = element_text(hjust = 0.5))

## Plotting

## Getting weights
sim_beta_coefs <- apply(neuralnet_info$layer_weights_1, 2, mean)

#### Plotting coefficient function for the first layer
final_beta_function <- function(x, c){
  value <- c[1] + c[2]*sin(1*x) - c[3]*cos(1*x) +
    c[4]*sin(2*1*x)# + c[5]*cos(2*1*x) + c[6]*sin(3*1*x) +
    #c[7]*cos(3*1*x) + c[8]*sin(4*1*x) 
  return(value)
}

beta_coef <- data.frame(time = seq(0, 1, 0.05), 
                        beta_evals = final_beta_function(seq(0, 1, 0.05),  sim_beta_coefs))

beta_coef %>% 
  ggplot(aes(x = time, y = beta_evals)) +
  geom_smooth(method = "loess", se = F) +
  geom_line(data = beta_coef_true, aes(x = beta_coef_true$time, y = beta_coef_true$beta_evals), color = "red") +
  theme_bw() +
  ggtitle("Neural Coefficient Function") +
  xlab("Space") +
  ylab("b(s) for Modified Sophmore") +
  theme(plot.title = element_text(hjust = 0.5))

## Looking at intercept
mean(neuralnet_info$layer_bias_1)

## Fitted vs residuals
data.frame(yhat = neuralnet_info$output, 
           y = neuralnet_info$y) %>% 
  ggplot(aes(x = yhat, y = y[1:99])) +
  geom_point(color = "steelblue2") +
  theme_bw() +
  xlab("Fitted Values") +
  ylab("Actual Values") +
  theme(plot.title = element_text(hjust = 0.5))

### Predicting
final_pred_functionNN <- function(x, r, c){
  value <- (c[1] + c[2]*sin(1*x) + c[3]*cos(1*x) +
    c[4]*sin(2*1*x) + c[5]*cos(2*1*x) + c[6]*sin(3*1*x) +
    c[7]*cos(3*1*x) + c[8]*sin(4*1*x) + c[9]*cos(4*1*x) +
    c[10]*sin(5*1*x) + c[11]*cos(5*1*x))*(x^{-x*r})
  return(value)
}

y_test_funcNN_pred <- c()

for (i in 1:50) {
  
  y_test_funcNN_pred[i] <- integrate(final_pred_functionNN, r = r_values[i+100],
                                     c = sim_beta_coefs, lower = 0, upper = 1)[1] 
  
}

## Testing error
mean((unlist(y_test_funcNN_pred) - y[101:150])^2)

#### Difference in L2 norms
L2_Diff_FuncNN <- function(x, k){
  value <- ((0.6161439 + 0.5010658*sin(1*x) - 0.5665885*cos(1*x) +
              0.5657255*sin(2*1*x) + 0.4471477*cos(2*1*x) + 0.5370226*sin(3*1*x) +
              0.3258*cos(3*1*x) + 0.44589*sin(4*1*x) + 0.26480*cos(4*1*x) +
              0.354716*sin(5*1*x) + 0.2821453*cos(5*1*x)) - cos(1.5^(1.5*x)))^2
  return(value)
}

integrate(L2_Diff_FuncNN, k = 1.5, lower = 0, upper = 1)

L2_Diff_FuncLM <- function(x, k){
  value <- ((0.3897141 + 11.0179094*sin(1*x) - 11.4425681*cos(1*x) +
              3.1262353*sin(2*1*x) + -26.9541209*cos(2*1*x) + -36.1780191*sin(3*1*x) +
              17.30000608*cos(3*1*x) + 48.2747648*sin(4*1*x) + 21.3105881*cos(4*1*x) +
              -1.6545053*sin(5*1*x) + -34.2163824*cos(5*1*x)) - cos(1.5^(1.5*x)))^2
  return(value)
}

integrate(L2_Diff_FuncLM, k = 2, lower = 0, upper = 1)

##### LOOCV ######
##### Functional LM ######

# Initiating
functional_LM_LOOCV_preds <- c()

#### Running
for (i in 1:34) {
  
  # Swapping
  dailydat <- daily$precav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailytemp <- daily$tempav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailydat2 <- daily$precav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailytemp2 <- daily$tempav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  
  # pick out data and 'new data'
  dailydat <- dailydat[,1:34]
  dailytemp <- dailytemp[,1:34]
  dailydatNew <- dailydat2[ ,35]
  dailytempNew <- dailytemp2[ ,35]
  
  # set up response variable
  annualprec1 <- apply(dailydat, 2, mean)
  
  # create basis objects for and smooth covariate functions
  tempbasis <- create.fourier.basis(c(0,365),65)
  tempSmooth <- smooth.basis(day.5,dailytemp,tempbasis)
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
  annPrecTemp <- fRegress(annualprec1, templist, betalist)
  
  # Checking mse
  annualprechat1 = annPrecTemp$yhatfdobj
  mean((annualprechat1 - annualprec1[-c(35)])^2)
  
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
  functional_LM_LOOCV_preds[i] <- (yhatmat - mean(dailydatNew))^2
}

# LOOCV Error
mean((functional_LM_LOOCV_preds)^2)

##### Now, doing the same for functional NN LOOCV ######

# Initiating
functional_NN_LOOCV_preds <- c()
functional_NN_weights <- list()

for (i in 1:34) {
  
  # Swapping
  dailydat <- daily$precav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailytemp <- daily$tempav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailydat2 <- daily$precav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  dailytemp2 <- daily$tempav[,c(1:(35-(i+1)), (35-i + 1):35, (35-i))]
  
  # pick out data and 'new data'
  dailydat <- dailydat[,1:34]
  dailytemp <- dailytemp[,1:34]
  dailydatNew <- dailydat2[ ,35]
  dailytempNew <- dailytemp2[ ,35]
  
  # Define the 65 Fourier basis functions
  tempbasis65  = create.fourier.basis(c(0,365), 65)
  timepts = seq(1, 365, 1)
  temp_fd = Data2fd(timepts, dailytemp, tempbasis65)
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
  
  # set up response variable
  annualprec1 <- apply(dailydat, 2, mean)
  
  ## Loss Function
  MSE <- function(neural_net) {
    return(mean((neural_net$y - neural_net$output)^2))
  }
  
  ## Initializing weights
  
  #### W_1(t) Weights
  layer_weights_1 <- matrix(data = runif(11*1), nrow = 34, ncol = 11)
  
  ### OTHER WEIGHTS
  layer_weights_2 <- c(runif(length(annualprec) - 1))
  layer_bias_2 <- c(runif(length(length(annualprec) - 1)))
  layer_bias_1 <- c(runif(length(length(annualprec) - 1)))
  
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
    y = annualprec1,
    
    ## Predictions
    output = matrix(rep(0, 34), ncol = 34)
  )
  
  
  ## Forward pass
  forward_pass <- function(neural_net, nobs) {
    
    for (h in 1:nobs) {
      
      # Layer 1 activations
      neural_net$layer1[h] <- c(fda_integ_approximator(betabasis5, temp_fd, h)%*%c(neural_net$layer_weights_1[h,]) + 
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
    
    ## Backpropagating to first layer
    # Applied chain rule here
    for (u in 1:nobs) {
      
      # Layer 1 activations
      deriv_weights1[u,] <- 2*(neural_net$y - neural_net$output)[u]*fda_integ_approximator(betabasis5, temp_fd, i)*layer_weights_2[u]
      
    }
    
    ## Now need to do bias derivatives
    deriv_bias2 <- 2*(neural_net$y - neural_net$output)
    
    deriv_bias1 <- 2*(neural_net$y - neural_net$output)*layer_weights_2*neural_net$layer1
    
    ## Adaptive learning rate
    if(f < 6){learn_rate = 0.1}
    if(f >= 6){learn_rate = 0.01}
    if(f > 11){learn_rate = 0.001}
    
    ## Weight update using derivative
    for (v in 1:nobs) {
      # Layer 1 activations
      neural_net$layer_weights_1[v,] <- neural_net$layer_weights_1[v,] + learn_rate*c(scale(deriv_weights1[v,]))
      
    }
    
    neural_net$layer_weights_2 <- neural_net$layer_weights_2 + learn_rate*c(scale(deriv_weights2))
    neural_net$layer_bias_1 <- neural_net$layer_bias_1 + learn_rate*c(scale(deriv_bias1))
    neural_net$layer_bias_2 <- neural_net$layer_bias_2 + learn_rate*c(scale(deriv_bias2))
    
    # Returning updated information
    return(neural_net)
    
  }
  
  ## Epochs
  epoch_num <- 7
  
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
  
  # Redefining temp_fd
  tempbasis65  = create.fourier.basis(c(0,365), 65)
  timepts = seq(1, 365, 1)
  temp_fd = Data2fd(timepts, dailytemp2, tempbasis65)
  temp_fd$coefs <- scale(temp_fd$coefs)
  
  # Predicting out of bag
  LOOCV_layer1 <- c(fda_integ_approximator(betabasis5, temp_fd, 35)%*%colMeans(neuralnet_info$layer_weights_1) + 
                         mean(neuralnet_info$layer_bias_1))
  
  
  LOOCV_pred <- c(LOOCV_layer1*mean(neuralnet_info$layer_weights_2) + 
                       mean(neuralnet_info$layer_bias_2))

  functional_NN_LOOCV_preds[i] <- (LOOCV_pred - mean(dailydat2[,35]))^2
  
  print(i)
  print(LOOCV_pred)
  print(colnames(dailydat2)[35])
  print(colnames(dailytemp2)[35])
  
  ## Saving coefficient function
  functional_NN_weights[[i]] <- apply(neuralnet_info$layer_weights_1, 2, mean)
}

# LOOCV Error
mean((functional_NN_LOOCV_preds[-c(6, 3:33)])^2)
mean((functional_LM_LOOCV_preds[-c(6, 3:33)])^2)

sum(functional_NN_LOOCV_preds <= functional_LM_LOOCV_preds)

## Functional NN Weights - 5 Epochs
functional_NN_weights_5epoch <- functional_NN_weights
functional_NN_weights_3epoch <- functional_NN_weights

colnames(dailytemp)

for (i in 1:34) {
  
  # printing
  print(c(1:(35-(i+1)), (35-i + 1):35, (35-i)))

}

apply(dailydat2, 2, mean)


###### Plotting all functional coefficients
## Plotting

## Getting weights
apply(neuralnet_info$layer_weights_1, 2, mean)

#### Plotting coefficient function for the first layer
general_beta_function <- function(x, c){
  value <- c[1] + c[2]*sin(1*x) - c[3]*cos(1*x) +
    c[4]*sin(2*1*x) + c[5]*cos(2*1*x) + c[6]*sin(3*1*x) +
    c[7]*cos(3*1*x) + c[8]*sin(4*1*x) + c[9]*cos(4*1*x) +
    c[10]*sin(5*1*x) + c[11]*cos(5*1*x)
  return(value)
}

par(mfrow=c(1, 2))

for (o in 1:30) {
  beta_coeftest <- data.frame(time = seq(1, 365, 15), 
                              beta_evals = general_beta_function(seq(1, 365, 15), 
                                                                 functional_NN_weights_5epoch[[19]]))
  
  
  beta_coeftest %>% 
    ggplot(aes(x = time, y = beta_evals)) +
    geom_smooth(method = "loess", se = F) +
    theme_bw() +
    ggtitle("Neural Coefficient Function") +
    xlab("Space") +
    ylab("b(s) for Modified Sophmore") +
    theme(plot.title = element_text(hjust = 0.5))
}


plot(temp_fd[28])
