#################################################
#############     Weather Data    ###############
##### Barinder Thind & Jiguo Cao ################
#### Functional Neural Networks #################
#################################################

###### FUNCTIONS #######

##### Libraries #####
library(fda)
library(fda.usc)
library(tidyverse)

#################################################

# Composite approximator
composite_approximator <- function(f, a, b, n) {
  
  # This function does the integral approximations and gets called in the
  # integral approximator function. In the integral approximator function
  # we pass in a function f into this and that is final output - a collection
  # of numbers - one for each of the functional observations
  
  # Error checking code
  if (is.function(f) == FALSE) {
    stop('The input f(x) must be a function with one parameter (variable)')
  }
  
  # General formula
  h <- (b - a)/n
  
  # Setting parameters
  xn <- seq.int(a, b, length.out = n + 1)
  xn <- xn[-1]
  xn <- xn[-length(xn)]
  
  # Approximating using the composite rule formula
  integ_approx <- (h/3)*(f(a) + 2*sum(f(xn[seq.int(2, length(xn), 2)])) + 
                           4*sum(f(xn[seq.int(1, length(xn), 2)])) + 
                           f(b))
  
  # Returning result
  return(integ_approx)
  
}

#################################################

# Integration Approximation
integral_form <- function(functional_data, 
                          beta_basis = NULL, 
                          num_fd_basis = 65, 
                          num_beta_basis = 11){
  
  ########################################################################
  
  #### Setting up x_i(s) form ####
  
  # Initializing
  func_basis_sin <- c()
  func_basis_cos <- c()
  
  # Setting up vectors
  for (i in 1:((num_fd_basis - 1)/2)) {
    func_basis_sin[i] <- paste0("sin(2*pi*", i, "/365)")
  }
  for (i in 1:((num_fd_basis - 1)/2)) {
    func_basis_cos[i] <- paste0("cos(2*pi*", i, "/365)")
  }
  
  # Putting together
  fd_basis_form <- c(1, rbind(func_basis_sin, func_basis_cos))
  
  # Combining with functional data
  x_1s <- paste0(functional_data, "*", fd_basis_form, collapse = " + ")
  
  ########################################################################
  
  #### Setting up beta_(s) ####
  
  beta_basis_sin <- c()
  beta_basis_cos <- c()
  
  # Setting up vectors
  for (i in 1:((num_beta_basis - 1)/2)) {
    beta_basis_sin[i] <- paste0("sin(2*pi*", i, "/365)")
  }
  for (i in 1:((num_beta_basis - 1)/2)) {
    beta_basis_cos[i] <- paste0("cos(2*pi*", i, "/365)")
  }
  
  # Combining with functional data
  beta_basis_form <- c(1, rbind(beta_basis_sin, beta_basis_cos))
  
  ########################################################################
  
  #### Getting approximations ####
  
  # Initializing - should be vector of size 11
  integ_approximations <- c()
  
  for (i in 1:length(beta_basis_form)) {
    
    # Combining
    form_approximated <- paste0(beta_basis_form[i], "*(", x_1s, ")")
    
    # Passing to appropriate form
    final_func <- function(x){
      a = eval(parse(text = form_approximated))
      return(a)
    }
    
    # Evaluating
    integ_approximations[i] <- composite_approximator(final_func, 0, 365, 5000)
  }
  
  return(integ_approximations)
  
}

#################################################

# Forward pass function
forward_fnn <- function(neural_net, nobs) {
  
  for (i in 1:nobs) {
    
    # Layer 1 activations
    neural_net$layer1[i] <- sum(integral_form(temp_fd$coefs[,i])*neural_net$layer_weights_1) + 
      neural_net$layer_bias_1

  }
  
  # Output activations
  neural_net$output <- c(neural_net$layer1 * neural_net$layer_weights_2 + 
                           layer_bias_2)
  
  return(neural_net)
}

#################################################



# Backward pass function
backward_fnn <- function(neural_net, nobs, learn_rate, gamma){
  
  # Initializing
  deriv_weights1 <- matrix(ncol = 11, nrow = 34)
  
  # Weight - Layer 1
  for (i in 1:nobs) {
    
    # Layer 1 activations
    deriv_weights1[i,] <- 2*(neural_net$y - 
                               neural_net$output)[i]*integral_form(temp_fd$coefs[,i])*layer_weights_2
    
  }
  
  # Weight - Layer 2
  deriv_weights2 <- 2*(neural_net$y - neural_net$output)*neural_net$layer1
  
  # Bias - Layer 1
  deriv_bias1 <- 2*(neural_net$y - neural_net$output)*layer_weights_2*neural_net$layer1
  
  # Bias - Layer 2
  deriv_bias2 <- 2*(neural_net$y - neural_net$output)
  
  # Adaptive learning rate
  if(learn_rate < 6){learn_rate = 0.1/gamma}
  if(learn_rate >= 6){learn_rate = 0.05/gamma}
  if(learn_rate >= 11){learn_rate = 0.001/gamma}
  if(learn_rate > 20){learn_rate = 0.0001/gamma}
  if(learn_rate > 30){learn_rate = 0.00001/gamma}

  # Applying updates
  for (i in 1:11) {
    # Layer 1 activations
    neural_net$layer_weights_1[i] <- neural_net$layer_weights_1[i] + learn_rate*mean(c(deriv_weights1[,i]))
    
  }
  
  neural_net$layer_weights_2 <- neural_net$layer_weights_2 + learn_rate*c(mean(deriv_weights2))
  neural_net$layer_bias_1 <- neural_net$layer_bias_1 + learn_rate/10*c(mean(deriv_bias1))
  neural_net$layer_bias_2 <- neural_net$layer_bias_2 + learn_rate/10*c(mean(deriv_bias2))
  
  # Returning updated information
  return(neural_net)
  
}

#################################################

FNN_weather <- function(epochs, print_iter = T, gamma = 1){
  
  # MSE Function
  MSE <- function(neural_net) {
    return(mean((neural_net$y - neural_net$output)^2))
  }
  
  # Initializing loss vector
  lossData <- data.frame(epoch = 1:epochs, MSE = rep(0, epochs))
  
  # Training
  for (f in 1:epochs) {
    
    # print("layer 1")
    # print(fnn_net$layer_weights_1)
    # print(fnn_net$layer_bias_1)
    # 
    # print("layer 2")
    # print(fnn_net$layer_weights_2)
    # print(fnn_net$layer_bias_2)
    
    # Foreward iteration
    fnn_net <- forward_fnn(fnn_net, 34)
    
    # Backward iteration
    fnn_net <- backward_fnn(fnn_net, 34, 3, gamma)
    
    # Storing loss
    lossData$MSE[f] <- MSE(fnn_net)
    
    if(print_iter == T){
      # Printing
      print(paste0("Iteration ", f, ": Done"))
      print(paste0("Current Loss: ", lossData$MSE[f]))
      # Current Loss
      suppressForeignCheck(print(ggplot(lossData[1:f,], aes(x = epoch, y = MSE)) +
                                   geom_point(color = "blue") +
                                   geom_line() +
                                   theme_bw() +
                                   labs(x = "Epoch Number", y = "Loss", title = "Iterative Loss Plot - FNN") +
                                   theme(plot.title = element_text(hjust = 0.5))))
    }
    
  }
  
  # Returning trained network
  return(fnn_net)
  
}

#################################################

# Here is an act on the fnn object to get the coefficient function
neural_coef <- function(fnn_object, print_iter = T){
  
  # Pulling out coefficients
  neural_weights <- fnn_object$layer_weights_1
  
  # Crossing out weights
  neural_weights <- ifelse(neural_weights < 0.5, 0, neural_weights)

  # Creating function
  final_beta_lm <- function(x, c){
    value <- c[1] + c[2]*sin(1*2*pi/365*x) + c[3]*cos(1*2*pi/365*x) +
      c[4]*sin(2*2*pi/365*x) + c[5]*cos(2*2*pi/365*x) + c[6]*sin(3*2*pi/365*x) +
      c[7]*cos(3*2*pi/365*x) + c[8]*sin(4*2*pi/365*x) + c[9]*cos(4*2*pi/365*x) +
      c[10]*sin(5*2*pi/365*x) + c[11]*cos(5*2*pi/365*x)
    return(value)
  }
  
  # Getting updated function
  beta_coef_fnn <- data.frame(time = seq(1, 365, 1), 
                              beta_evals = final_beta_lm(seq(1, 365, 1),
                                                         neural_weights))
  
  # Printing function
  if(print_iter == T){
    print(beta_coef_fnn %>% 
            ggplot(aes(x = time, y = beta_evals)) +
            theme_bw() +
            geom_line() +
            ggtitle("Functional Neural Coefficient Function") +
            xlab("Time") +
            ylab("beta(t)") +
            theme(plot.title = element_text(hjust = 0.5))) +
            ylim(c(-3.5, 3.5))
  }
  
  
  # ggplot return
  plot_to_return <- beta_coef_fnn %>% 
    ggplot(aes(x = time, y = beta_evals)) +
    geom_line(size = 1.5) +
    theme_bw() +
    ggtitle("Functional Neural Coefficient Function") +
    xlab("Time") +
    ylab("beta(t)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(c(-3.5, 3.5))
  
  return(plot_to_return)
  
}


#################################################

# Here is an act on the fnn object to get the coefficient weights
neural_coef_weights <- function(fnn_object, print_iter = T){
  
  # Pulling out coefficients
  neural_weights <- fnn_object$layer_weights_1
  
  # Returning
  return(neural_weights)
}

#################################################

# Here are the iterations to look at different convergences
fnn_neural_iterations <- function(seed_num){
  
  # Creating function
  final_beta_lm <- function(x, c){
    value <- c[1] + c[2]*sin(1*2*pi/365*x) + c[3]*cos(1*2*pi/365*x) +
      c[4]*sin(2*2*pi/365*x) + c[5]*cos(2*2*pi/365*x) + c[6]*sin(3*2*pi/365*x) 
      c[7]*cos(3*2*pi/365*x) + c[8]*sin(4*2*pi/365*x) + c[9]*cos(4*2*pi/365*x) +
      c[10]*sin(5*2*pi/365*x) + c[11]*cos(5*2*pi/365*x)
    return(value)
  }
  
  # Setting seed
  set.seed(1994*seed_num)
  current_seed <- 1994*seed_num
  
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
  
  # Saving plots
  plot_list <- list()
  
  for (k in 1:9) {
    if(k == 1){
      
      # Getting original weight function
      beta_coef_fnn <- data.frame(time = seq(1, 365, 1), 
                                  beta_evals = final_beta_lm(seq(1, 365, 1),
                                                             fnn_net$layer_weights_1))
      
      # Putting in plot
      plot_list[[k]] <- beta_coef_fnn %>% 
        ggplot(aes(x = time, y = beta_evals)) +
        theme_bw() +
        ggtitle("Functional Neural Coefficient Function") +
        xlab("Time") +
        ylab("beta(t)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylim(c(-3.5, 3.5))
      
    } else{
      
      # Running model
      model <- FNN_weather(k, print_iter = F, gamma = 0.25)
      
      # Putting in plot
      plot_list[[k]] <- neural_coef(model, print_iter = F)
    }
    
  }
  
  # Binding together
  final_ds <- data.frame()
  for (j in 1:9) {
    plot_list[[j]]$data$iteration = j
    final_ds <- rbind(final_ds, plot_list[[j]]$data)
  }
  
  # Plot 1
  plot_return <- ggplot(data = final_ds, aes(x = time, y = beta_evals, color = as.factor(iteration))) + 
    theme_bw() +
    labs(x = "Time", y = "beta(t)", title = paste0("Random Seed: ", current_seed)) +
    geom_line() +
    guides(color=guide_legend(title="Epoch Number")) + 
    scale_color_brewer(palette = "Set1") + 
    theme(legend.position = "none")
  
  # Returning
  return(plot_return)
  
}

#################################################

# Here, I am predicting
fnn_pred <- function(fnn_object, new_obs = "none"){
  
  if(new_obs == "none"){
    
    # sMSE
    FunctionalNN_MSE <- mean((fnn_object$output - annualprec[-35])^2)
    return(FunctionalNN_MSE)
    
  } 
  
  # MSPE
  
  pred_l1 <- sum(integral_form(new_obs)*fnn_object$layer_weights_1) + 
                 fnn_object$layer_bias_1
  
  pred_l2 <- c(pred_l1*fnn_object$layer_weights_2 + 
                 fnn_object$layer_bias_2)
  
  return(pred_l2)
  
}


