#------------------------------------------------------------------------------#
#                                                                              #
#  June c(28th, 30th), 2019                                                    #
#  July c(18th, ), 2019                                                        #
#                                                                              #
#  Final Function Code - Functional Neural Networks                            #
#                                                                              #
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#                                                                              #
#  Libraries                                                                   #
#                                                                              #
#------------------------------------------------------------------------------#

library(tidyverse)
library(fda)
library(refund)
library(fda.usc)

#------------------------------------------------------------------------------#
#                                                                              #
#  Set Seed                                                                    #
#                                                                              #
#------------------------------------------------------------------------------#

set.seed(1994)

#------------------------------------------------------------------------------#
#                                                                              #
#  Functions List                                                              #
#                                                                              #
#------------------------------------------------------------------------------#

# Non-Linear (activation) Function Maker
# Basis Selector Function
# Gaussian Quadrature/Composite Function
# Integral Approximator Function
# Forward Pass Function
# Backpropagation Function
# Overall FNN Function
# Diagnostics/Plot Function

#------------------------------------------------------------------------------#
#                                                                              #
#  Non-Linear Function Maker                                                   #
#                                                                              #
#------------------------------------------------------------------------------#


activation_selector <- function(type) {
  
  # This function lets the user pick the activation function they would like
  # to use for the functional neural network. The function will be called equal
  # to the number of layers that the user has picked for the network. The user
  # passes a vector that is of equal size to the number of layers in the model.
  # This will be resolved in the general FNN function.
  
  # Identity function
  if(type == "Identity"){
    fn <- function(x){
      return(x)
    }
  }
  
  # ReLu function
  if(type %in% c("Relu", "ReLu")){
    fn <- function(x){
      return(max(0, x))
    }
  }
  
  # Sigmoid function
  if(type == "Sigmoid"){
    fn <- function(x){
      return(1/(1 + exp(x)))
    }
  }
  
  # Else
  if(!(type %in% c("Sigmoid", "Relu", "Identity"))){
    fn <- "Are you sure you picked a valid activation function? 
    If you think you did, you should check if things are appropriately 
    capitalized"
  }
  
  return(fn)
  
}

# Testrun
activation <- activation_selector(type = "Relu")
activation(-5)  

#------------------------------------------------------------------------------#
#                                                                              #
#  Basis Selector Function                                                     #
#                                                                              #
#------------------------------------------------------------------------------#

basis_selector <- function(type, numbasis, rangeval, norder) {
  
  # This function is called in the overall FNN function as a part of the 
  # integral approximator function. This helps define the integrals to be 
  # computed by the composite approximator function below - basically, we 
  # need to define this so we can pull out those values which we update
  # later as a part of the network.
  
}

#------------------------------------------------------------------------------#
#                                                                              #
#  Composite Function                                                          #
#                                                                              #
#------------------------------------------------------------------------------#

composite_approximator <- function(f, a, b, n) {
  
  # This function does the integral approximations and gets called in the
  # integral approximator function. In the integral approximator function
  # we pass in a function f into this and that is final output - a collection
  # of numbers - one for each of the functional observations
  
  # Error checking code
  if (is.function(f) == FALSE) {
    stop('The inpute f(x) must be a function with one parameter (variable)')
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

# test run
test_fn <- function(x){
  return(x)
}
composite_approximator(test_fn, 0, 2, 10)


#------------------------------------------------------------------------------#
#                                                                              #
#  FNN Integral Beta Approximator                                              #
#                                                                              #
#------------------------------------------------------------------------------#

fnn_integ_approximator <- function(basis, func_obs, nobs) {
  
  
}

#------------------------------------------------------------------------------#
#                                                                              #
#  Neural Network Function                                                     #
#                                                                              #
#------------------------------------------------------------------------------#

neural_network <- function(input, num_neurons, nobs) {
  
  # weights
  weights_layer <- array(dim = c(nobs, 
                                    length(input),
                                    num_neurons))
  
  bias_layer <- array(dim = c(number_obs,num_neurons))
  
}



#------------------------------------------------------------------------------#
#                                                                              #
#  FNN - Forward Pass                                                          #
#                                                                              #
#------------------------------------------------------------------------------#

forward_fnn <- function(neural_net, obs) {
  
  
}


#------------------------------------------------------------------------------#
#                                                                              #
#  FNN - Backward Pass                                                         #
#                                                                              #
#------------------------------------------------------------------------------#

backward_fnn <- function(neural_net, obs) {
  
  
}


#------------------------------------------------------------------------------#
#                                                                              #
#  Overall FNN Function                                                        #
#                                                                              #
#------------------------------------------------------------------------------#

fnn <- function(layers = 1, neurons = 1, 
                scalar_inputs, functional_inputs, activation_function,
                basis_type, num_basis, rangeval, norder = 100,
                epoch_num = 5,
                datasplit_percent = 10,
                learn_rate = "dynamic",
                optimizer = "sgd",
                loss_function = "mse",
                weight_initialization = "uniform") {
  
  # This is the overall fnn function. It calls basically all other functions in
  # this package and has a number of parameters to help the user tune the model
  # to their liking. I comment below the process which the function takes and
  # what function from above is called and how it is used here. In the end,
  # it outputs a list including the final model
  
  # Get number of observations
  number_obs <- 10#nrow(scalar_inputs)
  
  # Get number of scalar covariates
  number_scalar <- 3#ncol(scalar_inputs)
  
  # Number of functional covariates
  number_func <- 2#ncol(functional_inputs)
  
  # Initialize weights for the network
  functional_weight_list <- list()
  scalar_weights_1 <- array(dim = c(number_obs, 
                                    number_scalar,
                                    neurons[1]))
  layer1_bias <- array(dim = c(number_obs,
                               neurons[1]))

  # w_1(s), w_2(s), ..., w(s) Initial Weights
  # Here, we have a list of tensors - the list corresponds to a 
  # particular functional covariate, so for example, the first tensor
  # of the list corresponds to the functional observation. In each tensor
  # we have 3 indices - the row index, i, is the observation number and
  # the column index is the number of coefficients that define the 
  # beta coefficient function in the neuron - this varies obvious functional
  # covariate to functional coviarate (hence the list). Now, k, the slices of
  # the tensor are for the different neurons of the first layer of the network.
  # Ultimately, this results in all the initialized weights of the network
  # for the functional covariates
  for (u in 1:length(num_basis)) {
    functional_weights <- array(dim = c(number_obs, 
                                        num_basis[u], 
                                        neurons[1]))
    for (i in 1:number_obs) {
      for (j in 1:num_basis[u]) {
        for (k in 1:neurons[1]) {
          functional_weights[i, j, k] <- runif(1)
        }
      }
    }
    functional_weight_list[[u]] <- functional_weights
  }
  
  # Now, next let's create the list of non-functional covariates
  # Here, the i index is for the observation number and j is the index
  # for the particular scalar covariate. K is for the number of neurons
  # in this particular layer
  for (i in 1:number_obs) {
    for (j in 1:number_scalar) {
      for (k in 1:neurons[1]) {
        scalar_weights_1[i, j, k] <- runif(1)
      }
    }
  }
  
  # Next, we do the same for bias. Here, for the bias of this first layer
  # we basically have a bias for each neuron in the first layer and one
  # for each observation. So, in this first layer, i is the observation and
  # k is the neuron number
  for (i in 1:number_obs) {
    for (k in 1:neurons[1]) {
      layer1_bias[i, k] <- runif(1)
    }
  }
  
  # Now, we can initialize weights for the rest of the network. We need
  # to get all the weights as part of some list and the length of the list
  # is equal to the number of layers
  
  # Initializing weight list
  weights_rest <- list()
  
  # Now, in this loop, the index i, rows, is again the observations of the
  # data set. The index j, which is the columns, is the number of weights
  # or coefficients here we need to initialize, you can see that the size of
  # this will be equal to the number of neurons in the previous layer and hence
  # we have [u - 1] in this case. Now, the third one will give us all
  # the weights in the particular layer. So k is going to be the number of
  # neurons and the matrix of weights exists for each neuron. Lastly, the 
  # u index is for a particular layer. The layers btw correspond to the
  # number of hidden layers all the way until the last HIDDEN layer. So, 2
  # to the last layer will give us all the values in the network right before
  # the last activation function
  for (u in 2:layers) {
    
    # Initializing
    weights_layer <- array(dim = c(nobs, 
                                   neurons[u - 1],
                                   neurons[u]))
    
    # Putting in random weights
    for (i in 1:nobs) {
      for (j in 1:neurons[u - 1]) {
        for (k in 1:neurons[u]) {
          weights_layer[i, j, k] <- runif(1)
        }
      }
    }
    
    weights_rest[[u]] <- weights_layer
    
  }
  
  # Initializing bias list
  bias_rest <- list()
  
  # In this loop, we initialize the biases of the rest of the network. So,
  # i, the row index is the particular observations and j, the column, is
  # equal to 1 because there is only 1 bias. k is the number of neurons
  # in the current layer so we can have the corrent number of basis for
  # each observation and for each layer. Lastly, again u corresponds to
  # the layer we are on
  for (u in 2:layers) {
    
    # Initializing
    bias_layer <- array(dim = c(nobs, 
                                   1,
                                   neurons[u]))
    
    # Putting in random weights
    for (i in 1:nobs) {
      for (j in 1:1) {
        for (k in 1:neurons[u]) {
          bias_layer[i, j, k] <- runif(1)
        }
      }
    }
    
    bias_rest[[u]] <- bias_layer
    
  }
  
  ################### Everything Works Up Until Here ###################
  

  
  # Creating list to return
  return_list <- list(functional_weight_list, weights_rest, scalar_weights_1,
                      layer1_bias, bias_rest)
  
  return(return_list)
}

# testing 1 - this may break
test = fnn(layers = 3, neurons = c(5, 2, 3), num_basis = c(6, 7))
test

# testing 2

#------------------------------------------------------------------------------#
#                                                                              #
# Diagnostics Function                                                         #
#                                                                              #
#------------------------------------------------------------------------------#

fnn_diagnostics <- function(fnn_object) {
  
  
  
}




# Now, we create the neural network list - this will be called repeatedly
# through the network
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


