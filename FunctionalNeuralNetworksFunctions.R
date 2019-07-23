#------------------------------------------------------------------------------#
#                                                                              #
#  June c(28th, 30th), 2019                                                    #
#  July c(18th, 21st, 22nd), 2019                                              #
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

# Non-Linear (activation) Function Maker - DONE (Tentative)
# Basis Selector Function
# Gaussian Quadrature/Composite Function - DONE (Tentative)
# Integral Approximator Function
# Forward Pass Function - DONE (Tentative)
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

basis_selector <- function(type, numbasis, range_val, norder) {
  
  # This function is called in the overall FNN function as a part of the 
  # integral approximator function. This helps define the integrals to be 
  # computed by the composite approximator function below - basically, we 
  # need to define this so we can pull out those values which we update
  # later as a part of the network.
  
  # Getting range difference
  range_diff <- range_val[2] - range_val[1]
  
  # First, we pick the the type
  
  # Fourier Code
   if (type == "fourier") {

    basis_used <- create.fourier.basis(rangeval = range_val, nbasis = numbasis)
    basis_functions <- basis_used$names
    
    # Here, I loop through the basis function names and make them into a proper
    # kind of order, so for example, i take sin1 and make it into the format that
    # it would be required to be so that I can approximate the integral when
    # multiplied by the functional observation!! I need to fix the constant term
    # here still - will talk to Jiguo about this
    for (i in 1:length(basis_functions)) {
      
      ## Need to figure out this constant situation
      if (i == 1) {
        basis_functions[i] <- "1"
      }

       if (i != 1 & is.na(str_extract(basis_functions[i], "cos")) == FALSE) {
           basis_functions[i] <- paste0(str_extract(basis_functions[i], "cos"),
                                        "(", stri_extract_first_regex(basis_functions[i], "[0-9]+"),
                                       "*", range_diff, "*x)")
       }
  
       if (i != 1 & is.na(str_extract(basis_functions[i], "sin")) == FALSE) {
         basis_functions[i] <- paste0(str_extract(basis_functions[i], "sin"),
                                      "(", stri_extract_first_regex(basis_functions[i], "[0-9]+"),
                                      "*", range_diff, "*x)")
       }
     }
  
   }
  
  # B-Spline Code
  if (type == "b-spline"){
    basis_used <- create.bspline.basis(rangeval = rangeval, nbasis = numbasis,
                                       norder = n_order)
    }
  
  return(basis_functions)
  
}


# test run
basis_selector("fourier", 65, c(0, 365), 3)

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

fnn_integ_approximator <- function(basis, obs, which_func_obs, func_obs, term, 
                                   num_basis, range_val, n_order, subintervals) {
  
  # This function approximates an integral, specifically of: int phi_i(s)*x(s)
  # and of course, the $phi_i$ here depends on which term of the functional
  # obseration we are on. In order to do this, I will first seperate this function
  # into the two types of basis here and then work to create a big list of the
  # proposed basis expansion, just getting purely the function associated with it.
  # Then, I will pull out the term we are on using the call to this function on
  # the parameters.
  
  # Note here that the func_obs refers to which functional co-variate we are
  # at in the process
  
  # First, let's split on fourier vs. b-splines as b-splines will be handled 
  # a bit differently
  
  if (basis == "fourier") {
    
    # First, I call the basis selector function to get the proper list of the
    # functions we need. This call returns to us the list of the separate terms of
    # the basis substitution of the weight function
    basis_list <- basis_selector(basis, num_basis, range_val, n_order)
    
    # Now, we have the list, we can pick out the specific term
    current_func <- basis_list[term]
    
    # Now, let's pass on an actual functional observation to this function and
    # see how it performs. Here, I will take all the relevant information from the
    # functional observation and extract it and ultimately build the character string
    # of the final function we will try an approximate
    
    # Let's scale the coefficients
    func_obs$coefs <- scale(func_obs$coefs)
    
    # First, pull out the coefficients
    a = as.character(func_obs$coefs[,1])
    
    # Then, I recreate the basis that made the original functional observation
    b = basis_selector(func_obs$basis$type, 65, range_val = temp_fd$basis$rangeval, norder = 100)
    
    # Then I initialize
    c = c()
    
    # Now, I have created the functional observation below
    for (i in 1:nrow(func_obs$coefs)) {
      if(i < 65){
        c = paste0(c, a[i], "*", b[i], " + ")
      } else {
        c = paste0(c, a[i], "*", b[i])
      }
    }
    
    # Now, I will put together the basis function we are at and multiply it by the 
    # functional observation we just created above
    func_to_approx <- paste0(current_func, "*(", c, ")")
    
    # Now we can create the function that will be passed on
    final_func <- function(x){
      a = eval(parse(text = func_to_approx))
      return(a)
    }
    
    # Now, let's do the integration by calling the composite function defined before
    integral_value <- composite_approximator(final_func, range_val[1], range_val[2], subintervals)
    
    
  }
  
  if (basis == "b-spline"){
    
  }
  
  return(integral_value)
  
  
}

fnn_integ_approximator("fourier", 1, 1, temp_fd[1], 6, 7, range_val = c(0, 365), 3, subintervals = 1000)


S#------------------------------------------------------------------------------#
#                                                                              #
#  FNN - Forward Pass                                                          #
#                                                                              #
#------------------------------------------------------------------------------#

forward_fnn <- function(neural_net, nobs, neurons, layer, activation_func, 
                        end_layer, scalar_inputs, functional_inputs, number_func,
                        num_basis, range_val, n_order, type_basis) {
  
  # So, here we first pick which layer we are at - since clearly the first layer 
  # and the rest of the layers are different from one another, I make that separation
  # here. Now, in this this layer, I declare the result, which will give us the
  # value of the j^th neuron for the i^th observation and sub result, which will
  # give us the value of the approximated integral of the u^th basis function and
  # the functional observation which is then multiplied by the coefficient, c_i
  # which comes from the previously declared weights of the network. Now, to make
  # it clear here: i is the i^th observation, j is the number of neurons in the current
  # layer that we are in (in this case, the first one), k is the number of functional
  # covariates in the network, and finally, u is the number of basis functions declared
  # for that particular functional covariate. The fnn approximator will approximator
  # a single instance for a particular m^th coefficient of the basis function multiplied
  # by the approximated integral of the particular basis term we are at multiplied by
  # the functional observation
  
  # Initializing locally
  current_layer_activations <- c()
  
  # Now I separate based on what layer we are at

  if (layer == 1) {
    
    # Initializing
    result = c()
    sub_result = c()
    neural_net$layer1 <- matrix(nrow = nobs, ncol = neurons[1])
    number_func = 1 ########### FIX AND FIX LAYER SITUATION TOO
    
    # Loops as described above
    for (i in 1:nobs) {
      
      for (j in 1:neurons[1]) {
        
        for (k in 1:number_func) {
          
          for (u in 1:num_basis[k]) { # u is like m here
            
            # Saving parts of the basis expansion
            sub_result[u] <- fnn_integ_approximator(type_basis[k], i, k, functional_inputs[[k]][[i]], u,
                                                    num_basis, range_val, n_order, subintervals = 3500)*neural_net$functional_weights_fnn[[k]][i, u, j]
            
          }
          
          result = activation_func(sum(sub_result) + 
                                     sum(neural_net$scalar_weights_fnn_l1[i, k, j]*scalar_inputs[i,]) + 
                                     neural_net$bias_fnn_l1[i, j])
          
          
        }
        
        neural_net$layer1[i, j] = result
        
      }
    }
  
  }
  
  # Here, we are now past that scary first layer and into the rest of the network
  # which runs exactly as you would expect. I first save the old layer activations
  # just to have them in a seperate object in the neural network and then declare
  # which are the current input into the layer. It is layer 1 if we are at layer 2
  # and it is the previous layer output if we are at the other layers. I declare
  # the matrix before proceeding. Now, in the for loop below, again the index
  # i refers to the observation we are considering and j here refers to the neurons
  # in the current layer. So here, the result is a matrix, with the columns corresponding
  # to the neuron values for each observation i in the current layer
  if (layer != 1) {
    
    # Initializing
    neural_net$old_layer = neural_net$layer_output

    if (layer == 2){
      current_layer_activations <- scale(neural_net$layer1) ########
      print(current_layer_activations)
    } else {
      current_layer_activations <- scale(neural_net$layer_output) #######
    }

    neural_net$layer_output <- matrix(nrow = nobs, ncol = neurons[layer])

    for (i in 1:nobs) {
      for (j in 1:neurons[layer]) {
        neural_net$layer_output[i, j] = activation_func(sum(current_layer_activations*neural_net$scalar_weights_fnn_rest[[layer]][i, ,j]) +
          neural_net$bias_fnn_rest[[layer]][i, ,j])
      }
    }
  }
  
  # Here is the code for the last layer using the final activation function, this is
  # specific because we will end up with a final prediction here going into a single neuron
  # This may not work right now, we will need to check this layer, or I can check it now
  # actually. Let's see
  # if(layer == end_layer){
  #   
  #   # First, we get the activations
  #   current_layer_activations <- neural_net$layer_output
  #   
  #   # Now, we multiply this by the final weights and bias
  #   for (i in 1:nobs) {
  #     print(current_layer_activations)
  # 
  #     neural_net$predictions[i] = activation_func(sum(as.vector(current_layer_activations)*c(neural_net$scalar_weights_fnn_rest[[end_layer]][i, , 1]))
  #                                                 + neural_net$bias_fnn_rest[[end_layer]][i, , 1])
  #   }
  #   
  # }
  
  
  return(neural_net)
  
}

length(c(temp_fd))


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
                scalar_inputs, functional_inputs, 
                response, activation_function,
                basis_type, num_basis, range_val, n_order = 100,
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
  number_obs <- nrow(scalar_inputs) ##########
  nobs <- number_obs
  
  # Get number of scalar covariates 
  number_scalar <- ncol(scalar_inputs) ##########
  
  # Number of functional covariates
  number_func <- ncol(functional_inputs) ##########
  
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
  
  # Next, we do the same for bias. Here, for the bias ofx this first layer
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
  
  ################### Everything Works Up Until Here (1) ###################
  
  # Next, we set the response. In this case, it will just come from the user
  # and we will save it here as "y". This is for scalar responses so this will
  # be a vector of size = to i, the number observations
  y = response
  
  # Now, we create an empty matrix to store all the responses - this will be
  # checked with the response above to see whether or not we have a good error
  # rate or not
  output = matrix(rep(0, number_obs), ncol = number_obs)
  
  # Now, we can create a list which holds all the above information so far in
  # one nice form. This list has the weights and the predictions in it - this is
  # essentially the whole network and this list is what will get updated through
  # out the whole network
  neuralnet_info <- list(
    
    # Input observations
    #input_functional <- functional_inputs, ##########
    #input_scalar <- scalar_inputs, ##########
    
    # Functional weights - layer 1
    functional_weights_fnn = functional_weight_list,
    
    # Scalar weights - layer 1
    scalar_weights_fnn_l1 = scalar_weights_1,
    
    # Bias - layer 1
    bias_fnn_l1 = layer1_bias,
    
    # Scalar Weights - layer 2 to end
    scalar_weights_fnn_rest = weights_rest,
    
    # Bias - layer 2 to end
    bias_fnn_rest = bias_rest,

    # Output
    true_values = y,
    
    # Predictions
    predictions = output
  )
  
  ################### Everything Works Up Until Here (2) ###################
  
  # Now, let's make sure our activation function works here by selecting it
  # and renaming it as appropriate
  activation <- activation_selector(type = activation_function)
  
  # Now, let's try out the forward pass function
  # Forward iteration
  for (i in 1:layers) {
    neuralnet_info <- forward_fnn(neuralnet_info, 
                                  number_obs, 
                                  neurons = neurons,
                                  layer = i, 
                                  activation_func = activation,
                                  end_layer = layers, 
                                  scalar_inputs = scalar_inputs,
                                  functional_inputs = functional_inputs,
                                  number_func = number_func, 
                                  num_basis = num_basis,
                                  range_val = range_val, 
                                  n_order = n_order,
                                  type_basis = basis_type)
    
    if (i == 1){
      print(neuralnet_info$layer1)
    }
    
    if (i > 1){
      print(neuralnet_info$layer_output)
    }
  }
  
  ################### Everything Works Up Until Here (3) ###################
  
  # Now, we call the forward pass function so that we can get a pass
  # of the network before we go back and update the network using the
  # backward pass function. This will be done for some number of epochs
  # which we will loop over
  
  # Loss Function
  MSE <- function(neural_net) {
    return(mean((neural_net$true_values - neural_net$predictions)^2))
  }
  
  # # Training Neural Net
  # for (f in 1:epoch_num) {
  #   
  #   # Foreward iteration
  #   neuralnet_info <- forward_fnn(neuralnet_info, 34)
  #   
  #   # Backward iteration
  #   neuralnet_info <- backward_fnn(neuralnet_info, 34, f)
  #   
  #   # Storing loss
  #   lossData$MSE[f] <- MSE(neuralnet_info)
  #   
  # }

  
  # Creating list to return
  return_list <- list(network = neuralnet_info)
  
  return(return_list)
}

# testing 1 - this may break
test = fnn(layers = 3, neurons = c(5, 2, 3), num_basis = c(6, 7))
test$network$

# testing 2 - this may break
test = fnn(layers = 3, neurons = c(5, 2, 3), num_basis = c(6, 7), response = c(rep(0, 10)))

# tetsing 3 - this may break

# creating inputs
scalars = data.frame(precip = rnorm(35), humid = rnorm(35))
functionals = list()
for (i in 1:nrow(scalars)) {
  functionals[[i]] = temp_fd[i]
}
functionals2 = list(functionals)


test = fnn(layers = 7, 
        neurons = c(4, 2, 2, 3, 2, 2, 1), 
        scalar_inputs = scalars, 
        functional_inputs = functionals2,
        response = 3, 
        c("Identity"),
        basis_type = c("fourier"), 
        num_basis = c(13), 
        range_val = c(0, 365), 
        n_order = 100,
        epoch_num = 5,
        datasplit_percent = 10,
        learn_rate = "dynamic",
        optimizer = "sgd",
        loss_function = "mse",
        weight_initialization = "uniform")

mean(test$network$layer_output - scale(apply(daily$precav, 2, mean)))^2


#------------------------------------------------------------------------------#
#                                                                              #
# Diagnostics Function                                                         #
#                                                                              #
#------------------------------------------------------------------------------#

fnn_diagnostics <- function(fnn_object) {
  
  
  
}





