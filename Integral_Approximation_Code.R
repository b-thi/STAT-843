### Creating basis
library(fda)

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

# Creating simpsons rule function
composite.simpson <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / n
  
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  
  approx <- (h / 3) * (f(a) + 2 * sum(f(xj[seq.int(2, length(xj), 2)])) + 4 * sum(f(xj[seq.int(1, length(xj), 2)])) + f(b))
  
  return(approx)
  
}

### First, I create formulas for the functional observation, x(t)

## Initializing
updated_names <- NULL
updated_internal_name <- NULL

## Looping to create coefficients
for (i in 1:length(temp_fd$coefs[1,])) {
  for (j in 1:length(temp_fd$basis$names)) {
    if(temp_fd$basis$names[j] == "const"){
      updated_internal_name <- paste0(updated_internal_name, " + ", temp_fd$coefs[j,i])
    } else {
      updated_internal_name <- paste0(updated_internal_name, " + ", temp_fd$coefs[j,i], "*", temp_fd$basis$names[j])
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
for (i in 1:length(betabasis5$names)) {
  updated_beta_names <- paste0(betabasis5$names)
}

### Next, I put together the previous results

## Cleaning up data set after expanding
basis_combinations = expand.grid(updated_beta_names, updated_names[1])
basis_combinations = basis_combinations[basis_combinations$Var1 != "const", ]
basis_combinations = basis_combinations[basis_combinations$Var2 != "const", ]

## Initializing updated form
final_form <- NULL

### Now, getting proper combinations
for (i in 1:length(basis_combinations$Var1)) {
  final_form[i] <- paste0(basis_combinations[i, 1], "*", "(", updated_names[1], ")")
}

### Replacing sin1, cos1 type of things
for (j in 1:length(basis_combinations$Var1)) {
  for (i in 1:100) {
    if(grepl(paste0("sin", 100 - i), final_form[j])){
      final_form[j] <- gsub(paste0("sin", 100 - i), paste0("sin", "(", 100 - i, "*", "x", ")"), final_form[j])
    }
    if(grepl(paste0("cos", 100 - i), final_form[j])){
      final_form[j] <- gsub(paste0("cos", 100 - i), paste0("cos", "(", 100 - i, "*", "x", ")"), final_form[j])
    }
  }
}

final_form[2]

### Here is a test function to see if everything works properly
test_func <- function(x){
  a = eval(parse(text = final_form[2]))
  return(a)
}

test_func(3)

composite.simpson(test_func, 0, 365, 5000)


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
        final_form[j] <- gsub(paste0("sin", 100 - i), paste0("sin", "(", 100 - i, "*", "365", "*", "x)"), final_form[j])
      }
      if(grepl(paste0("cos", 100 - i), final_form[j])){
        final_form[j] <- gsub(paste0("cos", 100 - i), paste0("cos", "(", 100 - i, "*", "365", "*", "x)"), final_form[j])
      }
    }
  }
  
  ## Taking care of const
  const_calc <- updated_names[k]
  for (i in 1:100) {
    if(grepl(paste0("sin", 100 - i), const_calc)){
      const_calc <- gsub(paste0("sin", 100 - i), paste0("sin", "(", 100 - i, "*", "365", "*", "x)"), const_calc)
    }
    if(grepl(paste0("cos", 100 - i), const_calc)){
      const_calc <- gsub(paste0("cos", 100 - i), paste0("cos", "(", 100 - i, "*", "365", "*", "x)"), const_calc)
    }
  }
  
  # Creating function
  const_func <- function(x){
    a = eval(parse(text = const_calc))
    return(a)
  }
  
  # Doing integral
  const_int <- composite.simpson(const_func, 0, 365, 50000)
  
  # Initializing
  integral_evals <- c()
  
  # Looping to get all evals
  for (r in 1:length(basis_combinations$Var1)) {
    
    final_func <- function(x){
      a = eval(parse(text = final_form[r]))
      return(a)
    }
    
    integral_evals[r] <- composite.simpson(final_func, 0, 365, 5000)
  }
  
  # Putting together
  final_results <- c(const_int, integral_evals)
  
  final_results <- c(scale(final_results))
  
  return(final_results)
  
}

fda_integ_approximator(betabasis5, temp_fd, 35)

