#################################################
############# Simulatiton Studies ###############
##### Barinder Thind & Jiguo Cao ################
#### Functional Neural Networks #################
#################################################

##### Libraries
library(fda)
library(fda.usc)

#################################################

### Let's first create a function, x_i(t)
sophmore_dream <- function(s, r, alpha, k){
  value <- alpha + sin(k^(s*k))*(s^{-s*r})
}

### Evaluating function
soph_data <- data.frame(s = seq(0, 1, 0.01), soph_value = sophmore_dream(seq(0, 1, 0.01), -2, alpha = 0, k = 2))
  
### Plotting function values
plot(soph_data$s, soph_data$soph_value, type = "l")

### Plotting beta
plot(soph_data$s, sin(2^(seq(0, 1, 0.01)*2)))

### Test integrating this function
integrate(sophmore_dream, r = 1, alpha = 0, k = pi, lower = 0, upper = 1)[1]

### Generating integral values
y <- c()
r_values <- c()
alpha_values <- c()
for (i in 1:150) {
  
  # generating r
  val_r = rnorm(1, 1, 0.9)
  
  # storing r
  r_values[i] <- val_r
  
  # generating alpha
  val_alpha = rnorm(1, 0, 0.9)
  
  # storing alpha
  alpha_values[i] <- val_alpha
  
  # Integrating to get y
  y[i] <- integrate(sophmore_dream, r = val_r, alpha = val_alpha, k = 1.5, lower = 0, upper = 1)[1]
  
}

### Turning into vector
y <- unlist(y)

### Adding noise
y_star <- y[1:100] + rnorm(100, 0, 0.1)

### Generating data for each of observations
sophmore_obs <- matrix(nrow = 150, ncol = 100)
for (j in 1:150) {
  for (i in 1:100) {
    sophmore_obs[j, i] <- sophmore_dream(seq(0, 100, 0.01)[i], 
                                         r = r_values[j],
                                         alpha = alpha_values[j],
                                         k = 2)
    }
}



##### Creating functional observations #####

### Creating fourier basis
sophmore_basis <- create.fourier.basis(c(0, 0.99), 65)
sophSmooth <- smooth.basis(seq(0, 0.99, 0.01), 
                           t(sophmore_obs[1:100,]), 
                           sophmore_basis)

### Plotting
plot(sophSmooth$fd)

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
