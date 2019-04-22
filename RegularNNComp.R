### Neural Network Comparison
library(fda)
annualprec = apply(daily$precav,2,mean)
## Creating data frame
temp_prec_df <- data.frame(cbind(annualprec, t(daily$tempav)))
str(temp_prec_df)

## Loading package
library(neuralnet)

## Scaling data
temp_prec_df_scaled <- as.data.frame(scale(temp_prec_df))

## Running neural network
NN = neuralnet(annualprec ~ ., 
                temp_prec_df_scaled, 
                hidden = 1 , 
                linear.output = T,
               rep = 10)

## Looking at error rate
NN$result.matrix[1]
