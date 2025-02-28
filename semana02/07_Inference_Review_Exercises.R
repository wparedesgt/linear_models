#### Inference Review Exercises

library(tidyverse)
library(dslabs)
library(rafalib)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

g <- 9.8 ## meters per second
h0 <- 56.67
v0 <- 0
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
y <- h0 + v0 *tt  - 0.5* g*tt^2 + rnorm(n,sd=1)


X <- cbind(1,tt,tt^2)
A <- solve(crossprod(X))%*%t(X)

A

# The LSE coefficients are A%*%y
# And g = -2*β₂, where β₂ is the coefficient for t²
# This means g = -2 * (A%*%y)[3]
# Or equivalently: g = -2 * (A %*% y)[3,1]

set.seed(1)

# Define parameters
g <- 9.8      # meters per second
h0 <- 56.67   # initial height
v0 <- 0       # initial velocity
n <- 25       # number of observations
tt <- seq(0, 3.4, len=n)  # time points

# Function to generate data and compute estimate of g
estimate_g <- function() {
  # Generate data
  y <- h0 + v0 * tt - 0.5 * g * tt^2 + rnorm(n, sd=1)
  
  # Design matrix
  X <- cbind(1, tt, tt^2)
  
  # Compute A matrix
  A <- solve(crossprod(X)) %*% t(X)
  
  # Compute coefficients and extract g
  coefs <- A %*% y
  g_est <- -2 * coefs[3]
  
  return(g_est)
}


# Set seed as instructed
set.seed(1)

# Perform 100,000 simulations
g_estimates <- replicate(100000, estimate_g())

# Calculate standard deviation
sd_g <- sd(g_estimates)
sd_g
