## Homework 
##Standard Error

library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)

x <- father.son$fheight
y <- father.son$sheight
n <- length(y)
N <- 50
set.seed(1)
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
betahat <- lm(y~x)$coef

fit <- lm(y~x)
fit$fitted.values

Y <- fit$fitted.values

residuals <- y-Y
SSR <- sum(residuals^2)

p <- 2

sigma_squared <- SSR/(N-p)

# Create design matrix
X <- cbind(rep(1, N), x)

# Calculate X'X
XtX <- t(X) %*% X

# Calculate (X'X)⁻¹
XtX_inv <- solve(XtX)

# Element in first row, first column
XtX_inv[1,1]



# Get diagonal elements of (X'X)⁻¹
diag_XtX_inv <- diag(XtX_inv)

# Calculate variances
var_beta <- sigma_squared * diag_XtX_inv

# Calculate standard errors
se_beta <- sqrt(var_beta)

# Standard error for the slope (second element)
se_slope <- se_beta[2]
se_slope

