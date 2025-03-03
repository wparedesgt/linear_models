### Quiz

library(tidyverse)
library(rafalib)
library(dslabs)
library(UsingR)

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

X

beta <- c(10,3,-3)

RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# Calculate fitted values
fitted_values <- X %*% beta

# Display all fitted values
fitted_values

# Extract the fitted values for B samples
B_samples <- fitted_values[rownames(X) == "b"]
B_samples

###############

library(UsingR)

x <- father.son$fheight
y <- father.son$sheight
n <- length(y)


N <-  50
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
betahat <-  lm(y~x)$coef

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
# Run Monte Carlo simulation with 10,000 replications
betahat_samples <- replicate(10000, {
  index <- sample(n, N)
  sampledat <- father.son[index,]
  x <- sampledat$fheight
  y <- sampledat$sheight
  lm(y~x)$coef
})

# The result is a 2xB matrix, where B is the number of replications
# First row contains intercept estimates, second row contains slope estimates
# Transpose to have estimates in columns
betahat_samples <- t(betahat_samples)

# Calculate standard error of the slope estimate (second column)
slope_se <- sd(betahat_samples[,2])
print(paste("Standard error of the slope estimate:", slope_se))


library(UsingR)
data(father.son)

# Extract father heights (X) and son heights (Y)
X <- father.son$fheight
Y <- father.son$sheight

# Calculate covariance using the formula mean((Y - mean(Y))*(X-mean(X)))
covariance <- mean((Y - mean(Y))*(X - mean(X)))

# Print the result
covariance

# We can also verify with the built-in cov function
cov(X, Y)



library(UsingR)
data(father.son)

# Extract father heights and son heights
x <- father.son$fheight  # father's height
y <- father.son$sheight  # son's height

# Original model: son's height as a function of father's height
fit1 <- lm(y ~ x)
slope1 <- fit1$coefficients[2]

# Reversed model: father's height as a function of son's height
fit2 <- lm(x ~ y)
slope2 <- fit2$coefficients[2]

# Print both slopes
print(paste("Original slope (son ~ father):", slope1))
print(paste("Reversed slope (father ~ son):", slope2))

# Calculate the relationship between the slopes
print(paste("Ratio of reversed to original slope:", slope2/slope1))
