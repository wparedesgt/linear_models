#### Matrix Algebra Examples Exercises

library(tidyverse)
library(dslabs)
library(rafalib)

X <- matrix(c(1,1,1,1,0,0,1,1), nrow = 4)
rownames(X) <- c("a", "a", "b", "b")

X

beta <- c(5,2)

# Extract rows for group A
X_A <- X[1:2, ]

# Calculate fitted values for group A
fitted_A <- X_A %*% beta
fitted_A

# Extract rows for group B
X_B <- X[3:4, ]

# Calculate fitted values for group B
fitted_B <- X_B %*% beta
fitted_B
