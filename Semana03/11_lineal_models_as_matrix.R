#### Linear models as Matrix Multiplication I
library(tidyverse)
library(rafalib)
library(dslabs)
library(UsingR)

y <- rnorm(1e6)
x <- cbind(rep(1,1e6), rep(0:1, each = 5e5))
beta <- c(1,1)

system.time({
  sum((y-x %*% beta)^2)
})

