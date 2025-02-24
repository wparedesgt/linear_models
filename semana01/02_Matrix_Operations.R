#### Matrix Operations

library(tidyverse)
library(dslabs)
library(rafalib)

X <- matrix(1:12,4,3)
X

a <- 2
a*X

X <- matrix(1:12,4,3)
X
t(X)

X  <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
abc <- c(3,2,1) #use as an example
rbind( sum(X[1,]*abc), sum(X[2,]*abc), sum(X[3,]*abc))

X%*%abc

n <- 5 #pick dimensions
diag(n)

X <- matrix(c(1,3,2,1,-2,1,1,1,-1),3,3)
y <- matrix(c(6,2,1),3,1)
solve(X)%*%y #equivalent to solve(X,y)
