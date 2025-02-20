##### Ejercicios de Introduccion 

library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)

data("father.son")

head(father.son)
mean(father.son$sheight)
mean(father.son$sheight[round(father.son$fheight) == 71])


#RNGkind()


## Matrix notation excercises

X <- matrix(1:1000,100,10)
X[25,3]


x <- 1:10



matr_exemple <- cbind(X1=x, X2 = 2*x, X3 = 3*x, X4 = 4*x, X5 = 5*x)

sum(matr_exemple[7,])


matrix(1:60, 20, 3, byrow = TRUE)

x <- 11:20
rbind(x,2*x, 3*x)

x <- 1:40
matrix(3*x, 20, 2)

seq(10,1,-2)
