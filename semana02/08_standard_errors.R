#### Standard_Errors

library(tidyverse)
library(dslabs)
library(rafalib)
library(UsingR)

set.seed(1)
B <- 10000
h0 <- 56.67
v0 <- 0
g <- 9.8 ##meters per second

n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
X <-cbind(1,tt,tt^2)
##create X'X^-1 X'
A <- solve(crossprod(X)) %*% t(X)
betahat<-replicate(B,{
  y <- h0 + v0*tt  - 0.5*g*tt^2 + rnorm(n,sd=1)
  betahats <- A%*%y
  return(betahats[3])
})
head(betahat)


mypar(1,2)
hist(betahat)
qqnorm(betahat)
qqline(betahat)

round(mean(betahat),1)

sd(betahat) 

data(father.son,package="UsingR")
x <- father.son$fheight
y <- father.son$sheight
n <- length(y)

N <- 50
B <-1000
betahat <- replicate(B,{
  index <- sample(n,N)
  sampledat <- father.son[index,]
  x <- sampledat$fheight
  y <- sampledat$sheight
  lm(y~x)$coef
})
betahat <- t(betahat) #have estimates in two columns


mypar(1,2)
qqnorm(betahat[,1])
qqline(betahat[,1])
qqnorm(betahat[,2])
qqline(betahat[,2])

cor(betahat[,1],betahat[,2])

mean( (betahat[,1]-mean(betahat[,1] ))* (betahat[,2]-mean(betahat[,2])))


n <- nrow(father.son)
N <- 50
index <- sample(n,N)
sampledat <- father.son[index,]
x <- sampledat$fheight
y <- sampledat$sheight
X <- model.matrix(~x)

N <- nrow(X)
p <- ncol(X)

XtXinv <- solve(crossprod(X))

resid <- y - X %*% XtXinv %*% crossprod(X,y)

s <- sqrt( sum(resid^2)/(N-p))
ses <- sqrt(diag(XtXinv))*s 


summary(lm(y~x))$coef[,2]
ses

apply(betahat,2,sd)


x <-  father.son$fheight
beta <-  c(34,0.5)
var(beta[1]+beta[2]*x)


n <- length(tt)
y <- h0 + v0*tt  - 0.5*g*tt^2 + rnorm(n,sd=1)
var(y)

########### Nota tecnica

data("father.son")

X <- father.son$fheight
beta <- c(34,0.5)
var(beta[1]+beta[2]*X)

Y <- h0+v0*tt - 0.5*g*tt^2 + rnorm(n, sd =1)
