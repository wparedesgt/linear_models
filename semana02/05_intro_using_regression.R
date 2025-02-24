## Introduccion usando regresiones 

library(tidyverse)
library(dslabs)
library(rafalib)

set.seed(1)
g <- 9.8 ##meters per second
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, note: we use tt because t is a base function
d <- 56.67  - 0.5*g*tt^2 + rnorm(n,sd=1) ##meters

mypar()
plot(tt,d,ylab="Distance in meters",xlab="Time in seconds")


data(father.son,package="UsingR")
x=father.son$fheight
y=father.son$sheight

plot(x,y,xlab="Father's height",ylab="Son's height")

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "datos/femaleMiceWeights.csv"
if (!file.exists(filename)) download(url,destfile=filename)


dat <- read.csv("datos/femaleMiceWeights.csv")
mypar(1,1)
stripchart(Bodyweight~Diet,data=dat,vertical=TRUE,method="jitter",pch=1,main="Mice weights")

g <- 9.8 ##meters per second
n <- 25
tt <- seq(0,3.4,len=n) ##time in secs, t is a base function
f <- 56.67  - 0.5*g*tt^2
y <-  f + rnorm(n,sd=1)

plot(tt,y,ylab="Distance in meters",xlab="Time in seconds")
lines(tt,f,col=2)

tt2 <-tt^2
fit <- lm(y~tt+tt2)
summary(fit)$coef

rss <- function(Beta0,Beta1,Beta2){
  r <- y - (Beta0+Beta1*tt+Beta2*tt^2)
  return(sum(r^2))
}

Beta2s<- seq(-10,0,len=100)
plot(Beta2s,sapply(Beta2s,rss,Beta0=55,Beta1=0),
     ylab="RSS",xlab="Beta2",type="l")
##Let's add another curve fixing another pair:
Beta2s<- seq(-10,0,len=100)
lines(Beta2s,sapply(Beta2s,rss,Beta0=65,Beta1=0),col=2)

#### Practica 

X <- cbind(rep(1, length(tt)), tt, tt^2)
X

Beta <- matrix(c(55,0,5), 3, 1)
Beta

r <- y - X %*% Beta
RSS <- t(r) %*% r
rss(55,0,5)


RSS <- crossprod(r)

betahat <- solve(t(X) %*% X) %*% t(X) %*% y
betahat

fit

betahat <- solve(crossprod(X)) %*% crossprod(X,y)
betahat


QR <- qr(X)
Q <- qr.Q(QR)
R <- qr.R(QR)
backsolve(R, crossprod(Q,y))
