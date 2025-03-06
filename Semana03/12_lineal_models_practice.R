## Linear models in practice

library(tidyverse)
library(rafalib)
library(dslabs)
library(UsingR)
library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "datos/femaleMiceWeights.csv"
download(url, filename)
if (!file.exists(filename)) download(url, filename)
  
set.seed(1) #same jitter in stripchart


dat <- read.csv("datos/femaleMiceWeights.csv") ##previously downloaded
stripchart(dat$Bodyweight ~ dat$Diet, vertical=TRUE, method="jitter",
           main="Bodyweight over Diet")  

#dat$Diet <- factor(dat$Diet)
levels(dat$Diet)
X <- model.matrix(~ Diet, data=dat)
head(X)
colnames(X)
class(X)

Y <- dat$Bodyweight
X <- model.matrix(~ Diet, data=dat)
solve(t(X) %*% X) %*% t(X) %*% Y

s <- split(dat$Bodyweight, dat$Diet)
mean(s[["chow"]])
mean(s[["hf"]]) - mean(s[["chow"]])

fit <- lm(Bodyweight ~ Diet, data=dat)
summary(fit)
(coefs <- coef(fit))

stripchart(dat$Bodyweight ~ dat$Diet, vertical=TRUE, method="jitter",
           main="Bodyweight over Diet", ylim=c(0,40), xlim=c(0,3))
a <- -0.25
lgth <- .1
library(RColorBrewer)
cols <- brewer.pal(3,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])
legend("right",names(coefs),fill=cols,cex=.75,bg="white")

summary(fit)$coefficients

ttest <- t.test(s[["hf"]], s[["chow"]], var.equal=TRUE)
summary(fit)$coefficients[2,3]
ttest$statistic
