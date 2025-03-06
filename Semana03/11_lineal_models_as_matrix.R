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

# Lineal algebra

x <- c(1,1,2,2)
f <- formula(~x)
f

model.matrix(f)

class(x)

group <- factor( c(1,1,2,2) )
model.matrix(~ group)

model.matrix(formula(~ group))

group <- c(1,1,2,2)
model.matrix(~ group)

group <- factor(c("control","control","highfat","highfat"))
model.matrix(~ group)

group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group)


group <- factor(c(1,1,2,2,3,3))
model.matrix(~ group + 0)

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
table(diet,sex)

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
model.matrix(~ diet + sex)

model.matrix(~ diet + sex + diet:sex)

model.matrix(~ diet*sex)


group <- factor(c(1,1,2,2))
group <- relevel(group, "2")
model.matrix(~ group)


group <- factor(group, levels=c("1","2"))
model.matrix(~ group)

group <- 1:4
model.matrix(~ group, data=data.frame(group=5:8))

tt <- seq(0,3.4,len=4) 
model.matrix(~ tt + I(tt^2))

