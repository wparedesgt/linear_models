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


