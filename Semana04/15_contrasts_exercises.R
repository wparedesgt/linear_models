#Contrast Excercises

library(tidyverse)
library(rafalib)
library(dslabs)

species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

model.matrix(~ species + condition)


# Load the dataset
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "datos/spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

# Fit the model with two variables
fitTL <- lm(friction ~ type + leg, data=spider)

library(contrast)
L4vsL2 <- contrast(fitTL, list(leg="L4", type="pull"), list(leg="L2", type="pull"))

L4vsL2
