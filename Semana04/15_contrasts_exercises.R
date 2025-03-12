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

spider$log2friction <- log2(spider$friction)

boxplot(log2friction ~ type*leg, data = spider)


fit_wp <- lm(log2friction ~ type + leg + type:leg, data=spider)

summary(fit_wp)

anova(fit_wp)

L2vsL1 <- contrast(fit_wp, list(leg = "L2", type = "pull"), list(leg = "L1", type = "pull"))
L2vsL1

coef(fit_wp)["legL2"]


# Use contrast to compare L2 vs L1 for push samples
L2vsL1_push <- contrast(fit, 
                        list(leg="L2", type="push"), 
                        list(leg="L1", type="push"))
L2vsL1_push
