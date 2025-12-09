source()
source("simulation.R")
source("cox.R")
source()

set.seed(123)

library(tidyverse)
library(fitdistrplus)
library(psfmi)
library(survival)
library(survminer)
library(broom)


df2 <- sim_recur(rotterdam, mammaca)
hist(df2$rtime)
hist(df2[df2$histgrad==2, ]$rtime)
hist(df2[df2$histgrad==3, ]$rtime)
mean(df2[df2$histgrad==2, ]$recur, na.rm=TRUE)
mean(df2[df2$histgrad==3, ]$recur, na.rm=TRUE)

df3 <- sim_surviv(rotterdam, df2)
hist(df3[df3$histgrad==2, ]$dtime)
hist(df3[df3$histgrad==3, ]$dtime)
mean(df3[df3$histgrad==2, ]$death, na.rm=TRUE)
mean(df3[df3$histgrad==3, ]$death, na.rm=TRUE)
