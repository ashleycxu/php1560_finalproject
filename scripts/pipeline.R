set.seed(123)

library(tidyverse)
library(fitdistrplus)
library(psfmi)
library(survival)
library(survminer)
library(ggplot2)
library(patchwork)
library(gtsummary)
library(gt)

# LOAD DATA
data(mammaca, package="psfmi")
data(cancer, package="survival") # click rotterdam

# INITIAL PREPROCESSING
# rename histgrad column in mammaca data to grade for consistency
names(mammaca)[names(mammaca) == 'histgrad'] <- 'grade'

# INITIAL ANALYSIS AND VISUALIZATION
source("prelim_vis.R")

# SIMULATE RECURRENCE AND SURVIVAL DATA
source("simulation.R")
mammaca_sim <- sim_data(rotterdam, mammaca)


# PLOTS FOR SIMULATED MAMMACA DATA

# plot univariate distributions of time to recurrence/death, split by tumor grade
png(file="results/mammaca_histograms.png", width=4, height=4, units="in", pointsize=8, res=1200)
par(mfrow=c(2,2))
hist(mammaca_sim[(mammaca_sim$recur==1 & mammaca_sim$grade==2), ]$rtime,
     xlab="Time to recurrence (days)", main="Mammaca, Tumor Grade 2")
hist(mammaca_sim[(mammaca_sim$recur==1 & mammaca_sim$grade==3), ]$rtime,
     xlab="Time to recurrence (days)", main="Mammaca, Tumor Grade 3")
hist(mammaca_sim[(mammaca_sim$death==1 & mammaca_sim$grade==2), ]$dtime,
     xlab="Time to death (days)", main="Mammaca, Tumor Grade 2")
hist(mammaca_sim[(mammaca_sim$death==1 & mammaca_sim$grade==3), ]$dtime,
     xlab="Time to death (days)", main="Mammaca, Tumor Grade 3")
dev.off()

# subset of mammaca_sim data for plotting (no NA values for tumor grade)
mammaca_sub <- subset(mammaca_sim, !is.na(grade) & grade!=1)

# linear regression plots for age vs time to recurrence/death, grouped by tumor grade
p1 <- ggplot(mammaca_sub[mammaca_sub$recur==1, ], aes(x=age, y=rtime, group=as.factor(grade), color=as.factor(grade))) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x="Age", y="Time to recurrence (days)", color="Tumor grade",
       title="Mammaca Linear Regression", subtitle="Age, Recurrence Time, Tumor Grade")

p2 <- ggplot(mammaca_sub[mammaca_sub$death==1, ], aes(x=age, y=dtime, group=as.factor(grade), color=as.factor(grade))) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x="Age", y="Time to death (days)", color="Tumor grade",
       title="Mammaca Linear Regression", subtitle="Age, Survival Time, Tumor Grade")

plot <- p1 + p2 + plot_layout(ncol=2, guides="collect")
ggsave(plot, filename="results/mammaca_regression.png", width=10, height=5, dpi=300)

# box plots comparing time to recurrence/death between tumor grade groups
p1 <- ggplot(mammaca_sub[mammaca_sub$recur==1, ], aes(x=as.factor(grade), y=rtime)) +
  geom_boxplot() +
  stat_compare_means() +
  labs(x="Tumor grade", y="Time to recurrence (days)", title="Mammaca: Tumor Grade vs Recurrence Time")

p2 <- ggplot(mammaca_sub[mammaca_sub$death==1, ], aes(x=as.factor(grade), y=dtime)) +
  geom_boxplot() +
  stat_compare_means() +
  labs(x="Tumor grade", y="Time to death (days)", title="Mammaca: Tumor Grade vs Survival Time")

plot <- p1 + p2 + plot_layout(ncol=2)
ggsave(plot, filename="results/mammaca_boxplots.png", width=10, height=5, dpi=300)


# COX MODELING
source("cox.R")
# fit cox model on rotterdam data, using tumor grade and treatments as covariates
cox_recur(rotterdam, "Rotterdam")
cox_surv(rotterdam, "Rotterdam")
# fit cox model on mammaca data, using tumor grade as covariate
cox_recur(mammaca_sim, "Mammaca")
cox_surv(mammaca_sim, "Mammaca")
