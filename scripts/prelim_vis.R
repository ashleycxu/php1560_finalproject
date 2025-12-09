library(gtsummary)
library(gt)
# compute summary statistics for both datasets and save
tbl_summary(rotterdam, statistic = list(all_continuous() ~ "{mean} ({sd})")) |> 
  as_gt() |>
  gtsave(filename="results/rotterdam_summary.png")
tbl_summary(mammaca, statistic = list(all_continuous() ~ "{mean} ({sd})")) |> 
  as_gt() |>
  gtsave(filename="results/mammaca_summary.png")

# plot univariate distributions of time to recurrence/death, split by tumor grade
png(file="results/rotterdam_histograms.png", width=4, height=4, units="in", pointsize=8, res=1200)
par(mfrow=c(2,2))
hist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==2), ]$rtime,
     xlab="Time to recurrence (days)", main="Rotterdam, Tumor Grade 2")
hist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==3), ]$rtime,
     xlab="Time to recurrence (days)", main="Rotterdam, Tumor Grade 3")
hist(rotterdam[(rotterdam$death==1 & rotterdam$grade==2), ]$dtime,
     xlab="Time to death (days)", main="Rotterdam, Tumor Grade 2")
hist(rotterdam[(rotterdam$death==1 & rotterdam$grade==3), ]$dtime,
     xlab="Time to death (days)", main="Rotterdam, Tumor Grade 3")
dev.off()


library(ggplot2)
library(patchwork)

# linear regression plots for age vs time to recurrence/death, grouped by tumor grade

p1 <- ggplot(rotterdam[rotterdam$recur==1, ], aes(x=age, y=rtime, group=as.factor(grade), color=as.factor(grade))) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x="Age", y="Time to recurrence (days)", color="Tumor grade",
       title="Rotterdam Linear Regression", subtitle="Age, Recurrence Time, Tumor Grade")

p2 <- ggplot(rotterdam[rotterdam$death==1, ], aes(x=age, y=dtime, group=as.factor(grade), color=as.factor(grade))) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE) +
  labs(x="Age", y="Time to death (days)", color="Tumor grade",
       title="Rotterdam Linear Regression", subtitle="Age, Survival Time, Tumor Grade")

plot <- p1 + p2 + plot_layout(ncol=2, guides="collect")
ggsave(plot, filename="results/rotterdam_regression.png", width=10, height=5, dpi=300)

# box plots comparing time to recurrence/death between tumor grade groups

p1 <- ggplot(rotterdam[rotterdam$recur==1, ], aes(x=as.factor(grade), y=rtime)) +
  geom_boxplot() +
  stat_compare_means() +
  labs(x="Tumor grade", y="Time to recurrence (days)", title="Rotterdam: Tumor Grade vs Recurrence Time")

p2 <- ggplot(rotterdam[rotterdam$death==1, ], aes(x=as.factor(grade), y=dtime)) +
  geom_boxplot() +
  stat_compare_means() +
  labs(x="Tumor grade", y="Time to death (days)", title="Rotterdam: Tumor Grade vs Survival Time")

plot <- p1 + p2 + plot_layout(ncol=2)
ggsave(plot, filename="results/rotterdam_boxplots.png", width=10, height=5, dpi=300)


library(fitdistrplus)

# plots from fitting weibull distribution to recurrence time, separated by tumor grade

fitw <- fitdist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==2), ]$rtime, "weibull", "mle")
png(file="results/fit_rotterdam_recur_grade2.png", width=4, height=4, units="in", 
    pointsize=8, res=1200)
plot(fitw)
dev.off()

fitw <- fitdist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==3), ]$rtime, "weibull", "mle")
png(file="results/fit_rotterdam_recur_grade3.png", width=4, height=4, units="in", 
    pointsize=8, res=1200)
plot(fitw)
dev.off()


