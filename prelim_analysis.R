library(psfmi)
library(survival)
data(mammaca, package="psfmi")
data(cancer, package="survival") # click rotterdam


library(gtsummary)
library(gt)
# compute summary statistics for both datasets and save
tbl_summary(rotterdam, statistic = list(all_continuous() ~ "{mean} ({sd})")) |> 
  as_gt() |>
  gtsave(filename="rotterdam_summary.png")
tbl_summary(mammaca, statistic = list(all_continuous() ~ "{mean} ({sd})")) |> 
  as_gt() |>
  gtsave(filename="mammaca_summary.png")


# rename histgrad column in mammaca data to grade for consistency
names(mammaca)[names(mammaca) == 'histgrad'] <- 'grade'


png(file=".png")




hist(rotterdam[rotterdam$recur==0, ]$rtime)
hist(rotterdam[rotterdam$recur==1, ]$rtime)

hist(rotterdam[rotterdam$death==0, ]$dtime)
hist(rotterdam[rotterdam$death==1, ]$dtime)


recur_df <- rotterdam %>%
  filter()
  group_by() %>%
  summarize()


model <- lm()

recur_model <- glm()





library(ggplot2)
library(broom)

ggplot(rotterdam[rotterdam$recur==1, ], aes(x=age, y=rtime, group=grade, color=grade)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

ggplot(rotterdam[rotterdam$death==1, ], aes(x=age, y=dtime, group=grade, color=grade)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

ggplot(rotterdam[rotterdam$recur==1, ], aes(x=as.factor(grade), y=rtime)) +
  geom_boxplot()

ggplot(rotterdam[rotterdam$death==1, ], aes(x=as.factor(grade), y=dtime)) +
  geom_boxplot()





hist(mammaca$age)

hist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==2), ]$rtime)
hist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==3), ]$rtime)

hist(rotterdam[(rotterdam$death==1 & rotterdam$grade==2), ]$dtime)
hist(rotterdam[(rotterdam$death==1 & rotterdam$grade==3), ]$dtime)

nrow(rotterdam[rotterdam$recur==1 & rotterdam$grade==2, ]) # 314
nrow(rotterdam[rotterdam$recur==1 & rotterdam$grade==3, ]) # 1204
nrow(rotterdam[rotterdam$death==1 & rotterdam$grade==2, ]) # 262
nrow(rotterdam[rotterdam$death==1 & rotterdam$grade==3, ]) # 1010



library(fitdistrplus)
fitw <- fitdist(rotterdam[(rotterdam$death==1 & rotterdam$grade==2), ]$dtime, "weibull", "mle")
fitw
summary(fitw)
fitw$estimate["shape"]
fitw$estimate["scale"]
plot(fitw)
hist(rweibull(300, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"]))

fitnb <- fitdist(rotterdam[(rotterdam$death==1 & rotterdam$grade==2), ]$dtime, "nbinom", "mle")
fitnb
plot(fitnb)
hist(rnbinom(300, mu=fitnb$estimate["mu"], size=fitnb$estimate["size"]), breaks=16)

fitw2 <- fitdist(rotterdam[(rotterdam$death==1 & rotterdam$grade==3), ]$dtime, "weibull", "mle")
fitw2
summary(fitw2)
plot(fitw2)
hist(rweibull(1000, scale=fitw2$estimate["scale"], shape=fitw2$estimate["shape"]))


fitexp <- fitdist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==2), ]$rtime, "exp", "mle")
fitexp
plot(fitexp)
hist(rexp(300, rate=fitexp$estimate["rate"]))

fitexp2 <- fitdist(rotterdam[(rotterdam$recur==1 & rotterdam$grade==3), ]$rtime, "exp", "mle")
fitexp2
hist(rexp(1000, rate=fitexp2$estimate["rate"]))


fitw3 <- fitdist(rotterdam[(rotterdam$death==1 & rotterdam$grade==2), ]$rtime, "weibull", "mle")
fitw3
plot(fitw3)
hist(rweibull(300, scale=fitw3$estimate["scale"], shape=fitw3$estimate["shape"]))

fitw4 <- fitdist(rotterdam[(rotterdam$death==1 & rotterdam$grade==3), ]$rtime, "weibull", "mle")
fitw4
plot(fitw4)
hist(rweibull(1000, scale=fitw4$estimate["scale"], shape=fitw4$estimate["shape"]))


df <- rotterdam %>% 
  filter(recur==1, death==1)



ggplot(df3[df3$recur==1, ], aes(x=age, y=rtime, group=histgrad, color=histgrad)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

ggplot(df3[df3$death==1, ], aes(x=age, y=dtime, group=histgrad, color=histgrad)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE)

ggplot(df3[df3$recur==1, ], aes(x=as.factor(histgrad), y=rtime)) +
  geom_boxplot()

ggplot(df3[df3$death==1, ], aes(x=as.factor(histgrad), y=dtime)) +
  geom_boxplot()


