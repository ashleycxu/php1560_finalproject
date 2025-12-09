library(tidyverse)
library(fitdistrplus)

set.seed(123)


#' Simulate recurrence data
#' @description 
#' @param 
#' @return
sim_recur <- function(data1, data2) {
  
  # calculate probability of recurrence for each tumor grade based on first data frame
  prob_df <- data1 %>%
    group_by(grade) %>%
    summarize(prob_recur=mean(recur))
  
  # fit weibull distribution for time to recurrence for each tumor grade separately
  # save fit distributions in a list
  fit_list <- list()
  for (g in unique(data1$grade)) {
    fitw <- fitdist(data1[(data1$recur==1 & data1$grade==g), ]$rtime, "weibull", "mle")
    fit_list[[g]] <- fitw
  }
  
  # simulate time to recurrence based on tumor grade for each observation in second data frame
  for (i in 1:nrow(data2)) {
    # get tumor grade from current observation
    g <- data2$histgrad[i]
    if (is.na(g) | !g %in% unique(data1$grade)) {
      # if tumor grade is NA or not found in first data frame, set recurrence status and time to NA
      data2$recur[i] <- NA
      data2$rtime[i] <- NA
    } else {
      # determine recurrence status (0 or 1) based on probability from first data frame
      data2$recur[i] <- rbinom(n=1, size=1, prob=prob_df[prob_df$grade==g, ]$prob_recur)
      # if recurrence does happen, simulate time to recurrence
      if (data2$recur[i]==1) {
        # get correct distribution for the tumor grade
        fitw <- fit_list[[g]]
        # randomly sample time to recurrence using the weibull distribution
        data2$rtime[i] <- rweibull(n=1, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"])
      } else {
        # leave time to recurrence as NA if recurrence doesn't happen
        data2$rtime[i] <- NA
      }
    }
  }
  return(data2)
}

df2 <- sim_recur(rotterdam, mammaca)
hist(df2$rtime)
hist(df2[df2$histgrad==2, ]$rtime)
hist(df2[df2$histgrad==3, ]$rtime)
mean(df2[df2$histgrad==2, ]$recur, na.rm=TRUE)
mean(df2[df2$histgrad==3, ]$recur, na.rm=TRUE)


#' Simulate survival data
#' @description 
#' @param 
#' @return 
sim_surviv <- function(data1, data2) {
  
  # calculate probability of death for each tumor grade and recurrence status based on first data frame
  prob_df <- data1 %>%
    group_by(grade, recur) %>%
    summarize(prob_death=mean(death))
  
  # fit weibull distribution for time to death for each tumor grade and recurrence status separately
  # save fit distributions in two lists (no recurrence and yes recurrence)
  fit_list0 <- list()
  fit_list1 <- list()
  for (g in unique(data1$grade)) {
    data1_sub0 <- subset(data1, recur==0 & death==1 & grade==g)
    fitw0 <- fitdist(data1_sub0$dtime, "weibull", "mle")
    fit_list0[[g]] <- fitw0
    data1_sub1 <- subset(data1, recur==1 & death==1 & grade==g)
    fitw1 <- fitdist(data1_sub1$dtime, "weibull", "mle")
    fit_list1[[g]] <- fitw1
  }
  
  # simulate time to death based on tumor grade for each observation in second data frame
  for (i in 1:nrow(data2)) {
    # get tumor grade from current observation
    g <- data2$histgrad[i]
    if (is.na(g) | !g %in% unique(data1$grade)) {
      # if tumor grade is NA or not found in first data frame, set death status and time to NA
      data2$death[i] <- NA
      data2$dtime[i] <- NA
    } else {
      rstatus <- data2$recur[i]
      # determine death status (0 or 1) based on probability from first data frame
      data2$death[i] <- rbinom(n=1, size=1, prob=subset(prob_df, grade==g & recur==data2$recur[i])$prob_death)
      # if deceased, simulate time to death
      if (data2$death[i]==1) {
        if (data2$recur[i]==0) {
          # get correct distribution for the tumor grade based on recurrence status
          fitw <- fit_list0[[g]]
          # randomly sample time to death using the weibull distribution
          data2$dtime[i] <- rweibull(n=1, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"])
        } else {
          fitw <- fit_list1[[g]]
          data2$dtime[i] <- rweibull(n=1, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"])
          # sample again if time to death is less than time to recurrence
          while (data2$dtime[i] < data2$rtime[i]) {
            data2$dtime[i] <- rweibull(n=1, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"])
          }
        }
      } else {
        # leave time to death as NA if patient isn't deceased
        data2$dtime[i] <- NA
      }
    }
  }
  return(data2)
}

df3 <- sim_surviv(rotterdam, df2)
hist(df3[df3$histgrad==2, ]$dtime)
hist(df3[df3$histgrad==3, ]$dtime)
mean(df3[df3$histgrad==2, ]$death, na.rm=TRUE)
mean(df3[df3$histgrad==3, ]$death, na.rm=TRUE)


run_sims <- function(nsims) {
  for (i in 1:nsims) {
    
  }
}





