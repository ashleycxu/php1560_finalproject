library(tidyverse)
library(fitdistrplus)

set.seed(123)

#' Simulate recurrence data
#' @description Simulates whether recurrence occurs and time to recurrence for each patient, based on tumor grade
#' @param data1 Reference data frame for recurrence data
#' @param data2 Data frame that is missing recurrence data
#' @return A modified version of data2 with columns for recurrence status and time to recurrence
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
  
  # initialize columns for recurrence status and time to recurrence
  data2$recur <- NA
  data2$rtime <- NA
  
  # simulate time to recurrence based on tumor grade for each observation in second data frame
  for (i in 1:nrow(data2)) {
    # get tumor grade from current observation
    g <- data2$grade[i]
    # if tumor grade found in first data frame
    if (g %in% unique(data1$grade)) {
      # determine recurrence status (0 or 1) based on probability from first data frame
      data2$recur[i] <- rbinom(n=1, size=1, prob=prob_df[prob_df$grade==g, ]$prob_recur)
      # if recurrence does happen, simulate time to recurrence
      if (data2$recur[i]==1) {
        # get correct distribution for the tumor grade
        fitw <- fit_list[[g]]
        # randomly sample time to recurrence using the weibull distribution
        data2$rtime[i] <- rweibull(n=1, scale=fitw$estimate["scale"], shape=fitw$estimate["shape"])
      }
    }
  }
  return(data2)
}


#' Simulate survival data
#' @description Simulates death status and time to death for each patient, based on tumor grade and recurrence status
#' @param data1 Reference data frame for survival data
#' @param data2 Data frame that is missing survival data
#' @return A modified version of data2 with columns for death status and time to death
sim_surv <- function(data1, data2) {
  
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
  
  # initialize columns for death status and time to death
  data2$death <- NA
  data2$dtime <- NA
  
  # simulate time to death based on tumor grade for each observation in second data frame
  for (i in 1:nrow(data2)) {
    # get tumor grade from current observation
    g <- data2$grade[i]
    # if tumor grade is found in first data frame
    if (g %in% unique(data1$grade)) {
      # determine death status (0 or 1) based on probability from first data frame
      # given tumor grade and recurrence status
      data2$death[i] <- rbinom(n=1, size=1, prob=subset(prob_df, grade==g & recur==data2$recur[i])$prob_death)
      # if deceased, simulate time to death
      if (data2$death[i]==1) {
        # get correct distribution for the tumor grade based on recurrence status
        if (data2$recur[i]==0) {
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
      }
    }
  }
  return(data2)
}


#' Simulates recurrence and survival data
#' @description Simulates recurrence status, time to recurrence, death status, and time to death.
#' Passes results from recurrence data simulation to survival data simulation.
#' @param data1 Reference data frame for recurrence and survival data
#' @param data2 Data frame that is missing recurrence survival data
#' @return A modified version of data2 with new columns for recurrence/survival information
sim_data <- function(data1, data2) {
  df <- sim_recur(data1, data2)
  df <- sim_surv(data1, df)
  return(df)
}





