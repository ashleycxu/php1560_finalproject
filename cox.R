library(survival)
library(survminer)
library(broom)

set.seed(123)

#' Cox modeling for recurrence
#' @description 
#' @param 
#' @return 
cox_recur <- function(data) {
  
  # compute the cox model with tumor grade as covariate
  cox_model <- coxph(formula = Surv(rtime, recur) ~ grade, data=data)
  print(summary(cox_model))
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file=paste0(name,"cox_recur_grade.csv"))
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(rtime, recur) ~ grade, data=data), 
                     conf.int = TRUE, 
                     legend.labs=c("Grade=2", "Grade=3"), 
                     ylab="Recurrence probability", 
                     title="Recurrence curve based on tumor grade",
                     ggtheme = theme_minimal())
  ggsave(paste0(name,"cox_recur_grade.png"), plot)
  
  # compute the cox model with treatments as covariates
  cox_model <- coxph(formula = Surv(rtime, recur) ~ hormon + chemo, data=data)
  print(summary(cox_model))
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file="recur_treatment_1.csv")
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(rtime, recur) ~ hormon + chemo, data=data), 
                     conf.int = TRUE, 
                     legend.labs=c("neither", "only chemo", "only hormonal", "both"),
                     ylab="Recurrence probability", 
                     title="Recurrence curve based on treatment",
                     ggtheme = theme_minimal())
  ggsave(paste0(name,"cox_recur_treatment.png"), plot)
  
}


#' Cox modeling for survival
#' @description 
#' @param 
#' @return
cox_surv <- function(data, name) {
  
  # compute the cox model with tumor grade as covariate
  cox_model <- coxph(formula = Surv(dtime, death) ~ grade, data=data)
  print(summary(cox_model))
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file=paste0(name,"cox_surv_grade.csv"))
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(dtime, death) ~ grade, data=data), 
                     conf.int = TRUE, 
                     legend.labs=c("Grade=2", "Grade=3"), 
                     title="Survival curve based on tumor grade",
                     ggtheme = theme_minimal())
  ggsave(paste0(name,"cox_surv_grade.png"), plot)
  
  # compute the cox model with treatments as covariates
  cox_model <- coxph(formula = Surv(dtime, death) ~ hormon + chemo, data=data)
  print(summary(cox_model))
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file=paste0(name,"cox_surv_treatment.csv"))
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(dtime, death) ~ hormon + chemo, data=data), 
                     conf.int = TRUE, 
                     legend.labs=c("neither", "only chemo", "only hormonal", "both"),
                     title="Survival curve based on treatment",
                     ggtheme = theme_minimal())
  ggsave(paste0(name,"cox_surv_treatment.png"), plot)
  
}



