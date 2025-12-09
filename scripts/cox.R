library(survival)
library(survminer)
library(broom)

set.seed(123)

#' Cox modeling for recurrence
#' @description 
#' @param 
#' @return 
cox_recur <- function(data, name) {
  
  # compute the cox model with tumor grade as covariate
  cox_model <- coxph(formula = Surv(rtime, recur) ~ grade, data=data)
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file=paste0("results/",name,"_cox_recur_grade.csv"))
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(rtime, recur) ~ grade, data=data), 
                     data=data,
                     conf.int = TRUE, 
                     legend.labs=c("Grade=2", "Grade=3"), 
                     xlab="Time (days)",
                     ylab="Recurrence probability", 
                     title=paste0(name,": Recurrence curve based on tumor grade"),
                     ggtheme = theme_minimal())
  ggsave(paste0("results/",name,"_cox_recur_grade.png"), plot$plot)
  
  if (name=="Rotterdam") {
    # compute the cox model with treatments as covariates
    cox_model <- coxph(formula = Surv(rtime, recur) ~ hormon + chemo, data=data)
    # save coefficients in csv file
    write.csv(summary(cox_model)$coefficients, file=paste0("results/",name,"_cox_recur_treatment.csv"))
    # plot survival curve and save as png
    plot <- ggsurvplot(fit=survfit(Surv(rtime, recur) ~ hormon + chemo, data=data), 
                       data=data,
                       conf.int = TRUE, 
                       legend.labs=c("neither", "only chemo", "only hormonal", "both"),
                       xlab="Time (days)",
                       ylab="Recurrence probability", 
                       title=paste0(name,": Recurrence curve based on treatment"),
                       ggtheme = theme_minimal())
    ggsave(paste0("results/",name,"_cox_recur_treatment.png"), plot$plot)
  }
}


#' Cox modeling for survival
#' @description 
#' @param 
#' @return
cox_surv <- function(data, name) {
  
  # compute the cox model with tumor grade as covariate
  cox_model <- coxph(formula = Surv(dtime, death) ~ grade, data=data)
  # save coefficients in csv file
  write.csv(summary(cox_model)$coefficients, file=paste0("results/",name,"_cox_surv_grade.csv"))
  # plot survival curve and save as png
  plot <- ggsurvplot(fit=survfit(Surv(dtime, death) ~ grade, data=data), 
                     data=data,
                     conf.int = TRUE, 
                     legend.labs=c("Grade=2", "Grade=3"), 
                     xlab="Time (days)",
                     title=paste0(name, ": Survival curve based on tumor grade"),
                     ggtheme = theme_minimal())
  ggsave(paste0("results/",name,"_cox_surv_grade.png"), plot$plot)
  
  if (name=="Rotterdam") {
    # compute the cox model with treatments as covariates
    cox_model <- coxph(formula = Surv(dtime, death) ~ hormon + chemo, data=data)
    # save coefficients in csv file
    write.csv(summary(cox_model)$coefficients, file=paste0("results/",name,"cox_surv_treatment.csv"))
    # plot survival curve and save as png
    plot <- ggsurvplot(fit=survfit(Surv(dtime, death) ~ hormon + chemo, data=data), 
                       data=data,
                       conf.int = TRUE, 
                       legend.labs=c("neither", "only chemo", "only hormonal", "both"),
                       xlab="Time (days)",
                       title=paste0(name,": Survival curve based on treatment"),
                       ggtheme = theme_minimal())
    ggsave(paste0("results/",name,"_cox_surv_treatment.png"), plot$plot)
  }
}



