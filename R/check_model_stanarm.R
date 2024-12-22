#rm(list=ls())
#source("~/MEGAsync/lavori/Rdevel/ADati5.5.2/R/cooks_distance_stanarm.R")
#source("~/MEGAsync/lavori/Rdevel/ADati5.5.2/R/hat_values_stanarm.R")

#library(rstanarm)
#data( school, package = "ADati" )

#fit <- stan_glm( math ~ homework, data = school, 
#                 cores = parallel::detectCores() )
#fit <- stan_glmer( math ~ homework + (1|classid), 
#   data = school, cores = parallel::detectCores()  )

# +++++++++++++++++++++++++++++++++++++
#' @title check_model_stan
#' @description diagnostiche dei modelli lineari ottenuti con rstanarm
#' @param fit = oggetto di classe rstanarm
check_model_stanarm <- function( fit, all = FALSE, 
      cook_levels = c(.5,1), cex = 3 ) {
  
  .fitted <- .hat <- .influential <- .k <- 
    .resid <- .stdresid <- .x <- CookLevel <- 
    Leverage <- StdResiduals <- NULL
  
  modelData <- fit$data
  modelData$.x <- 1:nrow(modelData)
  modelData$.resid <- residuals(fit)  
  modelData$.fitted <- fitted(fit)
  modelData$.stdresid <- c(scale(residuals(fit)))  
  modelData$.hat <- hat_values <- hat_values_stanarm(fit)
  modelData$.cook <- cooks_distance_stanarm(fit)
  modelData$.k <- loo(fit)$diagnostics$pareto_k
  modelData$.influential <- modelData$.cook > max(cook_levels)

  p <- length(coefficients(fit))  # Numero di parametri
  n <- nrow(modelData)     
  
  cook_curves <- data.frame(
    Leverage = rep(seq(0.01, max(hat_values), length.out = 100), times = 2),
    StdResiduals = c(
      sqrt(cook_levels[1] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
             seq(0.01, max(hat_values), length.out = 100)),
      -sqrt(cook_levels[1] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
              seq(0.01, max(hat_values), length.out = 100))
    ),
    CookLevel = factor(rep(cook_levels, each = 100))
  )
  
  # +++++++++++++++++
  theme_set(theme_bw())
  
  P1 <- ggplot(modelData,aes(.fitted,.resid)) + 
    geom_hline(yintercept = 0,lty="longdash") +
    geom_smooth() + geom_point(shape="+",size=cex) +     
    xlab("Fitted values") + ylab("Residuals") + 
    ggtitle("Residuals vs Fitted") +
    theme(plot.title =element_text(hjust=.5))
  
  P2 <- ggplot(modelData,aes(sample=.resid))  +
    geom_abline(aes(slope=sd(.resid),intercept=mean(.resid)),lty=2)+
    stat_qq( colour="#4D4D4D",shape = "+", size = cex )+ggtitle("Normal Q-Q")+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+
    theme(plot.title =element_text(hjust=.5))
  
  P3 <- ggplot(modelData,aes(.fitted,sqrt(abs(.stdresid)))) +
    geom_smooth()+geom_point(shape="+",size=cex,colour="#4D4D4D") +
    xlab("Fitted values") + 
    ylab(expression(sqrt("|Standardized residuals|"))) +
    ggtitle("Scale-Location") + 
    theme(plot.title =element_text(hjust=.5))

  
  P4 <- ggplot(modelData, aes(x = .hat, y = .stdresid)) + 
    geom_point(aes(color = .influential),shape="+",size=cex)  +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 2 * mean(modelData$.hat), linetype = "dashed", color = "gray") +
    geom_line(data = cook_curves, aes(x = Leverage, y = StdResiduals, group = CookLevel),
              color = "red", linetype = "dashed")  +
    scale_color_manual(values = c("black", "red"), labels = c("Non-Influential", "Influential"))  +
    labs(
      title = "Residuals vs Leverage",
      x = "Leverage",
      y = "Standardized Residuals" #,
      #color = "Observation Type"
    ) + guides(color = "none")
  
  P5 <- ggplot(modelData,aes(.x,.k)) + 
    geom_point(shape="+",color="blue",size=cex) +
    xlab("Data point") + ylab("Pareto k") +
    ggtitle("PSIS diagnostic plot") + 
    theme(plot.title =element_text(hjust=.5))
  
  if (max(modelData$.k)>.7) {
    P5 <- P5 + 
      geom_hline(yintercept = .7, linetype = 3, color = "red")
  }
  if (max(modelData$.k)>1) {
    P5 <- P5 + 
      geom_hline(yintercept = 1, linetype = 2, color = "red")
  }
  
  P6 <- pp_check( fit ) + 
    theme(legend.position = "bottom") +
    ggtitle("Posterior predictive check") + 
    theme(plot.title =element_text(hjust=.5))
  
  
  PLOT <- list(P1,P2,P3,P4,P5,P6)
  
  if (all) {
    par(ask=FALSE)
    return(PLOT)
  } else {
    par(ask=TRUE)
    for (j in 1:length(PLOT)) print(PLOT[[j]])  
    par(ask=FALSE)
  }
  
}


#check_model_stanarm(fit)
