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
      cook_levels = c(.5,1), cex = 3, whichplot = 1:6 ) {
  
  .fitted <- .hat <- .influential <- .k <- 
    .resid <- .stdresid <- .x <- CookLevel <- 
    Leverage <- StdResiduals.low <- 
    StdResiduals.up <- index <- NULL
  
  # controllo che cook levels abbia due valori 
  if (length(cook_levels)==1) {
    warning("cook_levels requires at least 2 values")
    cook_levels <- rep(cook_levels,2)
  }
  if (length(cook_levels)>2) {
    cook_levels <- range(cook_levels)
  }
  
  modelData <- check_model_data( fit, cook_levels )
  modelData$index <- rownames(modelData)
  p <- length(coefficients(fit))  # Numero di parametri
  n <- nrow(modelData)     
  hat_values <- modelData$.hat
  
  # Identifica i punti insoliti
  unusual <- with( modelData, 
         which(abs(.stdresid) > 1.64 | .hat > 3 * mean(.hat) | 
                 .cook > 4 / (n - p - 1) ))
  
  # se sono troppi tengo solo il 5%
  prop_unusual <- length(unusual) / nrow(modelData) 
  if ( (prop_unusual > .05) & (n > 150) ) {
    modelData$.unusual <- ifelse(
      modelData$index %in% unusual, TRUE, FALSE )
    sortedData <- modelData[order(modelData$.cook,decreasing = TRUE),]
    unusual <- sortedData$index[1:round(n*.05)]
  }
  if(length(unusual)>10) unusual <- unusual[1:10]
  
  cook_curves <- data.frame(
    Leverage = rep(seq(min(hat_values), max(hat_values), length.out = 100), times = 2),
    StdResiduals.low = c(
      sqrt(cook_levels[1] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
             seq(0.01, max(hat_values), length.out = 100)),
      -sqrt(cook_levels[1] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
              seq(0.01, max(hat_values), length.out = 100))
    ),
    StdResiduals.up = c(
      sqrt(cook_levels[2] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
             seq(0.01, max(hat_values), length.out = 100)),
      -sqrt(cook_levels[2] * p * (1 - seq(0.01, max(hat_values), length.out = 100)) /
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
    ylab(expression(sqrt(abs("Standardized residuals")))) +
    ggtitle("Scale-Location") + 
    theme(plot.title =element_text(hjust=.5))
  
  P4 <- ggplot(modelData, aes(x = .hat, y = .stdresid)) + 
    geom_text(data = modelData[unusual,], 
              aes(label = index), hjust = -0.3, 
              vjust = -0.3, size = cex) +
    geom_point(aes(color = .influential),shape="+",size=cex)  +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    geom_line(data = cook_curves, aes(x = Leverage, y = StdResiduals.low, group = CookLevel),
              color = "red", linetype = 2)  +
    geom_line(data = cook_curves, aes(x = Leverage, y = StdResiduals.up, group = CookLevel),
              color = "red", linetype = 3)  +
    scale_color_manual(values = c("black", "red"), labels = c("Non-Influential", "Influential"))  +
    labs(
      title = "Residuals vs Leverage",
      x = "Leverage",
      y = "Standardized Residuals" #,
      #color = "Observation Type"
    ) + guides(color = "none") +
    scale_y_continuous( limits = range(c(-4,4,modelData$.stdresid)) ) +
    scale_x_continuous( limits = c(0,max(modelData$.hat)*1.05)) +
    theme(plot.title =element_text(hjust=.5))
  
  
  P5 <- ggplot(modelData,aes(.x,.k)) + 
    geom_hline(yintercept = 0, linetype = 3, color = "grey") +
    geom_point(shape="+",color="#1a6ca8",size=cex) +
    xlab("Data point") + ylab("Pareto k") +
    ggtitle("PSIS diagnostic plot") 
  
  if (max(modelData$.k)>.7) {
    P5 <- P5 + 
      geom_hline(yintercept = .7, linetype = 3, color = "red")
  }
  if (max(modelData$.k)>1) {
    P5 <- P5 + 
      geom_hline(yintercept = 1, linetype = 2, color = "red")
  }
  
  P5 <- P5 + theme(plot.title =element_text(hjust=.5))
  
  P6 <- pp_check( fit ) + 
    theme(legend.position = "bottom") +
    ggtitle("Posterior predictive check") + 
    theme(plot.title =element_text(hjust=.5))
  
  
  PLOT <- list(P1,P2,P3,P4,P5,P6)
  PLOT <- PLOT[whichplot]
  
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
