#' @examples 
#' library( ggplot2 )
#' library( lme4 )
#' data(sherifdat, package="ADati")
#' fit <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' all <- TRUE
#' cex <- 1
## diagnostic plot for lmer objects
lmer_check <- function( fit, all = FALSE, cex = 1 ) {
  
  #n <- nrow(fit@frame)
  #k <- length(fixef(fit))+sum(unlist(lapply(ranef(fit),FUN=function(x){dim(x)[2]})))

  ## dati per i grafici 
  # inutili ma se non li metto dice "no visible binding"
  .stdresid <- c(scale(residuals(fit)))
  .hat <- hatvalues(fit)
  .cooksd <- cooks.distance( fit )
  zcooksd <- .cooksd/max(.cooksd)#+cex*1.2
  .fitted <- fitted(fit)
  .resid <- resid(fit)
  ###########################################
  ## GRAFICI 
  theme_set(theme_bw())
  
  P1 <- ggplot(fit,aes(.fitted,.resid))+geom_smooth()+
    geom_hline(yintercept = 0,lty="longdash")+geom_point(size=cex,colour="#4D4D4D")+
    xlab("Fitted values")+ylab("Residuals")+ggtitle("Residuals vs Fitted")+
    theme(plot.title =element_text(hjust=.5))
  
  P2 <- ggplot(fit,aes(sample=.resid))+geom_abline(aes(slope=sd(.resid),intercept=mean(.resid)),lty=2)+
    stat_qq( size=cex,colour="#4D4D4D" )+ggtitle("Normal Q-Q")+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+
    theme(plot.title =element_text(hjust=.5))
  
  P3 <- ggplot(fit,aes(.fitted,sqrt(abs(.stdresid))))+geom_smooth()+geom_point(size=cex,colour="#4D4D4D")+
    xlab("Fitted values")+ylab(expression(sqrt("|Standardized residuals|")))+
    ggtitle("Scale-Location")+theme(plot.title =element_text(hjust=.5))
  
  P4 <- ggplot( fit, aes(.hat,.stdresid,colour=.cooksd))+
    geom_smooth(se=FALSE,lty=2)+
    geom_point(size=cex+zcooksd)+
    xlab("Leverage")+ylab("Standardized residuals")+ggtitle("Residuals vs Leverage")+
    theme(plot.title =element_text(hjust=.5),legend.position = "bottom",
          legend.box.spacing = unit(.01,"cm"), #legend.key.size = unit(.5, "cm"),
          legend.text=element_text(size=rel(.8)),
          legend.title = element_text(size=rel(.8))) + 
    scale_color_continuous("Cook's Distance",low = "#4D4D4D", high = "#ff0000")#, range=c(1,5))
  
  PLOT <- list(P1,P2,P3,P4)
  
  if (all) {
    par(ask=FALSE)
    return(PLOT)
  } else {
    par(ask=TRUE)
    for (j in 1:length(PLOT)) print(PLOT[[j]])  
    par(ask=FALSE)
  }
}

#'@example 
#'lmer_check( fit, cex = 1 )
#'lmer_check( fit, all = TRUE )
#'PLOTS <- lmer_check(fit,TRUE)
#'cowplot::plot_grid( plotlist = PLOTS )
#'