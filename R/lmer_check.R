## diagnostic plot for lmer objects
lmer_check <- function(fit,all=TRUE) {
  
  par(ask=!all)

  ## aggrego dati per i grafici
  dati <- data.frame(fitted=fitted(fit),resid=resid(fit))
  dati$absres <- sqrt(abs(dati$resid))
  dati$lev <- hatvalues(fit)
  
  theme_set(theme_bw())
  P1 <- ggplot(dati,aes(fitted,resid))+geom_hline(yintercept = 0,lty="longdash")+geom_point()+
    geom_smooth()+xlab("Fitted values")+ylab("Residuals")+ggtitle("Residuals vs Fitted")+
    theme(plot.title =element_text(hjust=.5))
  
  P2 <- ggplot(dati,aes(sample=resid))+geom_abline(aes(slope=sd(resid),intercept=mean(resid)),lty=2)+
    stat_qq()+ggtitle("Normal Q-Q")+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")+
    theme(plot.title =element_text(hjust=.5))
  
  P3 <- ggplot(dati,aes(fitted,absres))+geom_point()+
    geom_smooth()+xlab("Fitted values")+ylab(expression(sqrt("|Standardized residuals|")))+
    ggtitle("Scale-Location")+theme(plot.title =element_text(hjust=.5))
  
  MAXLEV <- max(dati$lev)
  P4 <- ggplot(dati,aes(lev,resid))+geom_point()+
    scale_x_continuous(limits=c(0,MAXLEV)) +  
    geom_smooth()+xlab("Leverage")+ylab("Standardized residuals")+ggtitle("Residuals vs Leverage")+
    theme(plot.title =element_text(hjust=.5))
  
  PLOT <- list(P1,P2,P3,P4)
  for (j in 1:length(PLOT)) print(PLOT[[j]])
  return(PLOT)
}
