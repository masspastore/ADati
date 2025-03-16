#rm(list = ls())
#library(rstanarm)
#data( school, package = "ADati")
#fit <- stan_glmer( math ~ homework + (homework|classid) + (1|schid), data = school, 
#                   cores = parallel::detectCores(), seed = 1 )
#prob <- 0.9

check_ranef_stanarm <- function(fit, prob = 0.9, 
   CEXPOINT = .7, CEXTITLE = 10, 
   POINTSCOLOR = "#1a6ca8", LINECOLOR = "#3aaf85" ) {
   
  CEXSUBTITLE <- CEXTITLE*.75
  CEXAXISTITLE <- CEXTITLE*.85
  conf.high <- conf.low <- x <- y <- NULL
  
  RE <- rstanarm::ranef(fit)
  RECI <- rstanarm::posterior_interval( fit, prob = prob ) 
                            
  YY <- list()
  for (j in 1:length( RE )) {
    Y <- stack(RE[[j]])
    Y$names <- paste0("b[",Y$ind," ",names(RE)[j],
                  ":",rownames(RE[[j]]),"]")
    Y$level <- rownames(RE[[j]])
    colnames(Y)[1:2] <- c("y","facet")
    Y$x <- qqnorm(Y$y, plot.it = FALSE)$x
    
    PP <- RECI[ rownames(RECI) %in% Y$names, ]
    colnames(PP) <- c("conf.low","conf.high")
    PP <- data.frame(PP)
    PP$names <- rownames(PP)
    
    PP <- merge(Y,PP,by="names")
    PP <- PP[order(PP$y),]
    
    YY <- c( YY, list(PP) )
  }
  
  names(YY) <- names(RE)  
  
  #theme_set(theme_bw(plot.title.size = CEXTITLE,plot.title.space = 5, axis.title.size = CEXAXISTITLE, axis.text.size = CEXAXISTITLE))

  PLOT <- list()
  theme_set(theme_bw())
  for (j in 1:length(YY)) {
    
    P <- ggplot(YY[[j]], aes(x,y)) + facet_wrap(~facet, scales = "free") +
      geom_smooth(method = "lm", color=LINECOLOR, alpha=.2) + 
      geom_pointrange(aes(ymin=conf.low, ymax=conf.high),size=CEXPOINT*.2,color=POINTSCOLOR) + 
      ggtitle(paste0("Normality of Random Effects ",names(YY)[j]),subtitle = "Dots should be plotted along the line") + 
      xlab("Theoretical Quantiles") + 
      ylab("RE Quantiles") + 
      theme(plot.title = element_text(size=CEXTITLE),
        plot.subtitle = element_text(size=CEXSUBTITLE))
    PLOT <- c(PLOT, list(P))
  }
  names(PLOT) <- names(YY)
  
  return(PLOT)
  
}

#PP <- check_ranef_stanarm( fit )

