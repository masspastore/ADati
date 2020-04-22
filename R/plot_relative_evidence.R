#######################################################
#' @title Grafico evidenza relativa
#' @param weights = vettore contenente AIC-weights (preferibile che names siano i nomi modelli)
plot_relative_evidence <- function(weights,labels=NULL,log=TRUE,textsize=12,angle=0,return_table=FALSE) {
  
  X1 <- X2 <- value <- NA
  
  if (is.null(names(weights))) {
    if (is.null(labels)) {
      names(weights) <- paste("Model",1:length(weights))  
    } else {
      names(weights) <- labels
    }
  } 

  w <- weights
  RR <- matrix(NA,length(w),length(w))
  for (i in 1:length(w)) {
    for (j in 1:length(w)) {
      if (log) {
        RR[j,i] <- log(w[j]/w[i])  
        NAME <- "log-Relative\nEvidence"
        MIDPOINT <- 0
      } else {
        RR[j,i] <- w[j]/w[i]  
        NAME <- "Relative\nEvidence"
        MIDPOINT <- 1
      }
    }
  }
  rownames(RR) <- colnames(RR) <- names(weights)
  RRplot <- reshape::melt(RR)
  colnames(RRplot)[1:2] <- c("X1","X2")
  print(ggplot(RRplot,aes(X2,X1,fill=value))+geom_tile()+
          scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint =MIDPOINT, space = "Lab",name=NAME)+
          xlab("")+ylab("")+coord_fixed()+
          theme(plot.margin = unit(c(0,0,0,0), "cm"),
                text=element_text(size=textsize),axis.text.x=element_text(angle=angle)))
  
  if (return_table) return(RR)
}
