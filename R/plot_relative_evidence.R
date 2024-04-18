#rm(list=ls())
#weights <- c(0.0426,0.119,0.571,0.267)
#labels <- LETTERS[1:length(weights)] #NULL #
#load("~/MEGAsync/lavori/Rdevel/testdata/W.rda")
#weights <- W
#log <- FALSE
#textsize <- 12
#angle <- 0
#return_table <- FALSE
#short.names <- TRUE
#######################################################
#' @title Grafico evidenza relativa
#' @param weights = vettore contenente AIC-weights (preferibile che names siano i nomi modelli)
plot_relative_evidence <- function(weights,labels=NULL,log=TRUE,
                ordered = TRUE, textsize=12, 
                angle=0,U=c(0,0,0,0),return_table=FALSE,
                            short.names=FALSE) {
  
  X1 <- X2 <- value <- NA
  
  if (is.null(names(weights))) {
    if (is.null(labels)) {
      names(weights) <- paste("Model",1:length(weights))  
    } else {
      names(weights) <- labels
    }
  } 

  if (ordered) {
    w <- sort(weights)
  } else {
    w <- weights
  }
  
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
  rownames(RR) <- colnames(RR) <- names(w)
  RRplot <- reshape::melt(RR)
  colnames(RRplot)[1:2] <- c("X1","X2")
  RRplot$X2 <- factor(RRplot$X2, levels = names(w)[length(w):1], ordered = TRUE)
  RRplot$X1 <- factor(RRplot$X1, levels = names(w)[length(w):1], ordered = TRUE)
  
  if (short.names) {
    levels(RRplot$X2) <- paste0("(",1:length(levels(RRplot$X2)),")")
  }
  
  if ((!log)&(max(RRplot$value)>1e+150))
    stop("Valori di evidenza relativa troppo elevati; \ngrafico possibile solo su scala log.") 
  
  print(ggplot(RRplot,aes(X2,X1,fill=value))+geom_tile()+
          scale_fill_gradient2(low = "blue", high = "red", mid = "white",midpoint =MIDPOINT, space = "Lab",name=NAME)+
          xlab("")+ylab("")+coord_fixed()+
          theme(plot.margin = unit(U, "cm"),
                text=element_text(size=textsize),axis.text.x=element_text(angle=angle)))
  
  if (return_table) return(RR)
}

#plot_relative_evidence(W,short.names = TRUE)
#plot_relative_evidence(BBfit$weight, labels = rownames(BBfit))
