#' funzioni per ADati
#' Data frame con le previsioni del modello
#' @param fit = modello lineare ottenuto con stan_glm
#' @param B = numero di campioni da simulare 
model.predictions.rstanarm <- function( fit, B = 100 ) {
  
  Y <- t(rstanarm::posterior_predict( fit, draws = B ))
  Y <- stack( data.frame( Y ) )
  colnames(Y) <- c("simY","b")
  data <- fit$model
  
  modVar <- attr(fit$terms,"dataClasses")
  XVar <- modVar
  
  if (length(XVar)>0) {
    for (j in names(XVar)) {
      Y <- cbind(Y,data[,j])
    }
    colnames(Y)[3:ncol(Y)] <- names(XVar)
  }
  return(Y)
}

#' @examples 
#' data( studenti, package = "ADati" )
#' # null model
#' fit0 <- lm(voti~1,data=studenti)
#' Y <- model.predictions( fit0 )
#' hist( Y$hatY )
#' 
#' # modello con tre predittori
#' fit <- lm(voti~ore+anno, data = studenti)
#' Y <- model.predictions( fit )
#' par( mfrow=c(1,3))
#' for (j in levels(Y$anno)) plot( hatY ~ ore, data = subset(Y,anno==j))
#' 
#' library( ggplot2 )
#' ggplot( Y, aes( ore, hatY ))+facet_wrap(~anno)+
#' geom_point()+geom_point(aes(ore,voti),data=studenti,colour="red")
#' 