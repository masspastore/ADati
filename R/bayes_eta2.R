#rm(list=ls())
#data(ESdata, package = "ADati")
#partial <- FALSE
#posterior <- FALSE

#ESdata$ChildBeh[1] <- NA

#X <- foreign::read.spss( "~/MEGAsync/didattica/ADsvil2324/lezioni/data/auto.sav",
#                         to.data.frame = TRUE)
#fit <- rstanarm::stan_glm( consumo ~ peso, data = X, refresh = 0 )
#fit <- rstanarm::stan_glm( ChildBeh ~ ParSty * HighSens, 
#                           data = ESdata, chains = 1 ) 
#fit <- rstanarm::stan_glm( ChildBeh ~ 1, 
#                           data = ESdata, chains = 1 ) 
#fit <- rstanarm::stan_glm( ChildBeh ~ ParSty, 
#                           data = ESdata, chains = 1 ) 
#fit <- brms::brm( ChildBeh ~ ParSty*HighSens, 
#                           data = ESdata, chains = 1 ) 

#' @name bayes_eta2
#' @description Calcola le posterior degli eta-squared di un modello ottenuto con brms
#' @param fit = oggetto ottenuto con brms o rstanarm
#' @param partial = logico, se posto a \code{TRUE} calcola partial eta-squared
#' @param posterior = logico, se posto a \code{TRUE} restituisce le posterior  
#' @note richiede il pacchetto \code{heplots}
#' @details Produce le posterior predict distributions su ciascuna delle quali stima i valori di eta relativi al modello di input.
bayes_eta2 <- function(fit, partial = FALSE, 
                       posterior = FALSE) {
  
  package <- ifelse(class(fit) == "brmsfit", "brms", "rstanarm")
  if (package[1] == "rstanarm") {
    coef.names <- names(rstanarm::fixef(fit))
    npred <- length(rstanarm::fixef(fit))
    FORMULA <- formula(fit)
  } else {
    coef.names <- rownames(brms::fixef(fit))
    npred <- nrow(brms::fixef(fit))
    FORMULA <- formula(fit)$formula
  }
  
  X <- attr(terms(FORMULA), which = "term.labels")
  Y <- as.character(attr(terms(FORMULA), which = "variables")[[2]])
  FORMULA <- paste0("y ~ ",paste(X, collapse=" + "))
  PREDS <-  X <- X[!grepl(":",X)] 
  Z <- na.omit( fit$data[c(Y,PREDS)] )
  
  if (package[1] == "rstanarm") {
    pp <- data.frame(t(rstanarm::posterior_predict(fit)))
  } else {
    pp <- data.frame(t(brms::posterior_predict(fit)))
  }
  if (npred == 1) {
    warning("null model, eta-squared is 0")
  }
  if (npred == 2) {
    ETApost <- data.frame(matrix(sapply(1:ncol(pp), function(j) {
      Z$y <- pp[, j]
      fit <- lm(FORMULA, data = Z)
      ETA <- heplots::etasq(fit, partial = partial)[, 
                                                    1]
      ETA <- ETA[-length(ETA)]
      return(ETA)
    }), ncol = 1))
    colnames(ETApost) <- coef.names[-1]
  }
  if (npred > 2) {
    ETApost <- data.frame(t(sapply(1:ncol(pp), function(j) {
      Z$y <- pp[, j]
      fit <- lm(FORMULA, data = Z)
      ETA <- heplots::etasq(fit, partial = partial)[, 
                                                    1]
      ETA <- ETA[-length(ETA)]
      names(ETA) <- coef.names[-1]
      return(ETA)
    })))
  }
  if (npred > 1) {
    ETAstat <- data.frame(Estimate = apply(ETApost, 2, median), 
                          Error = apply(ETApost, 2, mad))
    if (posterior) {
      return(list(ETAstat = ETAstat, ETApost = ETApost))
    } else {
      return(ETAstat)
    }
  }
  
}

#bayes_eta2( fit )
