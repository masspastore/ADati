\name{hat_values_stanarm}
\alias{hat_values_stanarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hat values}
\description{
Calcola gli hat values di un modello ottenuto con il pacchetto \code{rstanarm}.
}
\usage{
hat_values_stanarm( fit ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{stan_glm()} o \code{stan_glmer()}.}
}
\details{
Per i modelli lineari semplici i valori sono gli stessi che si ottengono con la funzione \code{hatvalues} su un modello di classe \code{lm}; nel caso di modelli con effetti random, al posto degli hat values sono calcolati i k di Pareto ricavati da un oggetto ottenuto con la funzione \code{loo()}. 
}
\value{
Restituisce un vettore con gli hat values (per i modelli lineari semplici) o i k di Pareto (per i modelli multilivello) per ciascuna osservazione. 
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
%\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
require(rstanarm)
data(sherifdat)

# modello lineare semplice
fit_lm <- lm(y~time+condition,data=sherifdat)
hat_lm <- hatvalues( fit_lm )

fit_stan <- stan_glm(y~time+condition,data=sherifdat)
hat_stan <- hat_values_stanarm( fit_stan )

all.equal(hat_lm,hat_stan) # verifica

# ---------------------------
# modello multilivello 
fit <- stan_glmer(y~time+condition+(1|group),data=sherifdat)
HV <- hat_values_stanarm(fit)
plot( HV, ylab = "hat values", xlab = "data" )

# rappresentazione alternativa
PLOTS <- check_model_stanarm(fit,all=TRUE)
PLOTS[[5]]

}
\keyword{utility}
