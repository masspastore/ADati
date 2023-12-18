\name{model.predictions.rstanarm}
\alias{model.predictions.rstanarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Previsioni di un modello lineare}
\description{
Produce un data frame con le previsioni di un modello lineare.
}
\usage{
model.predictions.rstanarm( fit, B = 100 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{modello lineare ottenuto con \code{rstanarm}.}
  \item{B}{numero di campioni da simulare, per default 100.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce un data frame in forma lunga con le seguenti colonne
  \item{$simY}{valori della variabile dipendente simulati dal modello.}
  \item{$b}{etichetta per identificare il campione simulato.}
  \item{...}{valori osservati dei predittori usati nel modello lineare.}
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
%\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
data( studenti, package = "ADati" )
# null model
fit0 <- rstanarm::stan_glm( voti~1, data = studenti, chains = 1)
Y <- model.predictions.rstanarm( fit0 )
hist( Y$simY )
 
# modello con due predittori
fit <- rstanarm::stan_glm( voti~ore+anno, data = studenti, chains = 1 )
Y <- model.predictions.rstanarm( fit, 100 )
par( mfrow=c(1,3))
for (j in levels(Y$anno)) plot( simY ~ ore, data = subset(Y,anno==j))
 
library( ggplot2 )
ggplot( Y, aes( ore, simY ))+facet_wrap(~anno)+
  geom_point(colour="red")+geom_point(aes(ore,voti),data=studenti,size=3)
} 
\keyword{htest}
