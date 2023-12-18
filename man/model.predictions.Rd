\name{model.predictions}
\alias{model.predictions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Previsioni di un modello lineare}
\description{
Produce un data frame con le previsioni di un modello lineare.
}
\usage{
model.predictions( fit, B = 1000 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{lm}.}
  \item{B}{numero di campioni da simulare, per default 1000.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce un data frame con le seguenti colonne
  \item{$hatY}{valori della variabile dipendente simulati dal modello.}
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
fit0 <- lm(voti~1,data=studenti)
Y <- model.predictions( fit0 )
hist( Y$hatY )
 
# modello con due predittori
fit <- lm(voti~ore+anno, data = studenti)
Y <- model.predictions( fit, 100 )
par( mfrow=c(1,3))
for (j in levels(Y$anno)) plot( hatY ~ ore, data = subset(Y,anno==j))
 
library( ggplot2 )
ggplot( Y, aes( ore, hatY ))+facet_wrap(~anno)+
  geom_point()+geom_point(aes(ore,voti),data=studenti,colour="red")
} 
\keyword{htest}
