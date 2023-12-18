\name{akaike_weights}
\alias{akaike_weights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Akaike Weights}
\description{
Calcola gli Akaike weights di un set di modelli.
}
\usage{
akaike_weights( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore con i valori dei criteri di informazione calcolati su un set di modelli.}
}
%\details{
%I criteri di informazione possono essere AIC, BIC 
%}
\value{
Restituisce una lista con due elementi:
\item{$delta}{differenza tra ciascun valore del vettore di input ed il valore più basso.}
\item{$w}{Akaike weights.}
}
\references{
Burnham, K. P., Anderson, D. R., & Huyvaert, K. P. (2011). AIC model selection and multimodel inference in behavioral ecology: some background, observations, and comparisons. \emph{Behavioral Ecology and
Sociobiology}, 65 (1), 23–35.

McElreath, R. (2016). \emph{Statistical Rethinking: A Bayesian Course with Examples in R and Stan}. CRC Press:
Boca Raton, FL.

Wagenmakers, E.-J., & Farrell, S. (2004). AIC model selection using Akaike weights. \emph{Psychonomic bulletin
& review}, 11 (1), 192–196.
}
\author{Massimiliano Pastore}
%\note{La funzione richiede il pacchetto grafico \code{ggplot2} ed il pacchetto \code{reshape}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
data(kidiq)
m0 <- lm(kid_score ~ 1, data = kidiq)
m1 <- lm(kid_score ~ mom_hs, data = kidiq)
m2 <- lm(kid_score ~ mom_iq, data = kidiq)
m3 <- lm(kid_score ~ mom_iq + mom_hs, data = kidiq)
m4 <- lm(kid_score ~ mom_iq * mom_hs, data = kidiq)
TAB <- AIC(m0,m1,m2,m3,m4)
akaike_weights(TAB[,"AIC"])
}
\keyword{htest}
