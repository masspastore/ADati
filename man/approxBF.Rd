\name{approxBF}
\alias{approxBF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bayes Factor approssimato}
\description{
Calcola il Bayes Factor approssimato a partire da due modelli lineari.
}
\usage{
approxBF( M0, M1 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M0}{modello nullo ottenuto con la funzione \code{lm}.}
  \item{M1}{modello alternativo, ottenuto con la funzione \code{lm}.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce una lista con i seguenti elementi:
  \item{dBic }{\eqn{\Delta_{BIC}} calcolato come \eqn{BIC_{M0}-BIC_{M1}}.}
  \item{BF }{Bayes Factor calcolato con l'approssimazione \eqn{\mbox{exp}(-\frac{\Delta_{BIC}}{2})}.}
}
\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
%\note{Se mancano dati il calcolo del coefficiente puo' essere inesatto, si consiglia di eliminarli prima.
%}
%\seealso{\code{\link{Cohen.f}}}
\examples{
### t.test
x <- sample(0:1,10,TRUE)
y <- rnorm(10)
M0 <- lm(y~1)
M1 <- lm(y~x)
approxBF(M0,M1)

### regressione lineare semplice
z <- rnorm(10)
M0 <- lm(y~1)
M1 <- lm(y~z)
approxBF(M0,M1)

## ANOVA
a <- factor(sample(1:3,10,TRUE))
M0 <- lm(y~1)
M1 <- lm(y~a)
approxBF(M0,M1)
}
\keyword{htest}
