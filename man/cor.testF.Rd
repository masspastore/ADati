\name{cor.testF}
\alias{cor.testF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fisher correlation test}
\description{
 Esegue la correzione di Fisher per confrontare un coefficiente di correlazione 
 osservato con quello ipotizzato nella popolazione.
}
\usage{
cor.testF( r, N, rho = 0, alternative = c("two.sided","less","greater") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{coefficiente di correlazione calcolato sul campione.}
  \item{N}{numerosita' del campione.}
  \item{rho}{coefficiente di correlazione ipotizzato nella popolazione.}
  \item{alternative}{stringa che specifica l'ipotesi alternativa, 
  puo' essere una tra "\code{two.sided}" (default), "\code{greater}" (maggiore) or "\code{less}" (minore). }
}
\details{
Trasforma i coefficienti di correlazione \code{r} con \code{.5*log(abs((1+r)/(1-r)))} 
e poi li confronta utilizzando l'approssimazione alla normale.
}
\value{
Restituisce una lista di classe "\code{htest}" con i seguenti elementi:
  \item{statistic }{valore della statistica z calcolata.}
  \item{p.value }{probabilita' associata al test.}
  \item{alternative }{stringa che descrive l'ipotesi alternativa.}
  \item{method }{stringa che indica quale tipo di test e' stato condotto.}
  \item{data.name }{stringa con la numerosita' campionaria e il valore del coefficiente di correlazione osservato.}
}
\references{
Howell, D.C. (1997). \emph{Statistical Methods for Psychology}. Duxbury Press, Belmont, CA. }
\author{Massimiliano Pastore}
\examples{
cor.testF(.89,10,.8)
cor.testF(.5,25) # rho=0
cor.testF(-.2,25,alternative="less")
}
\keyword{htest}
