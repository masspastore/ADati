\name{campionaria.theta}
\alias{campionaria.theta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria}
\description{
Produce la distribuzione campionaria della generica statistica \code{theta}.
}
\usage{
campionaria.theta( Omega, n = 2, theta = 'mean', replace = FALSE,  
                exact = FALSE, grafico = TRUE, B = 1000, parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Omega}{vettore numerico, contiene i valori della popolazione.}
  \item{n}{numerosita' campionaria.}
  \item{theta}{funzione statistica di cui produrre la distribuzione campionaria, per default \code{mean}.}
  \item{replace}{logico, indica se i campioni sono con reinserimento (\code{TRUE}) oppure senza reinserimento (\code{FALSE}).}
  \item{exact}{logico, indica se creare tutti i campioni possibili (\code{TRUE}) oppure solo un numero \code{B} di campioni estratti casualmente (\code{FALSE}).}
  \item{grafico}{logico, indica se produrre il grafico (\code{TRUE}) oppure no (\code{FALSE}).}
  \item{B}{numero di campioni casuali da estrarre se \code{exact=FALSE}.}
 \item{parziali}{logico, indica se produrre i grafici parziali durante il ricampionamento.}
}
\details{
Richiede la funzione \code{combinations} del pacchetto \code{gtools}.

ATTENZIONE: dato che il numero di campioni possibili aumenta con l'aumento della dimensione di \code{Omega}, quando tale numero supera 50000 la distribuzione viene prodotta per approssimazione campionando per \code{B} volte.

Stampa una tabella con i valori della popolazione e le statistiche della distribuzione campionaria.
}
\value{
Restituisce una lista con i seguenti elementi:
  \item{$Omega}{i valori della popolazione.}
  \item{$Stheta}{distribuzione campionaria della statistica theta.}
  \item{$theta}{funzione statistica passata in input.}
}
\author{Massimiliano Pastore}
%
\examples{
## dati uniformi
require(gtools)
campionaria.theta(0:3,theta='median',exact=TRUE,replace=TRUE) # con reinserimento
campionaria.theta(0:3,theta='median',exact=TRUE) # senza reinserimento

## dati normali
campionaria.theta(rnorm(50),n=8)

## dati non normali
campionaria.theta(rchisq(50,1),n=8)
}
\keyword{htest}
