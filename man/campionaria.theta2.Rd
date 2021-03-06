\name{campionaria.theta2}
\alias{campionaria.theta2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria}
\description{
Produce la distribuzione campionaria di una statistica theta basata su due campioni.
}
\usage{
campionaria.theta2( Omega1, n1 = 2, n2 = n1, Omega2 = NULL, 
      theta = 'diff', replace = FALSE, exact = FALSE, 
      grafico = TRUE, B = 1000, parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Omega1}{vettore numerico, contiene i valori della popolazione 1.}
  \item{n1}{numerosita' dei campioni estratti da \code{Omega1}.}
  \item{n2}{numerosita' dei campioni estratti da \code{Omega2}. Se non specificato e' uguale a \code{n1}.}
  \item{Omega2}{vettore numerico, contiene i valori della popolazione 2. Se non specificato e' uguale ad \code{Omega1}.}
  \item{theta}{funzione statistica a due argomenti, di cui produrre la distribuzione campionaria, per default \code{diff}.}
  \item{replace}{logico, indica se i campioni sono con reinserimento (\code{TRUE}) oppure senza reinserimento (\code{FALSE}).}
  \item{exact}{logico, indica se creare tutti i campioni possibili (\code{TRUE}) oppure solo un numero \code{B} di campioni estratti casualmente
      (\code{FALSE}).}
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
  \item{$Omega1}{i valori della popolazione 1.}
  \item{$Omega2}{i valori della popolazione 2.}
  \item{$Stheta}{distribuzione campionaria della statistica theta.}
  \item{$theta}{funzione statistica passata in input.}
}
\author{Massimiliano Pastore}

\examples{
## dati uniformi
require(gtools)
campionaria.theta2(0:3,theta='max',exact=TRUE,replace=TRUE) # con reinserimento
campionaria.theta2(0:3,theta='max',exact=TRUE) # senza reinserimento

## dati normali
fx <- function(x1,x2){max(mean(x1),mean(x2))}
campionaria.theta2(rnorm(50),n1=8,theta='fx')
campionaria.theta2(rnorm(50),n1=8,Omega2=rnorm(50,5,3),theta='fx')

## dati non normali
campionaria.theta2(rchisq(50,1),n1=8,Omega2=rnorm(50,5,3),theta='fx')
}
\keyword{htest}
