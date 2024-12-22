\name{campionaria.diffmedie}
\alias{campionaria.diffmedie}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria della differenza tra medie}
\description{
Produce la distribuzione campionaria della differenza tra le medie di due campioni.
}
\usage{
campionaria.diffmedie( Omega1, n1 = 2, n2 = n1, Omega2 = NULL,
      replace = FALSE, exact = FALSE, grafico = TRUE, B = 1000,
      parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Omega1}{vettore numerico, contiene i valori della popolazione 1.}
  \item{n1}{numerosita' dei campioni estratti da \code{Omega1}.}
  \item{Omega2}{vettore numerico, contiene i valori della popolazione 2. Se non specificato e' uguale ad \code{Omega1}.}
  \item{n2}{numerosita' dei campioni estratti da \code{Omega2}. Se non specificato e' uguale a \code{n1}.}
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
  \item{$Sdiff}{distribuzione campionaria della differenza tra le medie.}
}
\author{Massimiliano Pastore}

\examples{
## dati uniformi
require(gtools)
campionaria.diffmedie(0:3,exact=TRUE,replace=TRUE) # con reinserimento
campionaria.diffmedie(0:3,exact=TRUE) # senza reinserimento

## dati normali
campionaria.diffmedie(rnorm(50),n1=8)
campionaria.diffmedie(rnorm(50),n1=8,Omega2=rnorm(50,5,3))

## dati non normali
campionaria.diffmedie(rchisq(50,1),n1=8,Omega2=rnorm(50,5,3))
}
\keyword{htest}
