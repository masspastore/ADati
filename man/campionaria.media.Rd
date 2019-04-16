\name{campionaria.media}
\alias{campionaria.media}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria della media}
\description{
Produce la distribuzione campionaria della media aritmetica.
}
\usage{
campionaria.media( Omega, n = 2, replace = FALSE, exact = FALSE, 
                       grafico = TRUE, B = 1000, parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Omega}{vettore numerico, contiene i valori della popolazione.}
  \item{n}{numerosita' campionaria.}
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
  \item{$Smx}{distribuzione campionaria della media.}
}
\author{Massimiliano Pastore}

\examples{
## dati uniformi
require(gtools)
campionaria.media(0:3,exact=TRUE,replace=TRUE) # con reinserimento
campionaria.media(0:3,exact=TRUE) # senza reinserimento

## dati normali
campionaria.media(rnorm(50),n=8)

## dati non normali
campionaria.media(rchisq(50,1),n=8)
}
\keyword{htest}
