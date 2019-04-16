\name{campionaria.cor}
\alias{campionaria.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria della correlazione}
\description{
Produce la distribuzione campionaria del coefficiente di correlazione.
}
\usage{
campionaria.cor( rho = 0, N = 10, n = 3, replace = FALSE, 
             exact = FALSE, grafico = TRUE, B = 1000, parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rho}{correlazione vera della popolazione.}
  \item{N}{numerosita' della popolazione.}
  \item{n}{numerosita' campionaria.}
  \item{replace}{logico, indica se i campioni sono con reinserimento (\code{TRUE}) oppure senza reinserimento (\code{FALSE}).}
  \item{exact}{logico, indica se creare tutti i campioni possibili (\code{TRUE}) oppure solo un numero \code{B} di campioni estratti casualmente (\code{FALSE}).}
  \item{grafico}{logico, indica se produrre il grafico (\code{TRUE}) oppure no (\code{FALSE}).}
  \item{B}{numero di campioni casuali da estrarre se \code{exact=FALSE}.}
 \item{parziali}{logico, indica se produrre i grafici parziali durante il ricampionamento.}
}

\details{
Richiede le funzioni \code{combinations} del pacchetto \code{gtools} e \code{mvrnorm} del pacchetto \code{MASS}.

ATTENZIONE: dato che il numero di campioni possibili aumenta con l'aumento di 
\code{N}, quando tale numero supera 50000 la distribuzione viene prodotta per approssimazione campionando per \code{B} volte.

Stampa una tabella con i valori della popolazione e le statistiche della distribuzione campionaria.
}
\value{
Restituisce una lista con i seguenti elementi:
  \item{$Omega}{i valori della popolazione.}
  \item{$Srx}{distribuzione campionaria della correlazione.}
}

\author{Massimiliano Pastore}

\examples{
require(MASS)
require(gtools)
campionaria.cor( 0.8, 200, 30, B = 10 )
campionaria.cor( 0.8, 20, 3, exact = TRUE, grafico = FALSE )
}
\keyword{htest}
