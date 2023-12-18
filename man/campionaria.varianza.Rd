\name{campionaria.varianza}
\alias{campionaria.varianza}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distribuzione campionaria della varianza}
\description{
Produce la distribuzione campionaria della varianza.
}
\usage{
campionaria.varianza( Omega, n = 2, replace = FALSE, exact = FALSE, 
                    grafico = TRUE, B = 1000, parziali = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Omega}{vettore numerico, contiene i valori della popolazione.}
  \item{n}{numerosita' campionaria.}
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
  \item{$Omega}{i valori della popolazione.}
  \item{$sigma2x}{distribuzione campionaria della varianza corretta.}
  \item{$s2x}{distribuzione campionaria della varianza non corretta.}
}

\author{Massimiliano Pastore}

\examples{
## dati uniformi
require(gtools)
campionaria.varianza(0:3,exact=TRUE,replace=TRUE) # con reinserimento
campionaria.varianza(0:3,exact=TRUE) # senza reinserimento

## dati normali
campionaria.varianza(rnorm(50),n=8)

## dati non normali
campionaria.varianza(rchisq(50,1),n=8)
}
\keyword{htest}
