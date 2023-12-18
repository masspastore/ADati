\name{CI.mean}
\alias{CI.mean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Intervallo di confidenza per la media}
\description{
Calcola l'intervallo di confidenza relativo ad una o piu' medie campionarie.
}
\usage{
CI.mean( x, sigma, n, level = 0.95 )
}
%- maybe also 'usage for other objects documented here.
\arguments{
  \item{x}{media campionaria (o vettore/matrice di medie).}
  \item{sigma}{deviazione standard stimata della popolazione (o vettore/matrice di deviazioni standard).}
  \item{n}{numerosita' campionaria (o vettore/matrice di numerosita' campionarie).}
  \item{level}{livello di confidenza, per default 95\%.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce una lista di tre elementi:
  \item{$input}{matrice con le informazioni passate in input.}
  \item{$err.st}{errore/i standard relativo/i alla/e media/e in input.}
  \item{$CI}{Matrice con \code{q} righe (\code{q} e' il numero di medie in input) e 2 colonne (soglia minima e
  massima dell'intervallo di confidenza).}
}
\references{Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, London.}
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
CI.mean(3,3.16,4) # solo una media
CI.mean(c(3,10,14),c(1.58,2.38,1.96),4) # vettore di medie
}
\keyword{htest}
