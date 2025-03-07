\name{var2}
\alias{var2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Varianza non corretta}
\description{
Calcola la varianze di un vettore numerico dividendo per n e non per n-1. 
}
\usage{
var2( x, na.rm = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore numerico di dati.}
  \item{na.rm}{logico; se posto a \code{TRUE} elimina dal vettore eventuali casi mancanti.}
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\author{Massimiliano Pastore}

\examples{
set.seed( 1 )
x <- rnorm( 10 ) 
var2( x )
}
\keyword{htest}
