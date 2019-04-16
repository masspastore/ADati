\name{moda}
\alias{moda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moda}
\description{
Calcola la moda (con la relativa frequenza) di una distribuzione di dati.
}
\usage{
moda( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di dati numerico o factor.}
}
\value{
Restituisce la moda e la frequenza modale.
}
\author{Massimiliano Pastore}
\examples{
x <- sample(5,15,replace=TRUE)
moda(x)
}
\keyword{htest}
