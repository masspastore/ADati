\name{shannon}
\alias{shannon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indice di entropia di Shannon.}
\description{
Indice di entropia di Shannon.
}
\usage{
shannon( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di osservazioni.}
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\value{
Calcola l'indice di eterogeneità di Shannon.
}

\author{Massimiliano Pastore}

\examples{
x <- 1:5
f <- c(10,20,30,40,50)
shannon( data_from_frequency(x,f) )

}
\keyword{htest}
