\name{gini.index}
\alias{gini}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indice di eterogeneità di Gini.}
\description{
Indice di eterogeneità di Gini.
}
\usage{
gini( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di osservazioni.}
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\value{
Calcola l'indice di eterogeneità di Gini.
}

\author{Massimiliano Pastore}

\examples{
x <- 1:5
f <- c(10,20,30,40,50)
gini( data_from_frequency(x,f) )

}
\keyword{htest}
