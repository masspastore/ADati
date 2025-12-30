\name{data_from_frequency}
\alias{data_from_frequency}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conversione di tabella in dati grezzi}
\description{
Converte una tabella di frequenze in dati grezzi. 
}
\usage{
data_from_frequency( x, f )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{modalità della variabile.}
  \item{f}{frequenze.}
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\value{
Restituisce il vettore di dati grezzi.
}

\author{Massimiliano Pastore}

\examples{
x <- c(10, 15, 20)
f <- c( 2, 3, 4 )
data_from_frequency( x, f )
}
\keyword{utility}
