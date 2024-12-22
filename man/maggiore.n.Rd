\name{maggiore.n}
\alias{maggiore.n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maggiore tra n numeri}
\description{
Trova il maggiore tra un insieme di numeri. Funzione di esempio per il ciclo \code{for}.
}
\usage{
maggiore.n( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di numeri.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce il valore maggiore tra quelli del vettore \code{x}. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- rnorm(10)
maggiore.n(x)

## The function is currently defined as
function(x)
{
    mx <- x[1]
    for (i in 2:length(x)) {
        if (x[i]>mx) mx <- x[i]
    }
    return(mx)
}
}
\keyword{math}
