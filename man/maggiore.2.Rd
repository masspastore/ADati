\name{maggiore.2}
\alias{maggiore.2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Maggiore tra due numeri}
\description{
Trova il maggiore tra due numeri. Funzione di esempio per le strutture condizionali.
}
\usage{
maggiore.2( a, b )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a,b}{valori numerici}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce il valore maggiore tra i due inseriti. 
}
%\references{ ~put references to the literature/web site here ~ }
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
maggiore.2(21,3)
## not run
## maggiore.2(21,3,5) # errore, ci sono tre numeri

## The function is currently defined as
function(a,b)
{
    if (a==b) stop("numeri uguali")

    if (a>b) {
        return(a)
    } else {
        return(b)
    }
}
}
\keyword{math}
