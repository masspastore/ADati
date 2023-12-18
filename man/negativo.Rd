\name{negativo}
\alias{negativo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trova il primo valore negativo}
\description{
Trova il primo valore negativo tra una serie di numeri in sequenza.
Funzione di esempio per il ciclo \code{while}.
}
\usage{
negativo( x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di numeri.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce il primo elemento negativo del vettore \code{x}. Se non ci sono 
valori negativi restituisce un messaggio.  
}
%\references{ ~put references to the literature/web site here ~ }
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
%~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- rnorm(10)
negativo(x)

# "non ci sono negativi"
y <- rnorm(10,100,15)
negativo(y) 

## The function is currently defined as
function(x)
{
    i <- 0
    trovato <- FALSE
    while ((i<length(x))&(trovato==FALSE)) {
        i <- i+1
        if (x[i]<0) trovato <- TRUE
    }
    if (trovato) {
        return(x[i])
    } else {
        print("non ci sono negativi")
    }
}
}
\keyword{math}
