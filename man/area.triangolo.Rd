\name{area.triangolo}
\alias{area.triangolo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Area del triangolo}
\description{
Calcola l'area del triangolo. Funzione di esempio.
}
\usage{
area.triangolo( b, h )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{b}{base del triangolo.}
  \item{h}{altezza del triangolo.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\value{
 Restituisce l'area del triangolo.}
%\references{ ~put references to the literature/web site here ~ }
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
area.triangolo(5,3)

## The function is currently defined as
function(b,h)
{
    area.t <- (b*h)/2
    return(area.t)
}
}
\keyword{math} % at least one, from doc/KEYWORDS
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
