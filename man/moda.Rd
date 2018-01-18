\name{moda}
\alias{moda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moda}
\description{
Calcola la moda di una distribuzione di dati.
}
\usage{
moda(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di dati numerico o factor.}
}
%\details{
%Utilizzabile solo per l'Anova ad un fattore tra soggetti. 
%}
\value{
Restituisce la moda.
}
%\references{
%Howell, D.C. (1997). \emph{Statistical Methods for Psychology}. Duxbury Press, Belmont, CA.
%
%Tabachnick, B.G., Fidell, L.S. (1996). \emph{Using multivariate statistics}. Harper Collins, NY.
%}
\author{Massimiliano Pastore}
%\note{Utilizzabile solo per l'Anova ad un fattore tra soggetti. }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
x <- sample(5,15,replace=TRUE)
moda(x)
}
\keyword{htest}
