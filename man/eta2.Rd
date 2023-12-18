\name{eta2}
\alias{eta2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Eta quadro}
\description{
Calcola Eta quadro, misura di associazione, per ANOVA a fattore singolo.
}
\usage{
eta2( SS.eff, SS.tot )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SS.eff}{devianza tra i gruppi, ottenuta con l'ANOVA.}
  \item{SS.tot}{devianza totale.}
}
%\details{
%Utilizzabile solo per l'Anova ad un fattore tra soggetti. 
%}
\value{
Restituisce il valore di Eta quadro.
}
\references{
Howell, D.C. (1997). \emph{Statistical Methods for Psychology}. Duxbury Press, Belmont, CA.

Tabachnick, B.G., Fidell, L.S. (1996). \emph{Using multivariate statistics}. Harper Collins, NY.
}
\author{Massimiliano Pastore}
\note{Utilizzabile solo per l'Anova ad un fattore tra soggetti. }
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
eta2(3314.25,5119.75)
}
\keyword{htest}
