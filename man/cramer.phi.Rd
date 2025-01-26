\name{cramer.phi}
\alias{cramer.phi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PHI di Cramer}
\description{
Calcola il coefficiente PHI di Cramer per tabelle di contingenza. 
}
\usage{
cramer.phi( chi, N, r, c = r )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chi}{valore di Chi quadrato ottenuto sulla tabella di contingenza.}
  \item{N}{numerosita' campionaria.}
  \item{r}{righe della tabella di contingenza.}
  \item{c}{colonne della tabella di contingenza. Se la tabella di contingenza e' quadrata non e' necessario specificarlo.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restistuisce il valore calcolato di PHI.
}
\references{Siegel, S., Castellan, J.N. (1992). \emph{Statistica non parametrica}. McGraw-Hill }
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
cramer.phi(45.47,753,3) # tabella quadrata
cramer.phi(15,120,3,2) # tabella con 3 righe e 2 colonne
cramer.phi(11.2,89,3,5) # tabella con 3 righe e 5 colonne
}
\keyword{nonparametric}
