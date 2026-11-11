% --- Source file: simpson.Rd --- 
\name{simpson} 
\alias{simpson} 
\title{Data set fittizio: Paradosso di Simpson} 

\description{ 
Data set fittizio:
punteggi di tre gruppi che correlano negativamente; se non si tiene conto del
gruppo di appartenenza la correlazione è positiva.} 

\usage{data( simpson )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{x}: num, ascissa.
    \item \code{y}: num, ordinata.
    \item \code{group}: Factor, gruppo.
}
}
%\source{Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, %London.}
\author{Massimiliano Pastore}
\examples{
data( simpson )
with( simpson, cor( x, y ) )
with( subset(simpson, group=="A"), cor( x, y ) )
with( subset(simpson, group=="B"), cor( x, y ) )
with( subset(simpson, group=="C"), cor( x, y ) )
}
\keyword{data}
