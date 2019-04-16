\name{multi.kappa}
\alias{multi.kappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kappa di Cohen}
\description{
Calcola Kappa di Cohen per G valutatori a partire da una tabella di 
frequenze con tante righe quanti sono gli oggetti (o i soggetti) valutati (N), e
tante colonne quante sono le modalita' di classificazione (m). 
}
\usage{
multi.kappa( r, nrater = NA )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{matrice numerica con N righe (N sono gli oggetti o i soggetti valutati) e m colonne (m sono le valutazioni 
  per ciascun oggetto).}
  \item{nrater}{numero di valutatori, se non specificato viene calcolato automaticamente in
base alle frequenze della tabella \code{r}.}
}
\details{
L'algoritmo utilizzato e' descritto in Siegel & Castellan (1992). 
In aggiunta esegue il test per valutare se il valore di K e' significativamente maggiore di zero. 
}
\value{
Restituisce una lista della classe \code{"htest"} con i seguenti componenti: 
  \item{$statistic}{valore della statistica test utilizzata.}
  \item{$p.value}{probabilita' associata al test.}
  \item{$alternative}{una stringa di descrizione del tipo di ipotesi alternativa (solo maggiore di zero).}
  \item{$method}{una stringa che indica il tipo di test eseguito.}
  \item{$data.name}{una stringa di testo che descrive i dati in input.}
  \item{$estimate}{valore di Kappa calcolato sui dati.}
}
\references{Siegel, S., Castellan, J.N. (1992). \emph{Statistica non parametrica}. McGraw-Hill.}
\author{Massimiliano Pastore}
%\note{ ~~further notes~~ 
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
# definizione della matrice di input
r <- matrix(c(0,0,0,4,2,2,0,0,0,0,0,4, 
            2,2,0,0,0,0,1,3,1,2,0,1,
            3,1,0,0,0,2,2,0,4,0,0,0,
            1,2,1,0),10,4,byrow=TRUE)
# calcola il coefficiente ed esegue il test
multi.kappa(r)
}
\keyword{nonparametric}
