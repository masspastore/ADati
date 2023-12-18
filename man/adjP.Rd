\name{adjP}
\alias{adjP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aggiustamento delle probabilita' per confronti multipli}
\description{
Aggiusta un vettore di probabilita' utilizzando il False Discovery Rate o il metodo Bonferroni.
}
\usage{
adjP( x, type = c("BH", "BY", "B") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vettore di probabilita' calcolate.}
  \item{type}{tipo di aggiustamento (\code{"BH"} = Benjamini-Hochberg False Discovery Rate, \code{"BY"} = Benjamini-Yekutieli 
  False Discovery Rate, \code{B} = Bonferroni). 
  Per default esegue il metodo \code{"BH"}.}
}

%\details{
%  ~~ If necessary, more details than the description above ~~
%}

\value{
Dato un vettore di probabilita' osservate \code{x}, restituisce una lista di tre elementi:
  \item{$metodo}{Metodo utilizzato per l'aggiustamento.}
  \item{$adjusted.p}{Matrice di dimensione con \code{q} righe (\code{q} e' il numero di probabilita' del 
  vettore di input) e 2 colonne (la prima con le probabilita' osservate e la seconda con quelle aggiustate).}
  \item{$index}{Vettore che indica le posizioni occupate dalle probabilita' nel vettore di input.}
}

\references{
Benjamini, Y., Hochberg, Y. (1995). 
  Controlling the False Discovery Rate: A practical and powerful approach to multiple testing. 
  \emph{Journal of the Royal Statistic Society B}, 57, 289-300.

Benjamini, Y., Yekutieli, D. (2001). The control of the
   False Discovery Rate in multiple testing under dependency.
   \emph{Annals of Statistics}, 29, 1165-1188.

Bonferroni, C. E. (1936). Teoria statistica delle classi e calcolo delle
    probabilita'. \emph{Pubblicazioni del R. Istituto Superiore di
    Scienze Economiche e Commerciali di Firenze}, 8, 3-62.
}  
\author{Massimiliano Pastore}

\examples{
x <- c(.136,.011,.004,.082,.034,.697) # probabilita' osservate
adjP(x) # correzione Benjamini-Hochberg
adjP(x,"B") # correzione Bonferroni
}
\keyword{htest}
