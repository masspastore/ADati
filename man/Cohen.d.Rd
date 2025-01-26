\name{Cohen.d}
\alias{Cohen.d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indice d di Cohen}
\description{
Calcola l'Effect Size per le medie con l'indice \code{d} di Cohen.
}
\usage{
Cohen.d( x, y = NULL, mu = 0, type = c("two.sample","one.sample","paired") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{variabile numerica.}
  \item{y}{variabile numerica o fattore. Se e' di tipo fattore, deve avere solo due livelli
            che indicano i gruppi.}
  \item{mu}{vero valore della media (solo nel caso di campione singolo).}
  \item{type}{tipologia dei dati. Per default considera due campioni indipendenti (\code{"two.sample"}), altrimenti 
                campione singolo (\code{"one.sample"}) o dati appaiati (\code{"paired"}).}  
}
\details{
Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.

}
\value{
Restituisce una lista di classe "\code{power.htest}" con i seguenti elementi:
  \item{data.name }{stringa con il nome dei dati.}
  \item{statistic }{l'indice \code{d} di Cohen calcolato.}
  \item{effect }{livello di effect size sulla base dei criteri di Cohen: .20 = effetto debole, 
        .50 = effetto medio, .80 = effetto forte.}
  \item{method }{stringa che indica il tipo di dati.}
}
\references{Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}. 
Lawrence Erlbaum Associates, publishers.}
\author{Massimiliano Pastore}
\note{Se mancano dati il calcolo del coefficiente puo' essere inesatto, si consiglia di eliminarli prima.
}
\seealso{\code{\link{Cohen.f}}}
\examples{
### campione singolo 
x <- rnorm(10,5)
Cohen.d(x,mu=5,type="one.sample")

### due campioni indipendenti
y <- rnorm(10,5)
Cohen.d(x,y)

### dati appaiati
Cohen.d(x,y,type="paired")

### dati appaiati con y fattore
y <- factor(rep(1:2,5))
Cohen.d(x,y,type="paired")
}
\keyword{htest}
