\name{Cohen.f}
\alias{Cohen.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indice f di Cohen}
\description{
Calcola l'Effect Size per le medie di k gruppi indipendenti con l'indice \code{f} di Cohen.
}
\usage{
Cohen.f( y, x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{variabile risposta, numerica.}
  \item{x}{predittore o fattore.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce una lista di classe "\code{power.htest}" con i seguenti elementi:
  \item{data.name }{stringa con il nome dei dati.}
  \item{statistic }{l'indice \code{f} di Cohen calcolato.}
  \item{effect }{livello di effect size sulla base dei criteri di Cohen: .10 = effetto debole,
        .25 = effetto medio, .40 = effetto forte.}
  \item{method }{stringa che indica il tipo di dati.}
}
\references{Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}.
Lawrence Erlbaum Associates, publishers.}
\author{Massimiliano Pastore}
\seealso{\code{\link{Cohen.d}}}
\examples{
data(radar)
attach(radar)
Cohen.f(score,hr)
}
\keyword{htest}
