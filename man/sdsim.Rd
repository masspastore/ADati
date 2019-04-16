\name{sdsim}
\alias{sdsim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sampling Distribution SIMulation}
\description{
Simula le distribuzioni campionarie da distribuzioni note.
}
\usage{
sdsim( n, popdist = c("Exp","Normal","Unif","Pois","Cauchy",
      "Binom","Gamma", "Xisq","Tstudent"), 
      param1 = NULL, param2 = NULL, R = 10000 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{dimensione del campione.}
  \item{popdist}{forma distributiva della della popolazione: \code{Exp} = esponenziale, 
  \code{Normal} = normale, \code{Unif} = uniforme, \code{Pois} = Poisson, \code{Cauchy} =
  Cauchy, \code{Binom} = binomiale, \code{Gamma} = gamma, \code{Xisq} = \eqn{\chi^2}, \code{Tstudent} = \eqn{t} di Student.}
  \item{param1}{parametro 1 della distribuzione scelta.}
  \item{param2}{parametro 2 della distribuzione scelta (se necessario).}
  \item{R}{numero di campioni casuali da estrarre.}
}
\details{
Produce quattro grafici: il primo rappresenta un campione estratto dalla popolazione con forma definita in \code{popdist} e dimensione \code{n}, gli altri sono le distribuzioni campionarie di somma, media e varianza.  
}
\value{
Restituisce una lista con i seguenti elementi:
  \item{$distribution}{distribuzione scelta.}
  \item{$param1}{parametro 1.}
  \item{$param2}{parametro 2.}
}
\author{Massimiliano Pastore}
%
\note{Original script by Nicole Radziwill; \code{http://qualityandinnovation.com/}} %2015/03/30/sampling-distributions-and-central-limit-theorem-in-r/}}

\examples{
## distribuzione uniforme
sdsim(20,"Unif",0,1)

## distribuzione normale standard
sdsim(20,"Normal",0,1)

## distribuzione chi-quadrato
sdsim(20,"Xisq",3)

}
\keyword{htest}
