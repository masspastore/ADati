\name{check_ranef_stanarm}
\alias{check_ranef_stanarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostiche}
\description{
Produce i grafici di normalità per gli effetti random di un modello ottenuto con il pacchetto \code{rstanarm}.
}
\usage{
check_ranef_stanarm( fit, prob = 0.9, 
   CEXPOINT = .7, CEXTITLE = 10, 
   POINTSCOLOR = "#1a6ca8", LINECOLOR = "#3aaf85"  ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione  \code{stan_glmer()}.}
  \item{prob}{livello di probabilità degli intervalli.}
  \item{CEXPOINT}{valore numerico che indica la grandezza dei punti.}
  \item{CEXTITLE}{valore numerico che indica la grandezza dei titoli.}
  \item{POINTSCOLOR}{colore dei punti.}
  \item{LINECOLOR}{colore delle linee.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce una lista di grafici Quantile-Quantile per gli effetti random del modello.

}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
require(rstanarm)
data(sherifdat)
fit <- stan_glmer(y~time+condition+(1|group),data=sherifdat)
check_ranef_stanarm(fit)

}
\keyword{utility}
