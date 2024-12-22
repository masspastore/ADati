\name{cooks_distance_stanarm}
\alias{cooks_distance_stanarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Distanza di Cook}
\description{
Calcola le distanze di Cook di un modello ottenuto con il pacchetto \code{rstanarm}.
}
\usage{
cooks_distance_stanarm( fit ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{stan_glm()} o \code{stan_glmer()}.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce un vettore con le distanze di Cook per ciascuna osservazione. 
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
%\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
require(rstanarm)
data(sherifdat)
fit <- stan_glmer(y~time+condition+(1|group),data=sherifdat)
CD <- cooks_distance_stanarm(fit)
plot(CD)

}
\keyword{utility}
