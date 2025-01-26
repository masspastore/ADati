\name{check_model_data}
\alias{check_model_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostiche}
\description{
Restituisce un data frame con le misure di diagnostica  di un modello ottenuto con il pacchetto \code{rstanarm}.
}
\usage{
check_model_data( fit, cook_levels = c(.5,1) ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{stan_glm()} o \code{stan_glmer()}.}
  \item{cook_levels}{vettore numerico, per default \code{c(.5,1)}, indica le soglie per rappresentare la distanza di Cook. Deve avere al massimo due valori.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce un dataframe con i dati del modello più le seguenti colonne: 
\itemize{
 \item \code{.resid}: num, residui.
 \item \code{.fitted}: num, valori attesi.
 \item \code{.stdresid}: num, residui standardizzati.
 \item \code{.hat}: num, hat values.
 \item \code{.cook}: num, distanze di Cook.
 \item \code{.k}: num, k di Pareto.
}
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
\note{Richiama le funzioni \code{\link{hat_values_stanarm}}, \code{\link{cooks_distance_stanarm}} e \code{loo}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
require(rstanarm)
data(sherifdat)
fit <- stan_glmer(y~time+condition+(1|group),data=sherifdat)
head( check_model_data(fit) )

}
\keyword{utility}
