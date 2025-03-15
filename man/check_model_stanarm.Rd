\name{check_model_stanarm}
\alias{check_model_stanarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostiche}
\description{
Produce i grafici per le diagnostiche di un modello ottenuto con il pacchetto \code{rstanarm}.
}
\usage{
check_model_stanarm( fit, all = FALSE, 
      cook_levels = c(.5,1), cex = 3, 
      whichplot = 1:6  ) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{stan_glm()} o \code{stan_glmer()}.}
  \item{all}{logico, se posto a \code{TRUE} (il default) produce tutti i grafici insieme.}
  \item{cook_levels}{vettore numerico, per default \code{c(.5,1)}, indica le soglie per rappresentare la distanza di Cook. Deve avere al massimo due valori.}
  \item{cex}{valore numerico che indica la grandezza dei punti.}
  \item{whichplot}{vettore numerico tra 1 e 6, che indica quali grafici produrre, per default tutti.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce sei grafici: 1) residui del modello rispetto ai valori attesi,
2) Quantile-Quantile rispetto alla normale, 3) Scale-Location: radice quadrata dei residui standardizzati in valore assoluto rispetto ai valori attesi, 4) residui del modello rispetto ai leverages con le curve relative alla distanza di Cook, 5) k di Pareto per le osservazioni, 6) Posterior predictive check.

I primi quattro grafici sono gli stessi che si ottengono con il comando \code{plot()} a partire da un oggetto ottenuto con la funzione \code{lm()}, il quinto grafico, è quello che si ottiene con un oggetto ricavato dalla funzione \code{loo()} ed il sesto si ottiene con la funzione \code{pp_check()}.
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
require(rstanarm)
data(sherifdat)
fit <- stan_glmer(y~time+condition+(1|group),data=sherifdat)
check_model_stanarm(fit)

## per ottenere un grafico unico
require(cowplot)
PLOTS <- check_model_stanarm(fit, TRUE)
plot_grid( plotlist = PLOTS )

}
\keyword{utility}
