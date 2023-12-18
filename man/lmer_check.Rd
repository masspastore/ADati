\name{lmer_check}
\alias{lmer_check}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diagnostiche}
\description{
Produce i grafici per le diagnostiche di un mixed model.
}
\usage{
lmer_check( fit, all = FALSE, cex = 1 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{oggetto ottenuto con la funzione \code{lmer}.}
  \item{all}{logico, se posto a \code{TRUE} (il default) produce tutti i grafici insieme.}
  \item{cex}{valore numerico che indica la grandezza dei punti.}
}
%\details{
%Se \code{y} non viene specificato esegue automaticamente il test a campione singolo.
%}
\value{
Restituisce quattro grafici: 1) residui del modello rispetto ai valori attesi,
2) Quantile-Quantile rispetto alla normale, 3) Scale-Location: radice quadrata dei residui in valore assoluto rispetto ai valori attesi, 4) residui del modello rispetto ai leverages colorati in funzione della distanza di Cook.

I grafici sono gli stessi che si ottengono con il comando \code{plot()} a partire da un oggetto ottenuto con la funzione \code{lm()}.
}
%\references{Raftery, A.E. (1995). Bayesian model selection in social research. \emph{Sociological Methodology}, 25, 111-163.}
\author{Massimiliano Pastore}
\note{La funzione richiede il pacchetto grafico \code{ggplot2}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
# require(lme4)
# data(sherifdat)
# fit <- lmer(y~time+condition+(1|group),data=sherifdat)
# lmer_check(fit)

## per ottenere un grafico unico
# require(cowplot)
# PLOTS <- lmer_check(fit, TRUE)
# cowplot::plot_grid( plotlist = PLOTS )

}
\keyword{utility}
