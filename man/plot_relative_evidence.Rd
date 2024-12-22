\name{plot_relative_evidence}
\alias{plot_relative_evidence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Relative evidence}
\description{
Produce la matrice grafica con le evidenze relative a coppie di un set di modelli.
}
\usage{
plot_relative_evidence( weights, labels = NULL, log = TRUE, 
          ordered = TRUE, 
          textsize = 12, angle = 0, U = c(0,0,0,0), 
          return_table = FALSE, short.names = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{weights}{vettore con gli Akaike Weights, meglio se gli elementi del vettore hanno i nomi dei modelli (vedi esempio).}
  \item{labels}{vettore (opzionale) con i nomi dei modelli da cui sono stati ricavati i weights.}
  \item{log}{logico, se posto a \code{TRUE} (il default) calcola i logaritmi delle evidenze relative.}
  \item{ordered}{logico, se posto a \code{TRUE} (il default) ordina i weights.}
  \item{textsize}{dimensione del testo.}
  \item{angle}{valore numerico, indica l'angolo delle etichette sulla matrice grafica.}
  \item{U}{vettore numerico, indica i margini (superiore, destro, inferiore e sinistro) della figura in cm.}
  \item{return_table}{logico, se posto a \code{TRUE} restituisce la tabella, contenente le informazioni rappresentate nel grafico.}
  \item{short.names}{logico, se posto a \code{TRUE} modifica i nomi delle colonne della tabella. Da usarsi per rendere più leggibile la tabella.}

}
%\details{
%Richiede il pacchetto \code{reshape}.
%}
\value{
Restituisce un grafico con una matrice quadrata di dimensione \code{length(weights)} \eqn{\times} \code{length(weights)}. Ogni cella della matrice rappresenta il rapporto \eqn{w_i/w_j} oppure, se \code{log = TRUE}, il log-rapporto \eqn{log(w_i/w_j)}, in cui \eqn{w_i} è il \code{weight} del modello sulla riga e  \eqn{w_j} il modello sulla colonna. 
}
\references{
Burnham, K. P., Anderson, D. R., & Huyvaert, K. P. (2011). AIC model selection and multimodel inference in behavioral ecology: some background, observations, and comparisons. \emph{Behavioral Ecology and
Sociobiology}, 65 (1), 23–35.

McElreath, R. (2016). \emph{Statistical Rethinking: A Bayesian Course with Examples in R and Stan}. CRC Press:
Boca Raton, FL.

Wagenmakers, E.-J., & Farrell, S. (2004). AIC model selection using Akaike weights. \emph{Psychonomic bulletin
& review}, 11 (1), 192–196.
}
\author{Massimiliano Pastore}
\note{La funzione richiede il pacchetto grafico \code{ggplot2} ed il pacchetto \code{reshape}.}
%\seealso{\code{\link{Cohen.f}}}
\examples{
weights <- c(0.24, 0.46, 0.20, 0.10)
names(weights) <- c("M1", "M2", "M3", "M4")
plot_relative_evidence(weights, log = FALSE)

### 
data(kidiq)
m0 <- lm(kid_score ~ 1, data = kidiq)
m1 <- lm(kid_score ~ mom_hs, data = kidiq)
m2 <- lm(kid_score ~ mom_iq, data = kidiq)
m3 <- lm(kid_score ~ mom_iq + mom_hs, data = kidiq)
m4 <- lm(kid_score ~ mom_iq * mom_hs, data = kidiq)
TAB <- AIC(m0,m1,m2,m3,m4)
weights <- akaike_weights(TAB[,"AIC"])$w
plot_relative_evidence(weights, labels = c("m0","m1","m2","m3","m4"))
}
\keyword{utility}
