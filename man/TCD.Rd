\name{TCD}
\alias{TCD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Total Coefficient of Determination}
\description{
Calcola il coefficiente di determinazione totale a partire da un modello multivariato.
}
\usage{
# richiede il pacchetto lavaan
TCD( fit, yvar = NULL, allmatrices = FALSE )  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{modello multivariato prodotto dalla funzione \code{sem()} di classe \code{lavaan}.}
  \item{yvar}{facoltativo, vettore con i nomi delle variabili endogene del modello.}
  \item{allmatrices}{logico, facoltativo, se posto a \code{TRUE}, restituisce una lista con TCD e le matrici \eqn{\Psi} e \eqn{\hat{\Sigma}_{y}}.}
}
\details{
Al momento è utilizzabile solo per modelli senza variabili latenti. 
}
\value{
Se \code{allmatrices = FALSE} restituisce il valore di TCD.

Se \code{allmatrices = TRUE} restituisce una lista:
  \item{TCD }{Total Coefficient of Determination}
  \item{PS }{Matrice di covarianza tra i residui del modello \eqn{\Psi}}
  \item{Sy }{Matrice di covarianza riprodotta \eqn{\hat{\Sigma}_{y}}}
}

\references{Bollen, K. A. (1989). \emph{Structural equations with latent variables}. New York: Wiley.}
\author{Massimiliano Pastore}
\note{Per utilizzare questa funzione bisogna avere installato il pacchetto \code{lavaan}.}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
## Per utilizzare la funzione serve il pacchetto lavaan
require(lavaan)

### regressione lineare semplice
### TCD e R-quadro sono uguali
data(Bullying)
fitLM <- lm("PBB~PPP",data=Bullying)
summary(fitLM)$r.squared

fitSEM <- sem("PBB~PPP",data=Bullying)
TCD(fitSEM)

### modello multivariato
data(MVM,package="ADati")
model <- "
  Y1 ~ X4
  Y2 ~ Y1+X4
  Y3 ~ Y1+Y2+X4
"
fit <- sem(model,data=MVM)
TCD(fit)
}
\keyword{utility}
