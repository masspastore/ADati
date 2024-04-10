\name{bayes_eta2}
\alias{bayes_eta2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Eta quadro a posteriori di un modello lineare}
\description{
Calcola gli Eta quadro a posteriori di un modello lineare  ottenuto con \code{rstanarm} o \code{brms}.
}
\usage{
bayes_eta2( fit, partial = FALSE, 
      posterior = FALSE )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{modello lineare ottenuto con \code{rstanarm} o \code{brms}.}
  \item{partial}{logico, se posto a \code{TRUE} calcola il partial eta-squared.}
  \item{posterior}{logico, se posto a \code{TRUE} restituisce anche le distribuzioni a posteriori.}
}

\details{
Richiede il pacchetto \code{heplots}.
}

\value{
Restituisce il valore di Eta quadro con relativo errore per ciascun effetto del modello; le stime sono calcolate come mediana delle posterior, gli errori sono i MAD (Median Absolute Deviation).

Se \code{posterior = TRUE} restituisce anche le distribuzioni a posteriori.   
}

\note{Non gestisce (ancora) i dati mancanti. Per avere il calcolo dell'indice si deve adattare il modello su un dataframe senza dati mancanti.}  
\author{Massimiliano Pastore}

\examples{
data( ESdata )

# modello con rstanarm
fit <- rstanarm::stan_glm( ChildBeh ~ ParSty * HighSens, data = ESdata, chains = 1 ) 
bayes_eta2( fit )

# modello con brms
fit <- brms::brm( ChildBeh ~ ParSty * HighSens, data = ESdata, chains = 1 ) 
bayes_eta2( fit )

}
\keyword{htest}
