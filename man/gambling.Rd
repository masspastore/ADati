% --- Source file: gambling.Rd ---
\name{gambling}
\alias{gambling}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data set}

\description{
Data set adattato da Canale & al. 2016: Dati di una ricerca sulle attitudini al gioco d'azzardo in un campione di 1221 adolescenti. 
}

\usage{
data( gambling )
}

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{ID}: Factor, codice identificativo
    \item \code{school}: Factor, codice scuola.
    \item \code{class}: Factor, codice classe.
    \item \code{age}: num, età.
    \item \code{gender}: Factor, sesso.
    \item \code{frequency}: num, frequenza di gioco. 
    \item \code{perc_peers}: int, percezione di quanto giocano i pari. 
    \item \code{disapproval}: num, grado di disapprovazione.
    \item \code{risk}: num, percezione del rischio.
    \item \code{par_know}: num, percezione del controllo parentale.
    }
}
\source{Canale, N., Vieno, A. ter Bogt, T., Pastore, M., Siciliano, V., Molinaro, S. (2016). Adolescent gambling-oriented attitudes mediate the relantionship between parental knowledge and adolescent gambling: Implications for prevention. \emph{Prevention Science}, 17, 970-980.}
\author{Massimiliano Pastore}
%}
\keyword{data}
%\eof
