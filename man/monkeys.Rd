% --- Source file: monkeys.Rd --- 
\name{monkeys} 
\alias{monkeys} 
\title{Data set fittizio} 

\description{ 
Data set fittizio: 
esperimento sulla capacita' di apprendimento di un campione di 24 
scimmie in funzione del tempo di deprivazione da cibo e 
l'assunzione di due tipologie di farmaci. Gli animali devono 
identificare degli oggetti e vengono ricompensati quando rispondono correttamente.} 

\usage{data( monkeys )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{subj}: num, codice del soggetto.
    \item \code{drug}: Factor, tipo di farmaco somministrato: \code{x}, \code{y} o nessuno (\code{c}).
    \item \code{fdep}: num, ore di deprivazione da cibo.
    \item \code{score}: num, numero di errori commessi.}
}

\source{Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, London.}
\author{Massimiliano Pastore}
\keyword{data}
