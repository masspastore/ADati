% --- Source file: trust.Rd --- 
\name{trust} 
\alias{trust} 
\title{Data set fittizio} 

\description{ 
Data set fittizio: 
dati relativi ad una ricerca condotta in ambito mondiale sui comportamenti sociali in funzione della fiducia che gli individui pongono sulle proprie istituzioni, sulla scienza etc. Il campione si compone di 5000 soggetti, di età media 34.3 anni (sd 7.9) reclutati in 23 paesi. Le variabili \code{X1}, \code{X2}, \code{X3} e \code{X4} sono relative a misure di fiducia espresse verso le proprie istituzioni politiche, sociali, scolastiche e scientifiche; punteggi più alti indicano un maggiore grado di fiducia. La variabile \code{Y} indica il grado in cui si è disponibili a rispettare le regole, anche in questo caso maggiore è il punteggio è maggiore l'adesione al rispetto.} 

\usage{data( trust )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{ID}: Factor, codice soggetto.
    \item \code{country}: Factor, paese di residenza.
    \item \code{gender}: Factor, genere.
    \item \code{Y}: num, disponibilità a seguire le regole.
    \item \code{X1}: num, fiducia nelle istituzioni politiche.
    \item \code{X2}: num, fiducia nelle istituzioni sociali.
    \item \code{X3}: num, fiducia nelle istituzioni scolastiche.
    \item \code{X4}: num, fiducia nelle istituzioni scientifiche.
}
}
%\source{Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, %London.}
\author{Massimiliano Pastore}
\keyword{data}
