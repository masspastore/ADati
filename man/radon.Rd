% --- Source file: radon.Rd --- 
\name{radon} 
\alias{radon} 
\title{Data set fittizio} 
\description{ 
Rilevazioni di radon nelle case. Dati raccolti dall'agenzia USA di protezione ambientale.
} 

\usage{data( radon )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{idnum}: num, codice identificativo.
    \item \code{state}: Factor, stato della rilevazione.
    \item \code{county}: Factor, contea della rilevazione.
    \item \code{floor}: num, piano dell'abitazione (\code{0} = piano terra).
    \item \code{radon}: num, concentrazione di radon. 
    \item \code{log.radon}: num, concentrazione di radon in logaritmi.
    \item \code{u}: num, concentrazione di uranio.
    \item \code{log.u}: num, concentrazione di uranio in logaritmi.
    }
}

\source{Gelman, A., Hill, J. (2006). \emph{Data analysis using regression and multilevel/hierarchical models}. Cambridge university press.}
\author{Massimiliano Pastore}
\keyword{data}
