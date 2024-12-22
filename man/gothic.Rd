% --- Source file: gothic.Rd ---
\name{gothic}
\alias{gothic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data set fittizio}

\description{
Adattato da ter Bogt & al., 2021. Dati di una ricerca sulla depressione nei preadolescenti. A ciascun soggetto è stato chiesto di esprimere su una scala a 5 punti il proprio grado di preferenza verso la musica gotica (variabile \code{goth}, più è alto il valore maggiore la preferenza), e  somministrato un test per valutare il livello di depressione con una misura che varia tra -2 e 2 (variabile \code{dep}, più è alto il valore maggiore il livello di depressione). 
}

\usage{
data( gothic )
}

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{subj}: Factor, codice soggetto.
    \item \code{gender}: Factor, genere dei soggetti. 
    \item \code{age}: num, età.
    \item \code{goth}: num, grado di preferenza per la musica gotica.
    \item \code{dep}: num, livello di depressione. 
    }
}
\references{ter Bogt, T., Hale, W.W., Canale, N., Pastore, M., Vieno, A. (2021). Goth Music and Depressive Symptoms among Adolescents: A Longitudinal Study, \emph{Journal of Youth and Adolescence}, 50, 1925--1936.}
\author{Massimiliano Pastore}
%}
\keyword{data}
%\eof
