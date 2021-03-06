% --- Source file: ansiastat.Rd ---
\name{ansiastat}
\alias{ansiastat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data set fittizio}

\description{
Data set fittizio: Dati di un esperimento per individuare un metodo per ridurre 
l'ansia da esame nei corsi di statistica. 
}

\usage{
data( ansiastat )
}

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{gruppo}: Factor, tipo di trattamento ricevuto: desensibilizzazione (\code{tr1}), 
training di rilassamento (\code{tr2}) e nulla (gruppo di controllo, \code{ctr}).
    \item \code{pre}: num, punteggi di ansia prima del trattamento.
    \item \code{post}: num, punteggi di ansia dopo il trattamento.}
}
\source{Tabacknick, B.G., Fidell, L. (1996), \emph{Using Multivariate Statistics}, Harper Collins, NY.}
\author{Massimiliano Pastore}
%}
\keyword{data}
%\eof
