% --- Source file: sherifdat.Rd ---
\name{sherifdat}
\alias{sherifdat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Sherif (1935) dati di gruppi di tre persone}

\description{
Il data set contiene le stime della lunghezza del movimento (in pollici) di una luce in una stanza buia. Otto gruppi di tre persone hanno fornito tre stime per un totale di 72 osservazioni. In quattro gruppi, i partecipanti fanno delle stime individualmente prima di produrre delle stime come gruppo. Negli altri quattro gruppi i partecipanti lavorano in gruppo. Lang e Bliese (2017) hanno usato questi dati per illustrare come le funzioni della varianza nei mixed-effects models (lme) potessero essere utilizzate per valutare se i gruppi mostrassero accordo in casi di emergenza. 
}

\usage{
data( sherifdat )
}

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{person}: numeric, ID dei partecipanti entro ogni gruppo
    \item \code{time}: numeric, momento della rilevazione
    \item \code{group}: Factor, identificativo del gruppo
    \item \code{y}: numeric, stima del movimento della luce in pollici
    \item \code{condition}: Factor, condizione sperimentale: inizio individuale (1) o stime di gruppo (0) 
    }
}
\source{
https://brocku.ca/MeadProject/Sherif/Sherif_1935a/Sherif_1935a_3.html
}
\author{Massimiliano Pastore}
%}
\references{
Lang, J. W. B., & Bliese, P. D. (2017). A temporal perspective on emergence: Using 3-level mixed effects models to track consensus emergence in groups. \emph{Handbook of Multilevel Theory, Measurement, and Analysis}. Washington, DC: American Psychological Association.
}
\note{Lo stesso data frame è disponibile nel pacchetto \code{multilevel}.}

\keyword{data}
%\eof
