% --- Source file: attivamente.Rd ---
\name{attivamente}
\alias{attivamente}
\alias{attiva.long}
\alias{attiva.wide}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data set fittizio}

\description{
Data set fittizio in forma long e wide: Dati raccolti per un progetto di prevenzione verso l’uso problematico delle tecnologie negli studenti di scuola primaria (ultimo anno) e secondaria.
}

\usage{
data( attivamente )
}

\format{
I dati sono organizzati in due diversi data frame \code{attiva.wide} e \code{attiva.long}.

\itemize{
  \item Il data frame \code{attiva.wide} contiene le seguenti variabili:
      \itemize{
      \item \code{ID}: Factor, identificativo soggetti.
      \item \code{genere}: Factor, sesso dei soggetti.
      \item \code{eta}: num, età dei soggetti.
      \item \code{scuola}: Factor, nome della scuola frequentata.
      \item \code{LivelloScuola}: num, livello scuola (1 = primaria, 2 = secondaria).
      \item \code{IUpre}: num, valutazione dell'uso problematico di Internet prima dell'intervento.
      \item \code{IUpost}: num, valutazione dell'uso problematico di Internet dopo l'intervento.
      \item \code{CGpre}: num, grado di controllo dei genitori prima dell'intervento.
      \item \code{CGpost}: num, grado di controllo dei genitori prima dell'intervento.
      }
  
   \item Il data frame \code{attiva.long} contiene le seguenti variabili:
      \itemize{
      \item \code{ID}: Factor, identificativo soggetti.
      \item \code{genere}: Factor, sesso dei soggetti.
      \item \code{eta}: num, età dei soggetti.
      \item \code{scuola}: Factor, nome della scuola frequentata.
      \item \code{fase}: Factor, momento di rilevazione prima (\code{pre}) o dopo (\code{post}) l'intervento.
      \item \code{IU}: num, valutazione dell'uso problematico di Internet.
      \item \code{CG}: num, grado di controllo dei genitori.
      }
  }
}

\note{Nel data frame in forma wide ciascuna riga corrisponde ad un soggetto mentre nel data frame in forma long ogni riga è una osservazione e pertanto ci sono due righe per ogni soggetto. 
}


%\source{Tabacknick, B.G., Fidell, L. (1996), \emph{Using Multivariate Statistics}, Harper Collins, NY.}
\author{Massimiliano Pastore}
%}

\examples{
data( attivamente )

# data frame in forma wide
head( attiva.wide )

# data frame in forma long
head( attiva.long )
}

\keyword{data}
%\eof
