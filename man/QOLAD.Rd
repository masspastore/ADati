% --- Source file: QOLAD.Rd --- 
\name{QOLAD} 
\alias{QOLAD} 
\title{Data set fittizio} 
\description{ 
Data set fittizio: ricerca per valutare l'efficacia di una nuova terapia il cui obiettivo è di migliorare la Qualità della Vita in pazienti Alzheimer. Il campione si compone di 14 soggetti di età compresa tra 61 e 99 anni ricoverati in 16 diversi centri specializzati. A 72 pazienti (scelti a caso) è stata applicata la nuova terapia (\emph{treatment group}) mentre ai restanti 68 (\emph{control group}) sono state applicate le terapie tradizionali. Lo strumento utilizzato per valutare la Qualità della Vita è il \emph{Quality Of Life in Alzheimer's Disease}, QOL-AD, somministrato a ciascun paziente per 3 volte, rispettivamente prima dell'inizio della terapia e dopo 9 e 23 settimane. 
} 

\usage{data( QOLAD )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{subj}: Factor, codice paziente.
    \item \code{age}: num, età del paziente.
    \item \code{gender}: Factor, genere.
    \item \code{group}: Factor, gruppo di appartenenza (\code{treatment}, \code{control}).
    \item \code{center}: Factor, centro di ricovero.
    \item \code{week}: num, settimana di rilevazione.
    \item \code{score}: num, punteggio al QOL-AD (valori più alti indicano una migliore qualità di vita).
    }
}

\references{Carbone, E., Piras, F., Pastore, M., Borella, E. (2022). The Role of Individual Characteristics in Predicting Short- and Long-Term Cognitive and Psychological Benefits of Cognitive Stimulation Therapy for Mild-to-Moderate Dementia. \emph{Frontiers in Aging Neuroscience}, 13.}
\author{Massimiliano Pastore}
\keyword{data}
