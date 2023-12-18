% --- Source file: SNA1.Rd --- 
\name{SNA1} 
\alias{SNA1} 
\title{Data set fittizio} 

\description{ 
Data set fittizio: 
dati relativi ad un esperimento condotto su 76 bambini di età compresa tra i 3 ed i 6 anni. Ciascun bambino è stato classificato in base alla modalità con cui utilizza l'associazione numeri-spazio (Spatial-Numeric Association, SNA). Con questo criterio sono stati identificati tre gruppi (definiti nella variabile \code{SNA}): soggetti SNA1, ovvero i bambini che mostrano un'associazione numeri-spazio compatibile con la direzione di lettura-scrittura, SNA2, ovvero i bambini con associazione inversa e non-SNA, ovvero bambini che non mostrano un'associazione stabile. Ai bambini viene mostrata una sequenza di numeri target da 1 a 9 ed essi devono riportare su una linea di 10 cm la posizione del numero. Nel file sono presenti le variabili che indicano la differenza in cm. tra la posizione indicata e la vera posizione dei numeri, e la media di tali scarti (variabile \code{media.scarti}).} 

\usage{data( SNA1 )} 

\format{
Il data frame contiene le seguenti variabili:
    \itemize{
    \item \code{gruppo.eta}: Factor, classe di età.
    \item \code{sna}: Factor, gruppo di appartenenza.
    \item \code{n1}: num, differenze rispetto al numero 1.
    \item \code{n2}: num, differenze rispetto al numero 2.
    \item \code{n3}: num, differenze rispetto al numero 3.
    \item \code{n4}: num, differenze rispetto al numero 4.
    \item \code{n5}: num, differenze rispetto al numero 5.
    \item \code{n6}: num, differenze rispetto al numero 6.
    \item \code{n7}: num, differenze rispetto al numero 7.
    \item \code{n8}: num, differenze rispetto al numero 8.
    \item \code{n9}: num, differenze rispetto al numero 9.
    \item \code{medie.scarti}: num, media degli scarti.
}
}
%\source{Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, %London.}
\author{Massimiliano Pastore}
\keyword{data}
