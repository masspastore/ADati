\name{Welch}
\alias{Welch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Test di Welch}
\description{
Esegue l'Anova univariata corretta con il metodo Welch per varianze non omogenee.
}
\usage{
Welch( n, mx, s2x )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{vettore delle numerosita' campionarie.}
  \item{mx}{vettore delle medie.}
  \item{s2x}{vettore delle varianze.}
}
%\details{
%  ~~ If necessary, more details than the description above ~~
%}
\value{
Restituisce un oggetto di classe \code{anova} con i seguenti elementi:
   \item{$Df}{gradi di liberta' relativi alla statistica test.}  
   \item{$Mean Sq}{valore delle varianze.}
   \item{$F value}{valore delle statistica test F corretta.}
  
  \item{$Pr(>F)}{probabilita' associata alla statistica test.}
}
\references{
Welch, B.L. (1951). On the comparison of several mean values: An alternative approach. \emph{Biometrika}, 38, 330-336.

Howell, D.C. (1997). \emph{Statistical Methods for Psychology}. Duxbury Press, Belmont, CA.

Keppel, G. (1991). \emph{Design and analysis}. Prentice Hall, London.
}
\author{Massimiliano Pastore}

\examples{
y <- c(rnorm(10,5,2),rnorm(5,5,8)) # variabile dipendente
A <- factor(c(rep(1:2,5),rep(3,5))) # tre gruppi
medie <- aggregate(y,list(A),mean)$x
varianze <- aggregate(y,list(A),var)$x
bartlett.test(y,A) # le varianze sono omogenee?
Welch(c(5,5,5),medie,varianze)
}
\keyword{htest}
