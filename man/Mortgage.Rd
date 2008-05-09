\name{Mortgage}
\alias{Mortgage}
\docType{data}
\title{Run off triangle of accumulated claims data}
\description{Development triangle (accumulated figures) of a mortgage guarantee
  business}
  
\usage{data(Mortgage)}
\format{
  A matrix with 9 accident years and 9 development years.
}
\source{Competition Presented at a London Market Actuaries Dinner,
  D.E.A. Sanders, 1990
}
\references{See table 4 in: Distribution-free Calculation of the
  Standard Error of Chain Ladder Reserve Estimates, Thomas Mack, 1993,
  \emph{ASTIN Bulletin} \bold{23}, 213 - 225
}
\examples{
Mortgage
Mortgage
matplot(t(Mortgage), type="l")
}
\keyword{datasets}