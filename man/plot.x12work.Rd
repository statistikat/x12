\name{plot.x12work}
\alias{plot.x12work}
\title{Plot method for objects of class x12work}
\description{
  Plot method for objects of class \code{"x12work"}.
}
\usage{
\method{plot}{x12work}(x,plots=c(1:9), \dots)
}
\arguments{
  \item{x}{an object of class \code{"x12work"}.}
  \item{plots}{a vector containing numbers between 1 and 9.}
  \item{\dots}{further arguments (currently ignored).}
}
\details{
  Plots:\cr
  1: Original\cr
  2: Original Trend Adjusted\cr
  3: Log Original\cr
  4: Seasonal Factors\cr
  5: Seasonal Factors with SI Ratios\cr
  6: Spectrum Adjusted Original\cr
  7: Spectrum Seasonal Adjusted\cr
  8: Spectrum Irregular\cr
  9: Spectrum Residulas\cr
}
\author{Alexander Kowarik}
\seealso{\code{\link{x12work}}}
\examples{
  data(AirPassengersX12)
  #plot(AirPassengersX12)
}
\keyword{aplot}


