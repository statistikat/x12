\name{loadP}
\docType{methods}
\alias{loadP-methods}
\alias{loadP,x12Batch-method}
\alias{loadP,x12Parameter-method}
\alias{loadP,x12Single-method}
\alias{saveP-methods}
\alias{saveP,x12Batch-method}
\alias{saveP,x12Parameter-method}
\alias{saveP,x12Single-method}
\alias{saveP}
\alias{loadP}
\title{\code{loadP} and \code{saveP}}
\description{
Functions \code{loadP} and \code{saveP} load and save parameter settings.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "x12Parameter")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
}}
\usage{
\S4method{loadP}{x12Single}(object, file)
\S4method{loadP}{x12Batch}(object, file)
\S4method{loadP}{x12Parameter}(object, file)
\S4method{saveP}{x12Single}(object, file)
\S4method{saveP}{x12Batch}(object, file)
\S4method{saveP}{x12Parameter}(object, file)
}
\arguments{
\item{object}{object of class \code{\link{x12Single-class}}, \code{\link{x12Batch-class}} or \code{\link{x12Parameter-class}}.}
\item{file}{filepath}
}
\examples{
\dontrun{
#Create new batch object with 4 time series and change some parameters
xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))
xb <- setP(xb,list(automdl=FALSE))
xb <- setP(xb,list(arima.model=c(1,1,0),arima.model=c(1,1,0)),1)
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),2)
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(1,1,1)),3)
xb <- setP(xb,list(arima.model=c(1,1,1),arima.smodel=c(1,1,1)),4)

#save all parameters
saveP(xb,file="xyz.RData")
xb1 <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))

#load all parameters and save it to the corresponding series inside a x12Batch-object
xb1 <- loadP(xb1,file="xyz.RData")


xs <- new("x12Single",ts=AirPassengers)
xs <- setP(xs,list(arima.model=c(2,1,1),arima.smodel=c(2,1,1)))
#Save the parameters
saveP(xs,file="xyz1.RData")

#Load a saved parameter set to a x12Single object
xs <- new("x12Single",ts=AirPassengers)
xs <- loadP(xs,file="xyz1.RData")

#Replace all parameters in a x12Batch object with one parameter set
xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))
xb <- loadP(xb,file="xyz1.RData")
}
}
\seealso{\code{\link{x12}}, \code{\link{x12Batch}}}

\keyword{methods}
