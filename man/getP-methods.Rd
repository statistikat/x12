\name{getP-methods}
\docType{methods}
\alias{getP}
\alias{getP-methods}
\alias{getP,x12Batch-method}
\alias{getP,x12Parameter-method}
\alias{getP,x12Single-method}
\alias{setP}
\alias{setP-methods}
\alias{setP,x12Batch-method}
\alias{setP,x12Parameter-method}
\alias{setP,x12Single-method}

\title{ \code{getP} and \code{setP} for retrieving and setting parameters}
\description{
 \code{getP} and \code{setP} for retrieving and setting parameters from a 
 \code{\link{x12Single-class}}, \code{\link{x12Batch-class}} or \code{\link{x12Parameter-class}} object.
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
\S4method{getP}{x12Single}(object, whichP)
\S4method{getP}{x12Batch}(object, whichP,index=NULL)
\S4method{getP}{x12Parameter}(object, whichP)
\S4method{setP}{x12Single}(object, listP)
\S4method{setP}{x12Batch}(object, listP,index=NULL)
\S4method{setP}{x12Parameter}(object, listP)
}
\arguments{
\item{object}{object of class \code{\link{x12Single-class}}, \code{\link{x12Batch-class}} or \code{\link{x12Parameter-class}}.}
\item{whichP}{character vector with the names of the parameters to extract}
\item{listP}{named list of parameters to change}
\item{index}{index of the series in \code{\link{x12Batch-class}} to change or extract (NULL=all)}
}

\seealso{\code{\link{x12}}, \code{\link{x12Single}}, \code{\link{x12Batch}}}
\examples{
\dontrun{
#Create new batch object with 4 time series
xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))
# change the automdl to FALSE in all 4 elements
xb <- setP(xb,list(automdl=FALSE))
#change the arima.model and arima.smodel settings for the first ts object
xb <- setP(xb,list(arima.model=c(1,1,0),arima.smodel=c(1,1,0)),1)
#change the arima.model and arima.smodel settings for the second ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),2)
#change the arima.model and arima.smodel settingsfor the third ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(1,1,1)),3)
#change the arima.model and arima.smodel settings for the fourth ts object
xb <- setP(xb,list(arima.model=c(1,1,1),arima.smodel=c(1,1,1)),4)
#run x12 on all series
xb <- x12(xb)
summary(xb)
#Set automdl=TRUE for the first ts
xb <- setP(xb,list(automdl=TRUE),1)
getP(xb,"automdl")
#rerun x12 on all series (the binaries will only run on the first one)
xb <- x12(xb)
#summary with oldOutput
summary(xb,oldOutput=10)
#Change the parameter and output of the first series back to the first run
xb <- prev(xb,index=1,n=1)
#summary with oldOutput (--- No valid previous runs. ---)
summary(xb,oldOutput=10)
}

}

\keyword{methods}

