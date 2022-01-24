\name{x12}
\docType{methods}
\alias{x12}
\alias{x12-methods}
\alias{x12,ts-method}
\alias{x12,x12Batch-method}
\alias{x12,x12Single-method}
\title{ ~~ Methods for Function \code{x12} in Package \pkg{x12} ~~}
\description{
 ~~ Methods for function \code{x12} in package \pkg{x12} ~~
}
\section{Methods}{
\describe{
\item{\code{signature(object = "ts")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
}
}}

\usage{
x12(object,x12Parameter=new("x12Parameter"),x12BaseInfo=new("x12BaseInfo"),...)
}

\arguments{
\item{object}{object of class \code{\link{ts}}, \code{\link{x12Single-class}} or \code{\link{x12Batch-class}}}
\item{x12Parameter}{object of class \code{\link{x12Parameter}}}
\item{x12BaseInfo}{object of class \code{\link{x12BaseInfo}}}
\item{...}{at the moment only forceRun=FALSE}
}

\section{Value}{
\describe{
\item{}{An S4 object of class \code{\link{x12Output-class}} if \code{object} is of class \code{ts}}
\item{}{An S4 object of class \code{\link{x12Single-class}} if \code{object} is of class \code{x12Single-class}}
\item{}{An S4 object of class \code{\link{x12Batch-class}} if \code{object} is of class \code{x12Batch-class}}
}
}

\source{
  \url{https://www.census.gov/data/software/x13as.html}
}

\note{
Parallelization is implemented for x12Batch objects with help of the package 'parallel'.
To process in parallel set the option 'x12.parallel' to an integer value representing the number of cores to use ( options(x12.parallel=2) ).
Afterwards all calls to the function 'x12' on an object of class 'x12Batch' will be parallelized (For reseting use options(x12.parallel=NULL) ).

cleanHistory is deprecated and cleanArchive should be used instead.

}
\author{
Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{summary}},
\code{\link{plot}},
\code{\link{x12env}},
\code{\link{setP}},
\code{\link{getP}},
\code{\link{loadP}},
\code{\link{saveP}},
\code{\link{prev}},
\code{\link{cleanArchive}},
\code{\link{crossVal}}
}

\examples{
\dontrun{
xts <- x12(AirPassengers)
summary(xts)
xs <- x12(new("x12Single",ts=AirPassengers))
summary(xs)


xb<-x12(new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers)))
summary(xb)

#Create new batch object with 4 time series
xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))

# change the automdl to FALSE in all 4 elements
xb <- setP(xb,list(automdl=FALSE))
#change the arima.model and arima.smodel setting for the first ts object
xb <- setP(xb,list(arima.model=c(1,1,0),arima.smodel=c(1,1,0)),1)
#change the arima.model and arima.smodel setting for the second ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),2)
#change the arima.model and arima.smodel setting for the third ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(1,1,1)),3)
#change the arima.model and arima.smodel setting for the fourth ts object
xb <- setP(xb,list(arima.model=c(1,1,1),arima.smodel=c(1,1,1)),4)
#run x12 on all series
xb <- x12(xb)
summary(xb)

#Set automdl=TRUE for the first ts
xb <- setP(xb,list(automdl=TRUE),1)

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
\references{Alexander Kowarik, Angelika Meraner, Matthias Templ, Daniel
  Schopfhauser (2014). Seasonal Adjustment with the R Packages x12 and
  x12GUI. Journal of Statistical Software, 62(2), 1-21. URL
  http://www.jstatsoft.org/v62/i02/.
}

\keyword{methods}
