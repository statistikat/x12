\name{x12Batch-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Batch-class}
\alias{dim,x12Batch-method}
\alias{length,x12Batch-method}
\alias{x12Batch}

\title{Class \code{"x12Batch"}}
\description{
Concatenation of multiple \code{\link{x12Single-class}} objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Batch", tsList, tsName, x12BaseInfo)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{x12List}:}{Object of class \code{"x12List"} ~~ }
    \item{\code{x12BaseInfo}:}{Object of class \code{"x12BaseInfo"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{setP}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{getP}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{prev}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{cleanHistory}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{loadP}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{saveP}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{summary}}}{\code{signature(object = "x12Batch")}: ... }
    \item{\code{\link{x12}}}{\code{signature(object = "x12Batch")}: ... }
    \item{dim}{\code{signature(x = "x12Batch")}: ... }
    \item{length}{\code{signature(x = "x12Batch")}: ... }
	 }
}
\author{
Alexander Kowarik
}


\seealso{
\code{\link{x12}},
\code{\linkS4class{x12Single}},
%\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
\code{\link{summary}},
\code{\link{getP}},
%\code{\link{summary.x12work}},
\code{\link{x12work}}
}
\examples{
\dontrun{
#object containing 4 time series and the corresponding parameters and output
data(AirPassengersX12Batch)
summary(AirPassengersX12Batch)
#summary with oldOutput
summary(AirPassengersX12Batch,oldOutput=10)
#Change the parameter and output of the first series back to the first run
AirPassengersX12Batch <- prev(AirPassengersX12Batch,index=1,n=1)
#summary with oldOutput (--- No valid previous runs. ---)
summary(AirPassengersX12Batch,oldOutput=10)

#Create new batch object with 4 time series
xb <- new("x12Batch",list(AirPassengers,ldeaths,nottem,UKgas),
  c("Air","ldeaths","nottem","UKgas"))
# change outlier.types to "all" in all 4 elements
xb <- setP(xb,list(outlier.types="all"))
#change the arima.model and arima.smodel setting for the first ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),1)
#change the arima.model and arima.smodel setting for the second ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),2)
#change the arima.model and arima.smodel setting for the third ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),3)
#change the arima.model and arima.smodel setting for the fourth ts object
xb <- setP(xb,list(arima.model=c(0,1,1),arima.smodel=c(0,1,1)),4)
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

#Create new batch object by combining objects of class x12Single
s1 <- new("x12Single",ts=AirPassengers,tsName="air")
s1 <- setP(s1,list(estimate=TRUE,regression.variables="AO1950.1",outlier.types="all",
  outlier.critical=list(LS=3.5,TC=2.5)))
s2 <- new("x12Single",ts=UKgas,tsName="UKgas")
s2 <- setP(s2,list(slidingspans=TRUE,history=TRUE,
  history.estimates=c("sadj","sadjchng","trend","trendchng","seasonal","aic"),
  history.sadjlags=c(1,2),automdl=TRUE))
b <- new("x12Batch",list(s1,s2))
b <- x12(b)
}
}
\keyword{classes}
