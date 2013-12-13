\name{prev-methods}
\docType{methods}
\alias{prev}
\alias{prev-methods}
\alias{prev,x12Batch-method}
\alias{prev,x12Single-method}
\alias{cleanHistory}
\alias{cleanHistory-methods}
\alias{cleanHistory,x12Batch-method}
\alias{cleanHistory,x12Single-method}
\alias{cleanArchive}
\alias{cleanArchive-methods}
\alias{cleanArchive,x12Batch-method}
\alias{cleanArchive,x12Single-method}
\title{ ~~ Methods for Function \code{prev} and \code{cleanArchive} in Package \pkg{x12} ~~}
\description{
Function \code{prev} in package \pkg{x12} reverts to previous parameter settings and output.\cr
Function \code{cleanHistory} resets \code{x12OldParameter} and \code{x12OldOutput}.
}
\section{Methods}{
\describe{
\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
}
}}

\usage{
\S4method{prev}{x12Single}(object,n=NULL)
\S4method{prev}{x12Batch}(object,index=NULL,n=NULL)
\S4method{cleanHistory}{x12Single}(object)
\S4method{cleanHistory}{x12Batch}(object,index=NULL)
}

\arguments{
\item{object}{object of class \code{\link{x12Single-class}} or \code{\link{x12Batch-class}}.}
\item{n}{index corresponding to a previous run.}
\item{index}{index corresponding to (an) object(s) of class \code{"x12Single"}.}
}

\seealso{\code{\link{x12}}}

\note{cleanHistory is deprecated and cleanArchive should be used instead.}
\examples{
data(AirPassengersX12)
summary(AirPassengersX12)
# a maximum of 10 previous x12 runs are added to the summary
summary(AirPassengersX12,oldOutput=10)
#the x12Parameter and x12Output of the x12Single is set to the previous run of x12
Ap=prev(AirPassengersX12)
summary(AirPassengersX12,oldOutput=10)
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
\author{Alexander Kowarik}