\name{crossVal}
\docType{methods}
\alias{crossVal}
%\alias{crossValidation}
\alias{crossVal-methods}
\alias{crossVal,ts-method}
\alias{crossVal,x12Single-method}
%\alias{crossVal,plotFbcast-methods}

\title{ ~~ Methods for Function \code{crossVal} in Package \pkg{x12} ~~}
\description{
Cross Validation with function \code{crossVal} in package \pkg{x12}.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ts")}}{
%%  ~~describe this method here~~
}

%\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
%}

\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
}}
%\arguments{
%  \item{x}{an object of class \code{"x12Output"} or \code{"x12Single"}.}
%  \item{which}{a string defining the executable of the editor to use.}
%}

\usage{
\S4method{crossVal}{ts}(object, x12Parameter, x12BaseInfo,
		showCI=FALSE, main="Cross Validation",
		col_original="black", col_fc="#2020ff", col_bc="#2020ff",
		col_ci="#d1d1ff", col_cishade="#d1d1ff",
		lty_original=1, lty_fc=2, lty_bc=2, lty_ci=1,
		lwd_original=1, lwd_fc=1, lwd_bc=1, lwd_ci=1, ytop=1,
		points_bc=FALSE, points_fc=FALSE, points_original=FALSE,
		showLine=TRUE, col_line="grey", lty_line=3,
		ylab="Value", xlab="Date",ylim=NULL,span=NULL)
\S4method{crossVal}{x12Single}(object, x12BaseInfo=new("x12BaseInfo"),
		showCI=FALSE, main="Cross Validation",
		col_original="black", col_fc="#2020ff", col_bc="#2020ff",
		col_ci="#d1d1ff", col_cishade="#d1d1ff",
		lty_original=1, lty_fc=2, lty_bc=2, lty_ci=1,
		lwd_original=1, lwd_fc=1, lwd_bc=1, lwd_ci=1, ytop=1,
		points_bc=FALSE, points_fc=FALSE, points_original=FALSE,
		showLine=TRUE, col_line="grey", lty_line=3,
		ylab="Value", xlab="Date",ylim=NULL,span=NULL)
}

\arguments{
\item{object}{object of class \code{\link{ts}} or \code{\link{x12Single-class}}.}
\item{x12Parameter}{object of class \code{\link{x12Parameter}}.}
\item{x12BaseInfo}{object of class \code{\link{x12BaseInfo}}.}
\item{showCI}{logical specifying if the prediction interval should be plotted.} 
\item{main}{plot title.} 
\item{col_original}{color of the original time series.} 
\item{col_fc}{color of the forecasts.} 
\item{col_bc}{color of the backcasts.} 
\item{col_ci}{color of the prediction interval.} 
\item{col_cishade}{color of the shading of the prediction interval.} 
\item{lty_original}{line type of the original time series.} 
\item{lty_fc}{line type of the forecasts.} 
\item{lty_bc}{line type of the backcasts.} 
\item{lty_ci}{line type of the prediction interval.} 
\item{lwd_original}{line width of the original time series.} 
\item{lwd_fc}{line width of the forecasts.} 
\item{lwd_bc}{line width of the backcasts.} 
\item{lwd_ci}{line width of the prediction interval.} 
\item{ytop}{multiplication factor for \code{ylim}.} 
\item{points_bc}{logical specifying if backcasts should additionally be indicated with points.}
\item{points_fc}{logical specifying if forecasts should additionally be indicated with points.}
\item{points_original}{logical specifying if the original time series should additionally be indicated with points.}
\item{showLine}{logical indicating if a boundary line should be drawn before/after fore-/backcasts.}
\item{col_line}{color of \code{showLine}.}
\item{lty_line}{line type of \code{showLine}.}
\item{ylab}{label of y-axis.} 
\item{xlab}{label of x-axis.}
\item{ylim}{range of the y-axis}
 \item{span}{vector of length 4, limiting the data used for the plot.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}}
%\item{...}{ xx }
}

\value{
An S4 object of class \code{\link{crossValidation-class}}.
}

\keyword{methods}

\author{Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{x12}},
\code{\link{plot}},
\code{\link{plotSpec}},
\code{\link{plotSeasFac}},
\code{\link{plotRsdAcf}}
}

\examples{
\dontrun{
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regression.variables="AO1950.1",outlier.types="all",
  outlier.critical=list(LS=3.5,TC=2.5),
backcast_years=1/2,forecast_years=1))

cv<-crossVal(s,showLine=TRUE)
cv
}
}
