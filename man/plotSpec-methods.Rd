\name{plotSpec}
\docType{methods}
\alias{plotSpec}
\alias{plotSpec-methods}
\alias{plotSpec,x12Output-method}
\alias{plotSpec,x12Single-method}
\alias{plotSpec,spectrum-method}
\title{ ~~ Methods for Function \code{plotSpec} in Package \pkg{x12} ~~}
\description{
Spectral plots with function \code{plotSpec} in package \pkg{x12}.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "x12Output",which="sa",
				xlab="Frequency",ylab="Decibels",
				main="Spectrum",
				col_bar="darkgrey",col_seasonal="red",col_td="blue",
				lwd_bar=4,lwd_seasonal=1,lwd_td=1,plot_legend=TRUE,...)}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Single",which="sa",
				xlab="Frequency",ylab="Decibels",
				main="Spectrum",
				col_bar="darkgrey",col_seasonal="red",col_td="blue",
				lwd_bar=4,lwd_seasonal=1,lwd_td=1,plot_legend=TRUE,...)}}{
%%  ~~describe this method here~~
}
\item{\code{signature(x = "spectrum",frequency,
				xlab="Frequency",ylab="Decibels",
				main="Spectrum",
				col_bar="darkgrey",col_seasonal="red",col_td="blue",
				lwd_bar=4,lwd_seasonal=1,lwd_td=1,plot_legend=TRUE,...)}}{
%%  ~~describe this method here~~
}
}}

\arguments{
\item{x}{an object of class \code{\link{x12Output-class}}, \code{\link{x12Single-class}} or \code{\link{spectrum-class}}.}
\item{which}{a string defining the executable of the editor to use (\code{"sa"} for the Spectrum of the Seasonally Adjusted Series,
\code{"original"} for the Spectrum of the Original Series, \code{"irregular"} for the Spectrum of the Irregular Series
and \code{"residuals"} for the Spectrum of the RegARIMA Residuals).}
\item{frequency}{frequency of the time series (has to be specified for objects of class \code{"spectrum"} only).}
\item{xlab}{label of the x-axis.} 
\item{ylab}{label of the y-axis.} 
\item{main}{plot title.} 
\item{col_bar}{color of bars.} 
\item{col_seasonal}{color of seasonal frequencies.} 
\item{col_td}{color of trading day frequencies.} 
\item{lwd_bar}{line width of bars.} 
\item{lwd_seasonal}{line width of seasonal frequencies.} 
\item{lwd_td}{line width of trading day frequencies.} 
\item{plot_legend}{logical specifying if a legend should be plotted.}
}

\author{Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{x12}},
\code{\link{plot}},
\code{\link{plotSeasFac}},
\code{\link{plotRsdAcf}}
}

\examples{

\dontrun{
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regression.variables="AO1950.1",outlier.types="all",
  outlier.critical=list(LS=3.5,TC=2.5),backcast_years=1/2))
s <- x12(s)
#w/o outliers
plot(s@x12Output,sa=TRUE,trend=TRUE,original=FALSE)
plot(s)
#with (all) outliers
plot(s,showAllout=TRUE,sa=TRUE,trend=TRUE,log_transform=TRUE,lwd_out=1,pch_ao=4)
plot(s,showAllout=TRUE,sa=TRUE,trend=TRUE,original=FALSE,showAlloutLines=TRUE,
  col_tc="purple")#,log_transform=TRUE)#,lwd_out=3)
#with showOut
plot(s,showOut="AO1960.Jun",sa=FALSE,trend=FALSE,annComp=TRUE,log_transform=TRUE)
plot(s,showOut="AO1958.Mar",sa=TRUE,trend=TRUE,annComp=TRUE,annCompTrend=FALSE)
plot(s,showOut="AO1950.Jun",annComp=FALSE,cex_out=3,pch_ao=19,col_ao="orange")
plot(s,showOut="TC1954.Feb")
plot(s,showOut="TC1954.Feb",col_tc="green3")

#w/o legend
plot(s,showAllout=TRUE,plot_legend=FALSE)
plot(s,plot_legend=FALSE)
plot(s,showOut="AO1950.1",plot_legend=FALSE,lwd_out=2,col_ao="purple")
plot(s,showOut="TC1954.Feb",col_tc="orange",col_ao="magenta",plot_legend=FALSE)
plot(s,showOut="AO1950.1",col_tc="orange",col_ao="magenta",plot_legend=FALSE)

#Forecasts & Backcasts
plot(s,forecast=TRUE)
plot(s,backcast=TRUE,showLine=TRUE)
plot(s,backcast=TRUE,forecast=TRUE,showCI=FALSE)
plot(s,forecast=TRUE,points_fc=TRUE,col_fc="purple",lty_fc=2,lty_original=3,
  lwd_fc=0.9,lwd_ci=2)
plot(s,sa=TRUE,plot_legend=FALSE)

#Seasonal Factors and SI Ratios
plotSeasFac(s)
#Spectra
plotSpec(s)
plotSpec(s,highlight=FALSE)
#Autocorrelations of the Residuals
plotRsdAcf(s)
plotRsdAcf(s,col_acf="black",lwd_acf=1)
}
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
