\name{plotRsdAcf}
\docType{methods}
\alias{plotRsdAcf}
\alias{plotRsdAcf-methods}
\alias{plotRsdAcf,x12Output-method}
\alias{plotRsdAcf,x12Single-method}
\title{ ~~ Methods for Function \code{plotRsdAcf} in Package \pkg{x12} ~~}
\description{
Plot of the (partial) autocorrelations of the (squared) residuals with function \code{plotRsdAcf} in package \pkg{x12}.
}
\section{Methods}{
\describe{

\item{\code{signature(x = "x12Output")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Single")}}{
%%  ~~describe this method here~~
}
}}

\usage{
\S4method{plotRsdAcf}{x12Output}(x, which="acf",
				xlab="Lag", ylab="ACF",
				main="default", col_acf="darkgrey", lwd_acf=4,
				col_ci="blue", lt_ci=2, ylim="default", ...)
\S4method{plotRsdAcf}{x12Single}(x, which="acf",
				xlab="Lag", ylab="ACF",
				main="default", col_acf="darkgrey", lwd_acf=4,
				col_ci="blue", lt_ci=2, ylim="default", ...)
				
}

\arguments{
\item{x}{object of class \code{\link{x12Output-class}} or \code{\link{x12Single-class}}.} 
\item{which}{character specifying the type of autocorrelation of
  the residuals that should be plotted, i.e. the autocorrelations or partial autocorrelations 
  of the residuals or the autocorrelations of the squared residuals (\code{"acf"}, \code{"pacf"}, \code{"acf2"}).}
\item{xlab}{label of the x-axis.} 
\item{ylab}{label of the y-axis.} 
\item{main}{plot title.}
\item{col_acf}{color of the autocorrelations.} 
\item{lwd_acf}{line width of the autocorrelations.} 
\item{col_ci}{color of the +- 2 standard error limits.} 
\item{lt_ci}{line type of the +- 2 standard error limits.} 
\item{ylim}{range of the y-axis.}
\item{...}{ignored.}
}	

\author{Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{x12}},
\code{\link{plot}},
\code{\link{plotSpec}},
\code{\link{plotSeasFac}}}
		
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
plot(s,forecast=TRUE,points_fc=TRUE,col_fc="purple",lty_fc=2,lty_original=3,lwd_fc=0.9,
  lwd_ci=2)
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
