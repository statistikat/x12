\name{plotSeasFac}
\docType{methods}
\alias{plotSeasFac}
\alias{plotSeasFac-methods}
\alias{plotSeasFac,x12Output-method}
\alias{plotSeasFac,x12Single-method}
\title{ ~~ Methods for Function \code{plotSeasFac} in Package \pkg{x12} ~~}
\description{
Seasonal factor plots with function \code{plotSeasFac} in package \pkg{x12}.
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
\S4method{plotSeasFac}{x12Output}(x,SI_Ratios=TRUE, ylab="Value", xlab="",
  lwd_seasonal=1, col_seasonal="black", lwd_mean=1, col_mean="blue",
  col_siratio="darkgreen",col_replaced="red", cex_siratio=.9, cex_replaced=.9,
  SI_Ratios_replaced=TRUE, plot_legend=TRUE, ...)
\S4method{plotSeasFac}{x12Single}(x,SI_Ratios=TRUE, ylab="Value", xlab="",lwd_seasonal=1,
  col_seasonal="black", lwd_mean=1, col_mean="blue", col_siratio="darkgreen",
  col_replaced="red", cex_siratio=.9, cex_replaced=.9, SI_Ratios_replaced=TRUE,
  plot_legend=TRUE, ...)

				
}


\arguments{
\item{x}{object of class \code{\link{x12Output-class}} or \code{\link{x12Single-class}}.} 
\item{SI_Ratios}{logical specifying if the SI ratios should be plotted.} 
\item{ylab}{label of the y-axis.} 
\item{xlab}{label of the x-axis.} 
\item{lwd_seasonal}{line width of the seasonal factors.} 
\item{col_seasonal}{color of the seasonal factors.} 
\item{lwd_mean}{line width of the mean.} 
\item{col_mean}{color of the mean.} 
\item{col_siratio}{color of the SI ratios.} 
\item{col_replaced}{color of the replaced SI ratios.} 
\item{cex_siratio}{magnification factor for the size of the symbols used for plotting the SI ratios.} 
\item{cex_replaced}{magnification factor for the size of the symbols used for plotting the replaced SI ratios.} 
\item{SI_Ratios_replaced}{logical specifying if the replaced SI ratios should be plotted.} 
\item{plot_legend}{logical specifying if a legend should be plotted.} 
\item{...}{ignored.}
}

\author{Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{x12}},
\code{\link{plot}},
\code{\link{plotSpec}},
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
