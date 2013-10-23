\name{plot-methods}
\docType{methods}
\alias{plot-methods}
%\alias{plot,ANY-method}
%\alias{plot,spectrum-method}
\alias{plot,x12Output-method}
\alias{plot,x12Single-method}
\alias{plot,x12Batch-method}
\alias{plot}

\title{ ~~ Methods for Function \code{plot} in Package \pkg{x12} ~~}
\description{
Plot function for \code{\link{x12}} output in package \pkg{x12}.
}
\section{Methods}{
\describe{

%\item{\code{signature(x = "ANY")}}{
%%  ~~describe this method here~~
%}

%\item{\code{signature(x = "spectrum")}}{
%%  ~~describe this method here~~
%}
\item{\code{signature(x = "x12Output")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Single")}}{
%%  ~~describe this method here~~
}
}}

\usage{
\S4method{plot}{x12Single}(x, original=TRUE, sa=FALSE, trend=FALSE, log_transform=FALSE,
  ylab="Value", xlab="Date", main="TS", col_original="black", col_sa="blue",
  col_trend="green", lwd_original=1, lwd_sa=1, lwd_trend=1, lty_sa=1, lty_trend=1, ytop=1,
  showAllout=FALSE, showAlloutLines=FALSE, showOut=NULL, annComp=TRUE, annCompTrend=TRUE,
  col_ao="red", col_ls="red", col_tc="red", col_annComp="grey", lwd_out=1, cex_out=1.5,
  pch_ao=4, pch_ls=2, pch_tc=23, plot_legend=TRUE, legend_horiz=TRUE, legend_bty="o",
  forecast=FALSE, backcast=FALSE,
  showCI=TRUE, col_fc="#2020ff", col_bc="#2020ff", col_ci="#d1d1ff",
  col_cishade="#d1d1ff", lty_original=1, lty_fc=2, lty_bc=2, lty_ci=1, lwd_fc=1, lwd_bc=1,
  lwd_ci=1, points_bc=FALSE, points_fc=FALSE, points_original=FALSE, showLine=FALSE,
  col_line="grey", lty_line=3, ylim=NULL, span=NULL, ...)
\S4method{plot}{x12Batch}(x, what="ask",original=TRUE, sa=FALSE, trend=FALSE, log_transform=FALSE,
  ylab="Value", xlab="Date", main="TS", col_original="black", col_sa="blue",
  col_trend="green", lwd_original=1, lwd_sa=1, lwd_trend=1, lty_sa=1, lty_trend=1, ytop=1,
  showAllout=FALSE, showAlloutLines=FALSE, showOut=NULL, annComp=TRUE, annCompTrend=TRUE,
  col_ao="red", col_ls="red", col_tc="red", col_annComp="grey", lwd_out=1, cex_out=1.5,
  pch_ao=4, pch_ls=2, pch_tc=23, plot_legend=TRUE, legend_horiz=TRUE, legend_bty="o",
  forecast=FALSE, backcast=FALSE,
  showCI=TRUE, col_fc="#2020ff", col_bc="#2020ff", col_ci="#d1d1ff",
  col_cishade="#d1d1ff", lty_original=1, lty_fc=2, lty_bc=2, lty_ci=1, lwd_fc=1, lwd_bc=1,
  lwd_ci=1, points_bc=FALSE, points_fc=FALSE, points_original=FALSE, showLine=FALSE,
  col_line="grey", lty_line=3, ylim=NULL, span=NULL, ...)
\S4method{plot}{x12Output}(x, original=TRUE, sa=FALSE, trend=FALSE, log_transform=FALSE,
  ylab="Value", xlab="Date", main="TS", col_original="black", col_sa="blue",
  col_trend="green", lwd_original=1, lwd_sa=1, lwd_trend=1, lty_sa=1, lty_trend=1, ytop=1,
  showAllout=FALSE, showAlloutLines=FALSE, showOut=NULL, annComp=TRUE, annCompTrend=TRUE,
  col_ao="red", col_ls="red", col_tc="red", col_annComp="grey", lwd_out=1, cex_out=1.5,
  pch_ao=4, pch_ls=2, pch_tc=23, plot_legend=TRUE, legend_horiz=TRUE, legend_bty="o",
  forecast=FALSE, backcast=FALSE, showCI=TRUE,
  col_fc="#2020ff", col_bc="#2020ff", col_ci="#d1d1ff", col_cishade="#d1d1ff",
  lty_original=1, lty_fc=2, lty_bc=2, lty_ci=1, lwd_fc=1, lwd_bc=1, lwd_ci=1,
  points_bc=FALSE, points_fc=FALSE, points_original=FALSE,
  showLine=FALSE, col_line="grey", lty_line=3, ylim=NULL, span=NULL, ...)
				%plot(x,y,...) 
}

\arguments{
\item{x}{object of class \code{\link{x12Output-class}} or \code{\link{x12Single-class}}.}
%\item{y}{ignored.}
\item{original}{logical defining whether the original time series should be plotted.}
\item{sa}{logical defining whether the seasonally adjusted time series should be plotted.}
\item{trend}{logical defining whether the trend should be plotted.}
\item{log_transform}{logical defining whether the log transform should be plotted.}
\item{showAllout}{logical defining whether all outliers should be plotted.}
\item{showOut}{character in the format \code{"TypeYear.Seasonalperiod"} defining a specific outlier to be plotted.}
\item{annComp}{logical defining whether an annual comparison should be performed for the outlier defined in \code{showOut}.}
\item{forecast}{logical defining whether the forecasts should be plotted.}
\item{backcast}{logical defining whether the backcasts should be plotted.}
\item{showCI}{logical defining whether the prediction intervals should be plotted.}
\item{ylab}{label of y-axis.}
\item{xlab}{label of x-axis.}
\item{main}{plot title.}
\item{col_original}{color of the original time series.}
\item{col_sa}{color of the seasonally adjusted time series.}
\item{col_trend}{color of the trend.}
\item{lwd_original}{line width of the original time series.}
\item{lwd_sa}{line width of the seasonally adjusted time series.}
\item{lwd_trend}{line width of the trend.}
\item{lty_original}{line type of the original time series.}
\item{lty_sa}{line type of the seasonally adjusted time series.}
\item{lty_trend}{line type of the trend.}
\item{ytop}{multiplication factor for \code{ylim}.}
\item{showAlloutLines}{logical specifying if vertical lines should be plotted with the outliers.}
\item{annCompTrend}{logical specifying if the trend of the annual comparison should be plotted.}
\item{col_ao}{color of additive outliers.}
\item{col_ls}{color of level shifts.}
\item{col_tc}{color of transitory changes.}
\item{col_annComp}{color of annual comparison.}
\item{lwd_out}{line width of outliers.}
\item{cex_out}{magnification factor for size of symbols used for plotting outliers.}
\item{pch_ao}{symbols used for additive outliers.}
\item{pch_ls}{symbols used for level shifts.}
\item{pch_tc}{symbols used for transitory changes.}
\item{plot_legend}{logical specifying if a legend should be plotted.}
\item{legend_horiz}{Orientation of the legend}
\item{legend_bty}{the type of box to be drawn around the legend. The allowed values are "o" (the default) and "n".}
\item{col_fc}{color of forecasts.}
\item{col_bc}{color of backcasts.}
\item{col_ci}{color of prediction interval.}
\item{col_cishade}{color of prediction interval shading.}
\item{lty_fc}{line type of forecasts.}
\item{lty_bc}{line type of backcasts.}
\item{lty_ci}{line type of prediction interval.}
\item{lwd_fc}{line width of forecasts.}
\item{lwd_bc}{line width of backcasts.}
\item{lwd_ci}{line width of prediction interval.}
\item{points_bc}{logical specifying if backcasts should additionally be indicated with points.}
\item{points_fc}{logical specifying if forecasts should additionally be indicated with points.}
\item{points_original}{logical specifying if the original time series should additionally be indicated with points.}
\item{showLine}{logical indicating if a boundary line should be drawn before/after fore-/backcasts.}
\item{col_line}{color of \code{showLine}.}
\item{lty_line}{line type of \code{showLine}.}
\item{ylim}{range of the y-axis.}
\item{span}{vector of length 4, limiting the data used for the plot.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}
  }
\item{what}{How multiple plots should be treated. "ask" is the only option at the moment.}
\item{...}{ignored.}
}

\author{Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{plotSpec}},
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
plot(s,showAllout=TRUE,span=c(1951,1,1953,12),points_original=TRUE,cex_out=2)
#with showOut
plot(s,showOut="AO1960.Jun",sa=FALSE,trend=FALSE,annComp=TRUE,log_transform=TRUE)
plot(s,showOut="AO1958.Mar",sa=TRUE,trend=TRUE,annComp=TRUE,annCompTrend=FALSE)
plot(s,showOut="AO1950.Jun",annComp=FALSE,cex_out=3,pch_ao=19,col_ao="orange")
plot(s,showOut="TC1954.Mar",span=c(1954,1,1955,12))
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
