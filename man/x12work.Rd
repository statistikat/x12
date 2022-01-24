\name{x12work}
\alias{x12work}
\title{Run x12 on an R TS-object}
\description{
A wrapper function for the x12 binaries. It creates a specification file for an R time series and runs x12,
afterwards the output is read into R.
}
\usage{x12work(tso,period=frequency(tso),file="Rout",
  series.span=NULL,series.modelspan=NULL,
 	transform.function="auto",transform.power=NULL,transform.adjust=NULL,
	regression.variables=NULL,regression.user=NULL,regression.file=NULL,
	regression.usertype=NULL,regression.centeruser=NULL,regression.start=NULL,
	regression.aictest=NULL,
	outlier.types=NULL,outlier.critical=NULL,outlier.span=NULL,outlier.method=NULL,
	identify=FALSE,identify.diff=NULL,identify.sdiff=NULL,identify.maxlag=NULL,
	arima.model=NULL,arima.smodel=NULL,arima.ar=NULL,arima.ma=NULL,
	automdl=FALSE,automdl.acceptdefault=FALSE,automdl.balanced=TRUE,
	automdl.maxorder=c(3,2),automdl.maxdiff=c(1,1),
	forecast_years=NULL,backcast_years=NULL,forecast_conf=.95,
	forecast_save="ftr",
	estimate=FALSE,estimate.outofsample=TRUE,
	check=TRUE,check.maxlag=NULL,
	slidingspans=FALSE,
	slidingspans.fixmdl=NULL,slidingspans.fixreg=NULL,
	slidingspans.length=NULL,slidingspans.numspans=NULL,
	slidingspans.outlier=NULL,
	slidingspans.additivesa=NULL,slidingspans.start=NULL,
	history=FALSE,
	history.estimates=NULL,history.fixmdl=FALSE,
	history.fixreg=NULL,history.outlier=NULL,
	history.sadjlags=NULL,history.trendlags=NULL,
	history.start=NULL,history.target=NULL,
	x11.sigmalim=c(1.5,2.5),x11.type=NULL,x11.sfshort=FALSE,x11.samode=NULL,
	x11.seasonalma=NULL,x11.trendma=NULL,
	x11.appendfcst=TRUE,x11.appendbcst=FALSE,x11.calendarsigma=NULL,
	x11.excludefcst=TRUE,x11.final="user",
	x11regression=FALSE,
	tblnames=NULL,Rtblnames=NULL,
	x12path=NULL,use="x12",keep_x12out=TRUE,showWarnings=TRUE)
%		seats=FALSE, seatsparameter=NULL,
		
}
\arguments{
  \item{tso}{a time series object.}
  \item{period}{frequency of the time series.}
  \item{file}{path to the output directory and filename, default is the working directory and \code{Rout}.*.}
  \item{series.span}{vector of length 4, limiting the data used for the calculations and analysis to a certain time interval.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}
  If the start or end date of the time series object should be used, the respective year and seasonal period are to be set to \code{NA}.}
  \item{series.modelspan}{vector of length 4, defining the start and end date of the time interval of the data 
  that should be used to determine all regARIMA model coefficients. Specified in the same way as \code{span}.}
  \item{transform.function}{transform parameter for x12 (\code{"auto"}, \code{"log"}, \code{"none"}).}
  \item{transform.power}{numeric value specifying the power of the Box Cox power transformation.}
  \item{transform.adjust}{determines the type of adjustment to be performed, 
    i.e. \code{transform.adjust="lom"} for length-of-month adjustment on monthly data, \code{transform.adjust="loq"} for length-of-quarter adjustment on quarterly data
    or \code{transform.adjust="lpyear"} for leap year adjustment of monthly or quarterly data (which is only allowed when either \code{transform.power=0} or \code{transform.function="log"}).}   
  \item{regression.variables}{character or character vector representing the names of the regression variables.}
  \item{regression.user}{character or character vector defining the user parameters in the regression argument.}
  \item{regression.file}{path to the file containing the data values of all \code{regression.user} variables.}
  \item{regression.usertype}{character or character vector assigning a type of model-estimated regression effect
  		on each user parameter in the regression argument (\code{"seasonal"}, \code{"td"}, \code{"lpyear"}, \code{"user"}, ...).\cr
  		By specifying a character vector of length greater one each variable can be given its own type.
  		Otherwise the same type will be used for all user parameters.}
  \item{regression.centeruser}{character specifying the removal of the (sample) mean or the seasonal means from 
  		the user parameters in the regression argument (\code{"mean"}, \code{"seasonal"}).\cr
  		Default is no modification of the respective user-defined regressors.}
  \item{regression.start}{start date for the values of the \code{regression.user} variables, specified as a vector of two integers in the format \code{c(year, seasonal period)}.}
  \item{regression.aictest}{character vector defining the regression variables for which an AIC test is to be performed.}
%  \item{outlier}{Object of class \code{"logical"} - if \code{TRUE}, the "outlier" specification will be enabled in the spc file.}
  \item{outlier.types}{to enable the "outlier" specification in the spc file, this parameter has to be defined by a character or character vector determining the method(s) used for outlier detection (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"all"}). }
  \item{outlier.critical}{number specifying the critical value used for outlier detection 
  		(same value used for all types of outliers)
  		or named list (possible names of list elements being \code{AO},\code{LS} and \code{TC})
  		where each list element specifies the respective critical value 
  		used for detecting the corresponding type of outlier.\cr
  		If not specified, the default critical value is used.}
  \item{outlier.span}{vector of length 2, defining the span for outlier detection.}
  \item{outlier.method}{character determining how detected outliers should be added to the model (\code{"addone"}, \code{"addall"}).
  If not specified,\code{"addone"} is used by default.}% (For x11 regression this argument cannot be used if the sigma argument is used.)
  \item{identify}{Object of class \code{"logical"} - if \code{TRUE}, the "identify" specification will be enabled in the spc file.}
  \item{identify.diff}{number or vector representing the orders of nonseasonal differences specified, default is 0.}
  \item{identify.sdiff}{number or vector representing the orders of seasonal differences specified, default is 0.}
  \item{identify.maxlag}{number of lags specified for the ACFs and PACFs, default is 36 for monthly series and 12 for quarterly series.}
  \item{arima.model}{vector of length 3, defining the arima parameters.}
  \item{arima.smodel}{vector of length 3, defining the sarima parameters.}
  \item{arima.ar}{numeric or character vector specifying the initial values for nonseasonal and seasonal autoregressive parameters in the order that they appear in the \code{arima.model} argument. Empty positions are created with NA.}
  \item{arima.ma}{numeric or character vector specifying the initial values for all moving average parameters in the order that they appear in the \code{arima.model} argument. Empty positions are created with NA.}
  \item{automdl}{\code{TRUE}/\code{FALSE} for activating auto modeling.}
  \item{automdl.acceptdefault}{logical for \code{automdl} defining whether the default model should be chosen if the Ljung-Box Q statistic 
  for its model residuals is acceptable.}
  \item{automdl.balanced}{logical for \code{automdl} defining whether the automatic model procedure will tend towards balanced
	models. \code{TRUE} yields the same preference as the TRAMO program.}
  \item{automdl.maxorder}{vector of length 2, maximum order for \code{automdl}. Empty positions are created with NA.}
  \item{automdl.maxdiff}{vector of length 2, maximum diff. order for \code{automdl}. Empty positions are created with NA.}
  \item{forecast_years}{number of years to forecast, default is 1 year.}
  \item{backcast_years}{number of years to backcast, default is no backcasts.}
  \item{forecast_conf}{probability for the confidence interval of forecasts}
  \item{forecast_save}{character either "ftr"(in transformed scaling) or "fct"(in original scaling)}
  \item{estimate}{if \code{TRUE}, the term "estimate" will be added to the spc file.}
  \item{estimate.outofsample}{logical defining whether "out of sample" or "within sample" forecast errors
  should be used in calculating the average magnitude of forecast errors over the last three years.}
  \item{check}{\code{TRUE}/\code{FALSE} for activating the "check" specification in the spc file.} 
  \item{check.maxlag}{the number of lags requested for the residual sample ACF and PACF, default is 24 for monthly series and 8 for quarterly series.}    
  \item{slidingspans}{if \code{TRUE}, "slidingspans" specification will be enabled in the spc file.}
  \item{slidingspans.fixmdl}{(\code{"yes"} (default), \code{"no"}, \code{"clear"}).}
  \item{slidingspans.fixreg}{character or character vector specifying the trading day, holiday, outlier or other user-defined regression effects to be fixed (\code{"td"}, \code{"holiday"}, \code{"outlier"}, \code{"user"}).
  	All other regression coefficients will be re-estimated for each sliding span.}
  \item{slidingspans.length}{numeric value specifying the length of each span in months or quarters (>3 years, <17 years).}
  \item{slidingspans.numspans}{numeric value specifying the number of sliding spans used to generate output for comparisons (must be between 2 and 4, inclusive).}
  \item{slidingspans.outlier}{(\code{"keep"} (default), \code{"remove"}, \code{"yes"}).}
  \item{slidingspans.additivesa}{(\code{"difference"} (default), \code{"percent"}).}
  \item{slidingspans.start}{specified as a vector of two integers in the format \code{c(start year, start seasonal period)}.}
  \item{history}{if \code{TRUE}, the \code{history} specification will be enabled.}
  \item{history.estimates}{character or character vector determining which estimates from the regARIMA modeling and/or the x11 seasonal adjustment will be analyzed in the history analysis (\code{"sadj"} (default), \code{"sadjchng"}, \code{"trend"}, \code{"trendchng"}, \code{"seasonal"}, \code{"aic"}, \code{"fcst"}).}
  \item{history.fixmdl}{logical determining whether the regARIMA model will be re-estimated during the history analysis.}
  \item{history.fixreg}{character or character vector specifying the trading day, holiday, outlier or other user-defined regression effects to be fixed (\code{"td"}, \code{"holiday"}, \code{"outlier"}, \code{"user"}). All other coefficients will be re-estimated for each history span.}
  \item{history.outlier}{(\code{"keep"} (default), \code{"remove"}, \code{"auto"})}
  \item{history.sadjlags}{integer or vector specifying up to 5 revision lags (each >0) that will be analyzed in the revisions analysis of lagged seasonal adjustments.}
  \item{history.trendlags}{integer or vector specifying up to 5 revision lags (each >0) that will be used in the revision history of the lagged trend components.}
  \item{history.start}{specified as a vector of two integers in the format \code{c(start year, start seasonal period)}.}              
  \item{history.target}{character determining whether the revisions of the seasonal adjustments and trends calculated at the lags specified in \code{history.sadjlags} and \code{history.trendlags} should be defined by the deviation from the concurrent estimate or the deviation from the final estimate (\code{"final"} (default), \code{"concurrent"}).}  
  \item{x11.sigmalim}{vector of length 2, defining the limits for sigma in the x11 methodology,
  used to downweight extreme irregular values in the internal seasonal adjustment iterations.}
  \item{x11.type}{character, i.e. \code{"summary"}, \code{"trend"} or \code{"sa"}. If  \code{x11.type="trend"}, x11 will only be used to estimate the final trend-cycle as well as the irregular components and to adjust according to trading days.
  The default setting is \code{type="sa"} where a seasonal decomposition of the series is calculated.}  
%  \item{x11.type}{if \code{TRUE}, x11 will only be used to estimate trend and to adjust according to trading days.}  
  \item{x11.sfshort}{logical controlling the seasonal filter to be used if the series is at most 5 years long.
  If \code{TRUE}, the arguments of the \code{seasonalma} filter will be used wherever possible. 
  If \code{FALSE}, a stable seasonal filter will be used irrespective of \code{seasonalma}.}
  \item{x11.samode}{character defining the type of seasonal adjustment decomposition calculated
  (\code{"mult"}, \code{"add"}, \code{"pseudoadd"}, \code{"logadd"}).}
  \item{x11.seasonalma}{character or character vector of the format \code{c("snxm","snxm", ...)} 
  defining which seasonal nxm moving average(s) should be used for which calendar months or quarters 
  to estimate the seasonal factors.
  If only one ma is specified, the same ma will be used for all months or quarters.
  If not specified, the program will invoke an automatic choice.}
  \item{x11.trendma}{integer defining the type of Henderson moving average used for estimating
  the final trend cycle.
  If not specified, the program will invoke an automatic choice.}
  \item{x11.appendfcst}{logical defining whether forecasts should be included in certain x11 tables.}
  \item{x11.appendbcst}{logical defining whether forecasts should be included in certain x11 tables.}
  \item{x11.calendarsigma}{regulates the way the standard errors used for the detection and adjustment of
  extreme values should be computed (\code{"all"}, \code{"signif"}, \code{"select"} or no specification).}
  \item{x11.excludefcst}{logical defining if forecasts and backcasts from the regARIMA model
  should not be used in the generation of extreme values in the seasonal adjustment routines.}
  \item{x11.final}{character or character vector specifying which type(s) of prior adjustment factors should be
  removed from the final seasonally adjusted series (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"user"}, \code{"none"}).}
  \item{x11regression}{if \code{TRUE}, x11Regression will be performed using the respective regression and outlier commands above, 
  i.e. \code{regression.variables}, \code{regression.user}, \code{regression.file}, \code{regression.usertype}, \code{regression.centeruser} 
  and \code{regression.start} as well as \code{outlier.critical}, \code{outlier.span} and \code{outlier.method}.}
  \item{tblnames}{character vector of additional tables to be read into R.}
  \item{Rtblnames}{character vector naming the additional tables.}
  \item{x12path}{path to the x12 binaries, for example \code{d:\\x12a\\x12a.exe}.}
%  \item{x13path}{path to the x13 binaries, for example \code{d:\\x13\\x13.exe}.}
  \item{use}{\code{"x12"} or \code{"x13"}, at the moment only \code{"x12"} is tested properly.}
%  \item{seats}{\code{TRUE}/\code{FALSE} for activating SEATS, at the moment SEATS is not properly implemented.}
%  \item{seatsparameter}{string defining the seats parameter.}
  \item{keep_x12out}{if \code{TRUE}, the output files generated by x12 are stored in the folder "gra" in 
  the output directory and are not deleted at the end of a successful run.}
  \item{showWarnings}{logical defining whether warnings and notes generated by x12 should be returned.
  Errors will be displayed in any case.}
}
\details{
 Generates an x12 specification file, runs x12 and reads the output files. 
}

\value{
\code{x12work} returns an object of \link{class} \code{"x12"}.

The function \code{summary} is used to print a summary of the diagnostics results.

An object of class \code{"x12"} is a list containing at least the following components: 

\item{a1}{original time series}
\item{d10}{final seasonal factors}
\item{d11}{final seasonally adjusted data}
\item{d12}{final trend cycle}
\item{d13}{final irregular components}
\item{d16}{combined adjustment factors}
\item{c17}{final weights for irregular component}
\item{d9}{final replacements for SI ratios}
\item{e2}{differenced, transformed, seasonally adjusted data}
\item{d8}{final unmodified SI ratios}
\item{b1}{prior adjusted original series}
\item{forecast}{point forecasts with prediction intervals}
\item{backcast}{point backcasts with prediction intervals}
\item{dg}{a list containing several seasonal adjustment and regARIMA modeling diagnostics, i.e.:\cr
\code{x11regress, transform, samode, seasonalma, trendma, arimamdl, automdl, regmdl, nout, nautoout, 
nalmostout, almostoutlier, crit, outlier, userdefined, autooutlier, peaks.seas, peaks.td, id.seas, 
id.rsdseas, spcrsd, spcori, spcsa, spcirr, q, q2, nmfail, loglikelihood, aic, aicc, bic, hq, aape,
autotransform, ifout, res.acf, res.pacf, res.acf2,...}
}
\item{file}{path to the output directory and filename}
\item{tblnames}{tables read into R}
\item{Rtblnames}{names of tables read into R}
}

\seealso{
  \code{\link{x12}},
  \code{\link{ts}},
  \code{\link{summary.x12work}},
  \code{\link{plot.x12work}},
  \code{\link{x12-methods}}
}
\examples{
### Examples
data(AirPassengers)
\dontrun{
x12out <- x12work(AirPassengers,x12path=".../x12a.exe",transform.function="auto",
		arima.model=c(0,1,1),arima.smodel=c(0,1,1),regression.variables="lpyear",
		x11.sigmalim=c(2.0,3.0),outlier.types="all",outlier.critical=list(LS=3.5,TC=3),
		x11.seasonalma="s3x3")
summary(x12out)}	
}
\source{
  \url{https://www.census.gov/data/software/x13as.html}
}
\note{
 Only working with available x12 binaries.
}
\references{Alexander Kowarik, Angelika Meraner, Matthias Templ, Daniel
  Schopfhauser (2014). Seasonal Adjustment with the R Packages x12 and
  x12GUI. Journal of Statistical Software, 62(2), 1-21. URL
  http://www.jstatsoft.org/v62/i02/.
}
\author{Alexander Kowarik, Angelika Meraner}