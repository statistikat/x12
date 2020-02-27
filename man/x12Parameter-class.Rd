\name{x12Parameter-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Parameter-class}
\alias{x12Parameter}

\title{Class \code{"x12Parameter"}}
\description{
Parameter class for \code{\link{x12}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Parameter", ...)}.
%%  ~~ describe objects here ~~ 
}


\section{Slots}{
  \describe{
    %\item{\code{period}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{series.span}:}{Object of class \code{"numericOrNULLOrcharacter"} - vector of length 4, limiting the data used for the calculations and analysis to a certain time interval.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}
  If the start or end date of the time series object should be used, the respective year and seasonal period are to be set to \code{NA}.}  
    \item{\code{series.modelspan}:}{Object of class \code{"numericOrNULLOrcharacter"} - vector of length 4, defining the start and end date of the time interval of the data 
  that should be used to determine all regARIMA model coefficients. Specified in the same way as \code{span}.}
    %\item{\code{decimals}:}{Object of class \code{"numeric"} - number of decimal places of the data.}
    \item{\code{transform.function}:}{Object of class \code{"character"} - transform parameter for x12 (\code{"auto"}, \code{"log"}, \code{"none"}).}
    \item{\code{transform.power}:}{Object of class \code{"numericOrNULL"} - numeric value specifying the power of the Box Cox power transformation.}   
    \item{\code{transform.adjust}:}{Object of class \code{"characterOrNULL"} - determines the type of adjustment to be performed, 
    i.e. \code{transform.adjust="lom"} for length-of-month adjustment on monthly data, \code{transform.adjust="loq"} for length-of-quarter adjustment on quarterly data
    or \code{transform.adjust="lpyear"} for leap year adjustment of monthly or quarterly data (which is only allowed when either \code{transform.power=0} or \code{transform.function="log"}).}   
    \item{\code{regression.variables}:}{Object of class \code{"characterOrNULL"} - character or character vector representing the names of the regression variables.}
    \item{\code{regression.user}:}{Object of class \code{"characterOrNULL"} - character or character vector defining the user parameters in the regression argument.}
    \item{\code{regression.file}:}{Object of class \code{"characterOrNULL"} - path to the file containing the data values of all \code{regression.user} variables.}
    \item{\code{regression.usertype}:}{Object of class \code{"characterOrNULL"} - character or character vector assigning a type of model-estimated regression effect
  		on each user parameter in the regression argument (\code{"seasonal"}, \code{"td"}, \code{"lpyear"}, \code{"user"}, ...).\cr
  		By specifying a character vector of length greater one each variable can be given its own type.
  		Otherwise the same type will be used for all user parameters.}
    \item{\code{regression.centeruser}:}{Object of class \code{"characterOrNULL"} - character specifying the removal of the (sample) mean or the seasonal means from 
  		the user parameters in the regression argument (\code{"mean"}, \code{"seasonal"}).\cr
  		Default is no modification of the respective user-defined regressors.}
    \item{\code{regression.start}:}{Object of class \code{"numericOrNULLOrcharacter"} - start date for the values of the \code{regression.user} variables, specified as a vector of two integers in the format \code{c(year, seasonal period)}.}
    \item{\code{regression.aictest}:}{Object of class \code{"characterOrNULL"} - character vector defining the regression variables for which an AIC test is to be performed.}
%  	\item{\code{outlier}:}{Object of class \code{"logical"} - if \code{TRUE}, the "outlier" specification will be enabled in the spc file.}
    \item{\code{outlier.types}:}{Object of class \code{"characterOrNULL"} - to enable the "outlier" specification in the spc file, this parameter has to be defined by a character or character vector determining the method(s) used for outlier detection (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"all"}).}
    \item{\code{outlier.critical}:}{Object of class \code{"listOrNULLOrnumeric"} - number specifying the critical value used for outlier detection 
  		(same value used for all types of outliers)
  		or named list (possible names of list elements being \code{AO},\code{LS} and \code{TC})
  		where each list element specifies the respective critical value 
  		used for detecting the corresponding type of outlier.\cr
  		If not specified, the default critical value is used.}  
    \item{\code{outlier.span}:}{Object of class \code{"numericOrNULLOrcharacter"} - vector of length 4, defining the span for outlier detection. Specified in the same way as \code{span}.}
    \item{\code{outlier.method}:}{Object of class \code{"characterOrNULL"} - character determining how detected outliers should be added to the model (\code{"addone"}, \code{"addall"}).
  If not specified,\code{"addone"} is used by default.}% (For x11 regression this argument cannot be used if the sigma argument is used.)
  	\item{\code{identify}:}{Object of class \code{"logical"} - if \code{TRUE}, the "identify" specification will be enabled in the spc file.}
  	\item{\code{identify.diff}:}{Object of class \code{"numericOrNULL"} - number or vector representing the orders of nonseasonal differences specified, default is 0.}
  	\item{\code{identify.sdiff}:}{Object of class \code{"numericOrNULL"} - number or vector representing the orders of seasonal differences specified, default is 0.}
  	\item{\code{identify.maxlag}:}{Object of class \code{"numericOrNULL"} - number of lags specified for the ACFs and PACFs, default is 36 for monthly series and 12 for quarterly series.}
    \item{\code{arima.model}:}{Object of class \code{"numericOrNULL"} - vector of length 3, defining the arima parameters.} 
    \item{\code{arima.smodel}:}{Object of class \code{"numericOrNULL"} - vector of length 3, defining the sarima parameters.}
  	\item{\code{arima.ar}:}{Object of class \code{"numericOrNULLOrcharacter"} - numeric or character vector specifying the initial values for nonseasonal and seasonal autoregressive parameters in the order that they appear in the \code{arima.model} argument. Empty positions are created with NA.}
  	\item{\code{arima.ma}:}{Object of class \code{"numericOrNULLOrcharacter"} - numeric or character vector specifying the initial values for all moving average parameters in the order that they appear in the \code{arima.model} argument. Empty positions are created with NA.}
    \item{\code{automdl}:}{Object of class \code{"logical"} - \code{TRUE}/\code{FALSE} for activating auto modeling.}   
    \item{\code{automdl.acceptdefault}:}{Object of class \code{"logical"} - logical for \code{automdl} defining whether the default model should be chosen if the Ljung-Box Q statistic 
  for its model residuals is acceptable.}
    \item{\code{automdl.balanced}:}{Object of class \code{"logical"} - logical for \code{automdl} defining whether the automatic model procedure will tend towards balanced
	models. \code{TRUE} yields the same preference as the TRAMO program.}
    \item{\code{automdl.maxorder}:}{Object of class \code{"numeric"} - vector of length 2, specifying the maximum order for \code{automdl}. Empty positions are created with NA.}
    \item{\code{automdl.maxdiff}:}{Object of class \code{"numeric"} - vector of length 2, specifying the maximum diff. order for \code{automdl}. Empty positions are created with NA.}
    \item{\code{forecast_years}:}{Object of class \code{"numericOrNULL"} - number of years to forecast, default is 1 year.}
    \item{\code{backcast_years}:}{Object of class \code{"numericOrNULL"} - number of years to backcast, default is no backcasts.}
    \item{\code{forecast_conf}:}{Object of class \code{"numeric"} - probability for the confidence interval of forecasts.}
    \item{\code{forecast_save}:}{Object of class \code{"character"} either "ftr"(in transformed scaling) or "fct"(in original scaling)}
    \item{\code{estimate}:}{Object of class \code{"logical"} - if \code{TRUE}, the term "estimate" will be added to the spc file.}
    \item{\code{estimate.outofsample}:}{Object of class \code{"logical"} - logical defining whether "out of sample" or "within sample" forecast errors
  should be used in calculating the average magnitude of forecast errors over the last three years.}
  	\item{\code{check}:}{Object of class \code{"logical"} - \code{TRUE}/\code{FALSE} for activating the "check" specification in the spc file.} 
  	\item{\code{check.maxlag}:}{Object of class \code{"numericOrNULL"} - the number of lags requested for the residual sample ACF and PACF, default is 24 for monthly series and 8 for quarterly series.}    
    \item{\code{slidingspans}:}{Object of class \code{"logical"} - if \code{TRUE}, "slidingspans" specification will be enabled in the spc file.}
  	\item{\code{slidingspans.fixmdl}:}{Object of class \code{"characterOrNULL"} - (\code{"yes"} (default), \code{"no"}, \code{"clear"}).}
  	\item{\code{slidingspans.fixreg}:}{Object of class \code{"characterOrNULL"} - character or character vector specifying the trading day, holiday, outlier or other user-defined regression effects to be fixed (\code{"td"}, \code{"holiday"}, \code{"outlier"}, \code{"user"}).
  	All other regression coefficients will be re-estimated for each sliding span.}
  	\item{\code{slidingspans.length}:}{Object of class \code{"numericOrNULL"} - numeric value specifying the length of each span in months or quarters (>3 years, <17 years).}
  	\item{\code{slidingspans.numspans}:}{Object of class \code{"numericOrNULL"} - numeric value specifying the number of sliding spans used to generate output for comparisons (must be between 2 and 4, inclusive).}
  	\item{\code{slidingspans.outlier}:}{Object of class \code{"characterOrNULL"} - (\code{"keep"} (default), \code{"remove"}, \code{"yes"}).}
  	\item{\code{slidingspans.additivesa}:}{Object of class \code{"characterOrNULL"} - (\code{"difference"} (default), \code{"percent"}).}
  	\item{\code{slidingspans.start}:}{Object of class \code{"numericOrNULLOrcharacter"} - specified as a vector of two integers in the format \code{c(start year, start seasonal period)}.}
  	\item{\code{history}:}{if \code{TRUE}, the \code{history} specification will be enabled.}
  	\item{\code{history.estimates}:}{Object of class \code{"characterOrNULL"} - character or character vector determining which estimates from the regARIMA modeling and/or the x11 seasonal adjustment will be analyzed in the history analysis (\code{"sadj"} (default), \code{"sadjchng"}, \code{"trend"}, \code{"trendchng"}, \code{"seasonal"}, \code{"aic"}, \code{"fcst"}).}
  	\item{\code{history.fixmdl}:}{Object of class \code{"logical"} - logical determining whether the regARIMA model will be re-estimated during the history analysis.}
  	\item{\code{history.fixreg}:}{Object of class \code{"characterOrNULL"} - character or character vector specifying the trading day, holiday, outlier or other user-defined regression effects to be fixed (\code{"td"}, \code{"holiday"}, \code{"outlier"}, \code{"user"}). All other coefficients will be re-estimated for each history span.}
  	\item{\code{history.outlier}:}{Object of class \code{"characterOrNULL"} - (\code{"keep"} (default), \code{"remove"}, \code{"auto"})}
  	\item{\code{history.sadjlags}:}{Object of class \code{"numericOrNULL"} - integer or vector specifying up to 5 revision lags (each >0) that will be analyzed in the revisions analysis of lagged seasonal adjustments.}
  	\item{\code{history.trendlags}:}{Object of class \code{"numericOrNULL"} - integer or vector specifying up to 5 revision lags (each >0) that will be used in the revision history of the lagged trend components.}
  	\item{\code{history.start}:}{Object of class \code{"numericOrNULLOrcharacter"} - specified as a vector of two integers in the format \code{c(start year, start seasonal period)}.}          
    \item{\code{history.target}:}{Object of class \code{"characterOrNULL"} - character determining whether the revisions of the seasonal adjustments and trends calculated at the lags specified in \code{history.sadjlags} and \code{history.trendlags} should be defined by the deviation from the concurrent estimate or the deviation from the final estimate (\code{"final"} (default), \code{"concurrent"}).}
    \item{\code{x11.sigmalim}:}{Object of class \code{"numericOrNULL"} - vector of length 2, defining the limits for sigma in the x11 methodology,
  used to downweight extreme irregular values in the internal seasonal adjustment iterations.}
    \item{\code{x11.type}:}{Object of class \code{"characterOrNULL"} - character, i.e. \code{"summary"}, \code{"trend"} or \code{"sa"}. If  \code{x11.type="trend"}, 
  x11 will only be used to estimate the final trend-cycle as well as the irregular components and to adjust according to trading days.
  The default setting is \code{type="sa"} where a seasonal decomposition of the series is calculated.}
 %   \item{\code{x11.type}:}{Object of class "logical" - if \code{TRUE}, x11 will only be used to estimate trend and to adjust according to trading days.}
    \item{\code{x11.sfshort}:}{Object of class \code{"logical"} - logical controlling the seasonal filter to be used if the series is at most 5 years long.
  If \code{TRUE}, the arguments of the \code{seasonalma} filter will be used wherever possible. 
  If \code{FALSE}, a stable seasonal filter will be used irrespective of \code{seasonalma}.}
    \item{\code{x11.samode}:}{Object of class \code{"characterOrNULL"} - character defining the type of seasonal adjustment decomposition calculated
  (\code{"mult"}, \code{"add"}, \code{"pseudoadd"}, \code{"logadd"}).}
    \item{\code{x11.seasonalma}:}{Object of class \code{"characterOrNULL"} - character or character vector of the format \code{c("snxm","snxm", ...)} 
  defining which seasonal nxm moving average(s) should be used for which calendar months or quarters 
  to estimate the seasonal factors.
  If only one ma is specified, the same ma will be used for all months or quarters.
  If not specified, the program will invoke an automatic choice.}
    \item{\code{x11.trendma}:}{Object of class \code{"numericOrNULL"} - integer defining the type of Henderson moving average used for estimating
  the final trend cycle.
  If not specified, the program will invoke an automatic choice.}
    \item{\code{x11.appendfcst}:}{Object of class \code{"logical"} - logical defining whether forecasts should be included in certain x11 tables.}
    \item{\code{x11.appendbcst}:}{Object of class \code{"logical"} - logical defining whether forecasts should be included in certain x11 tables.}
    \item{\code{x11.calendarsigma}:}{Object of class \code{"characterOrNULL"} - regulates the way the standard errors used for the detection and adjustment of
  extreme values should be computed (\code{"all"}, \code{"signif"}, \code{"select"} or no specification).}
    \item{\code{x11.excludefcst}:}{Object of class \code{"logical"} - logical defining if forecasts and backcasts from the regARIMA model
  should not be used in the generation of extreme values in the seasonal adjustment routines.}
    \item{\code{x11.final}:}{Object of class \code{"character"} - character or character vector specifying which type(s) of prior adjustment factors should be
  removed from the final seasonally adjusted series (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"user"}, \code{"none"}).}
    \item{\code{x11regression}:}{Object of class \code{"logical"} - if \code{TRUE}, x11Regression will be performed using the respective regression and outlier commands above, 
    i.e. \code{regression.variables}, \code{regression.user}, \code{regression.file}, \code{regression.usertype}, \code{regression.centeruser} and \code{regression.start} as well as \code{outlier.critical}, \code{outlier.span} and \code{outlier.method}.}
%	\item{\code{seats}:}{Object of class \code{"logical"} - \code{TRUE}/\code{FALSE} for activating SEATS, at the moment SEATS is not properly implemented.}
%    \item{\code{seatsparameter}:}{Object of class \code{"characterOrNULL"} - string defining the seats parameter.}
    
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{getP}}}{\code{signature(object = "x12Parameter")}: ... }
    \item{\code{\link{setP}}}{\code{signature(object = "x12Parameter")}: ... }
	 }
}
\author{
Alexander Kowarik, Angelika Meraner}

%\seealso{
%\code{\link{x12}},
%\code{\linkS4class{x12Single}},
%\code{\linkS4class{x12Batch}},
%\code{\linkS4class{x12Parameter}},
%\code{\linkS4class{x12List}},
%\code{\linkS4class{x12Output}},
%\code{\linkS4class{x12BaseInfo}},
%\code{\link{summary.x12work}},\code{\link{x12work}}
%}
\examples{
showClass("x12Parameter")
}
\keyword{classes}
