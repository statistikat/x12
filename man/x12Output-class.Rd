\name{x12Output-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Output-class}
\alias{x12Output}

\title{Class \code{"x12Output"}}
\description{
Output class for \code{\link{x12}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Output", ...)}.
%%  ~~ describe objects here ~~ 
}

\section{Slots}{
  \describe{
    \item{\code{a1}:}{Object of class \code{"ts"} - the original time series.}
    \item{\code{d10}:}{Object of class \code{"ts"} - the final seasonal factors.}
    \item{\code{d11}:}{Object of class \code{"ts"} - the final seasonally adjusted data.}
    \item{\code{d12}:}{Object of class \code{"ts"} - the final trend cycle.}
    \item{\code{d13}:}{Object of class \code{"ts"} - the final irregular components.}
    \item{\code{d16}:}{Object of class \code{"ts"} - the combined adjustment factors.}
    \item{\code{c17}:}{Object of class \code{"ts"} - the final weights for the irregular component.}
    \item{\code{d9}:}{Object of class \code{"ts"} - the final replacements for the SI ratios.}
    \item{\code{e2}:}{Object of class \code{"ts"} - the differenced, transformed, seasonally adjusted data.}
    \item{\code{d8}:}{Object of class \code{"ts"} - the final unmodified SI ratios.}
    \item{\code{b1}:}{Object of class \code{"ts"} - the prior adjusted original series.}
    \item{\code{td}:}{Object of class \code{"tsOrNULL"} - the trading day component}
    \item{\code{otl}:}{Object of class \code{"tsOrNULL"} - the outlier regression series}
    \item{\code{sp0}:}{Object of class \code{"spectrum"} - the spectrum of the original series.}
    \item{\code{sp1}:}{Object of class \code{"spectrum"} - the spectrum of the differenced seasonally adjusted series.}
    \item{\code{sp2}:}{Object of class \code{"spectrum"} - the spectrum of modified irregular series.}
    \item{\code{spr}:}{Object of class \code{"spectrum"} - the spectrum of the regARIMA model residuals.}
    \item{\code{forecast}:}{Object of class \code{"fbcast"} - the  point forecasts with prediction intervals}
    \item{\code{backcast}:}{Object of class \code{"fbcast"} - the  point backcasts with prediction intervals}
    \item{\code{dg}:}{Object of class \code{"list"}, containing several seasonal adjustment and regARIMA modeling diagnostics, i.e.:\cr
	\code{x11regress, transform, samode, seasonalma, trendma, arimamdl, automdl, regmdl, nout, nautoout, nalmostout, almostoutlier, crit, 
	outlier, userdefined, autooutlier, peaks.seas, peaks.td, id.seas, id.rsdseas, spcrsd, spcori, spcsa, spcirr, m1, m2, m3, m4, m5, m6, 
	m7, m8, m9, m10, m11, q, q2, nmfail, loglikelihood, aic, aicc, bic, hq, aape, autotransform, ifout, rsd.acf, rsd.pacf, rsd.acf2, 
	tsName, frequency, span,...}}
%    \item{\code{seats}:}{Object of class \code{"logical"} ~~ }
    \item{\code{file}:}{Object of class \code{"character"} - path to the output directory and filename}
    \item{\code{tblnames}:}{Object of class \code{"character"} - tables read into R}
    \item{\code{Rtblnames}:}{Object of class \code{"character"} - names of tables read into R}
  }
}

\section{Methods}{
  \describe{
    \item{\link{summary}}{\code{signature(object = "x12Output")}: ... }
    \item{\link{plot}}{\code{signature(object = "x12Output")}: ... }
	\item{\link{plotSpec}}{\code{signature(object = "x12Output")}: ... }
	\item{\link{plotSeasFac}}{\code{signature(object = "x12Output")}: ... }    
	\item{\link{plotRsdAcf}}{\code{signature(object = "x12Output")}: ... }    
	 }
}
\author{
Alexander Kowarik, Angelika Meraner
}

\seealso{
\code{\link{x12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
%\code{\link{summary}},
\code{\link{summary.x12work}},
\code{\link{x12work}}
}
\examples{
data(AirPassengersX12)
summary(AirPassengersX12)
showClass("x12Output")
}
\keyword{asummary}
