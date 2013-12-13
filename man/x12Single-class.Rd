\name{x12Single-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Single-class}
\alias{x12Single}

\title{Class \code{"x12Single"}}
\description{
Class consisting of all information for \code{\link{x12}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Single", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ts}:}{Object of class \code{\linkS4class{ts}}}
    \item{\code{x12Parameter}:}{Object of class \code{\link{x12Parameter-class}}}
    \item{\code{x12Output}:}{Object of class \code{\link{x12Output-class}}}
    \item{\code{x12OldParameter}:}{Object of class \code{\linkS4class{list}}}
    \item{\code{x12OldOutput}:}{Object of class \code{\linkS4class{list}}}
    \item{\code{tsName}:}{Object of class \code{characterOrNULL}}
    \item{\code{firstRun}:}{Object of class \code{\linkS4class{logical}}}
  }
}
\section{Methods}{
  \describe{
   	\item{\code{\link{setP}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{getP}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{prev}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{cleanArchive}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{loadP}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{saveP}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{summary}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{x12}}}{\code{signature(object = "x12Single")}}
    \item{\code{\link{plot}}}{\code{signature(object = "x12Single")}}
	\item{\code{\link{crossVal}}}{\code{signature(object = "x12Single")}}
	\item{\code{\link{plotSpec}}}{\code{signature(object = "x12Single")}}
	\item{\code{\link{plotSeasFac}}}{\code{signature(object = "x12Single")}}    
	\item{\code{\link{plotRsdAcf}}}{\code{signature(object = "x12Single")}}
	\item{\code{\link{cleanHistory}}}{\code{signature(object = "x12Single")}}    
    
	 }
}
\author{
Alexander Kowarik}
\note{cleanHistory is deprecated and cleanArchive should be used instead.}
\seealso{
\code{\link{x12}},
%\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
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
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regression.variables="AO1950.1",outlier.types="all",
  outlier.critical=list(LS=3.5,TC=2.5)))
s <- x12(s)
}
}
\keyword{classes}
