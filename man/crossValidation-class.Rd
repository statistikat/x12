\name{crossValidation-class}
\Rdversion{1.1}
\docType{class}
\alias{crossValidation-class}

\title{Class \code{"crossValidation"}}
\description{
Standardized object for saving the output of \code{crossVal} in R. %Used by the function \code{crossVal} in this package.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("crossValidation", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{backcast}:}{Object of class \code{"dfOrNULL"} ~~ }
    \item{\code{forecast}:}{Object of class \code{"dfOrNULL"} ~~ }
  }
}
\author{
Alexander Kowarik, Angelika Meraner
}
\examples{
showClass("crossValidation")
}
\keyword{classes}
