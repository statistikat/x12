\name{diagnostics-class}
\Rdversion{1.1}
\docType{class}
\alias{diagnostics-class}

\title{Class \code{"diagnostics"}}
\description{
The x12 binaries produce a file with the suffix .udg. This class is a list of a selection of its content.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("diagnostics", ...)}.
It is used internally by the methods for x12Batch and x12Single objects. 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
}
\author{
Alexander Kowarik
}
\examples{
showClass("diagnostics")
}
\keyword{classes}
