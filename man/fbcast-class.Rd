\name{fbcast-class}
\Rdversion{1.1}
\docType{class}
\alias{fbcast-class}

\title{Class \code{"fbcast"}}
\description{
Objects to save estimate, lowerci and upperci of fore- and/or backcasts in one standardized list. Used by the functions in this package.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fbcast", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{estimate}:}{Object of class \code{"ts"} ~~ }
    \item{\code{lowerci}:}{Object of class \code{"ts"} ~~ }
    \item{\code{upperci}:}{Object of class \code{"ts"} ~~ }
  }
}
\author{
Alexander Kowarik
}
\examples{
showClass("fbcast")
}
\keyword{classes}
