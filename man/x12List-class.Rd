\name{x12List-class}
\Rdversion{1.1}
\docType{class}
\alias{x12List-class}
\alias{x12List}

\title{Class \code{"x12List"}}
\description{
Support class for \code{\link{x12Batch-class}} containing multiple \code{\link{x12Single-class}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12List", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class \code{"list"}, distance 2.
}
\section{Methods}{
No methods defined with class \code{"x12List"} in the signature.
}
\author{
Alexander Kowarik
}

\seealso{
\code{\link{x12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
%\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}}
}

\examples{
showClass("x12List")
}
\keyword{classes}
