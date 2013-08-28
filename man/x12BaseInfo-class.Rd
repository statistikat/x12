\name{x12BaseInfo-class}
\Rdversion{1.1}
\docType{class}
\alias{x12BaseInfo-class}
\alias{x12BaseInfo}

\title{Class \code{"x12BaseInfo"}}
\description{
Baseinfo for use with the \code{\link{x12}} function and classes.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12BaseInfo", x12path, x13path, use, showWarnings)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{x12path}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{x13path}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{use}:}{Object of class \code{"character"} ~~ }
    \item{\code{showWarnings}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "x12BaseInfo" in the signature.
}
\author{
Alexander Kowarik
}


\seealso{
\code{\link{x12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}}
%\code{\linkS4class{x12BaseInfo}},
}
\examples{
showClass("x12BaseInfo")
}
\keyword{classes}
