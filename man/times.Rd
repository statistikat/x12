\name{times}
\alias{times}
\alias{times-methods}
\alias{times,x12Output-method}
\alias{times,x12Single-method}
\title{
Read start and end of a x12Single or x12Output object
}
\description{
Combination of start() and end() for ts objects-
}
\section{Methods}{
\describe{

\item{\code{signature(x = "x12Output")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Single")}}{
%%  ~~describe this method here~~
}

}}

\usage{
  times(x)
  \S4method{times}{x12Output}(x)
  \S4method{times}{x12Single}(x)   
}
\arguments{
  \item{x}{a x12Single or x12Output object}
}
\value{
Returns a list with start and end for original, backcast and forecast timeseries
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
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
%\code{\link{summary}},
\code{\link{summary.x12work}},
\code{\link{x12work}}
}