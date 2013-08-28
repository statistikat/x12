\name{x12path}
\alias{x12path}
\alias{x13path}
\alias{putd}
\alias{x12env}
\alias{rmd}
\alias{existd}
\alias{getd}
\title{Function to interact with the environment x12env}
\description{
  \code{"x12env"} is used to store the x12path and x13path (and more for the GUI).
}
\usage{
x12env
x12path(path=NULL)
x13path(path=NULL)
putd(x,value)
getd(x, mode="any")
rmd(x)
existd(x, mode="any")
}
\arguments{
  \item{path}{The path to the X12 or X13 binaries.}
  \item{x}{a character for the name}
  \item{value}{value that should be assigned to the object with name x.}
  \item{mode}{the mode or type of object sought}
}
\author{Alexander Kowarik}
\examples{
  \dontrun{
  x12path("d:/x12/x12a.exe")
  x12path()
  getd("x12path")
  }
}
\seealso{\code{\link{get}}, \code{\link{assign}}, \code{\link{exists}}, \code{\link{x12}}}
\keyword{manip}


