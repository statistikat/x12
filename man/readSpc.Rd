\name{readSpc}
\alias{readSpc}
\title{
Function to read X12-spc Files in x12Parameter R objects
}
\description{
Still an early beta, so it will not work in specific situations
}
\usage{
readSpc(file,filename=TRUE)
}
\arguments{
  \item{file}{character vector containing filenames of spc files}
  \item{filename}{if TRUE the filename (without) ".spc" will be used as name for the serie}
}
\details{
Not all arguments of an X12 spc file are supported, but the parameters described in \code{\link{x12}} should be covered. 
}
\value{
The function returns an object of class "x12Single" if the file argument has length 1, otherwise it returns an
"x12Batch" object.
}
\author{
Alexander Kowarik
}
\seealso{
\code{\link{x12}}
}
\examples{
\dontrun{
  x12SingleObject1 <- readSpc("D:/aaa.spc")
  x12SingleObject2 <- readSpc("D:/ak_b.SPC")
  x12BatchObject1 <- readSpc(c("D:/ak_b.SPC","D:/aaa.spc"))
  setwd("M:/kowarik/Test/x12test")
  lf <- list.files()
  lf <- lf[unlist(lapply(lf,function(x)substr(x,nchar(x)-2,nchar(x)))) \%in\%c("spc","SPC")]
  lf <- lf[-c(grep("ind",lf))]
  allSPC <- readSpc(lf)
  a <- x12(allSPC)  
  plot(a@x12List[[1]])
  summary(a@x12List[[1]])
}
}