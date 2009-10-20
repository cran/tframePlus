\name{TSwriteXLS}
\alias{TSwriteXLS}

\title{Write Time Series to an .xls Spreadsheet File}
\description{Write one or more time series to a .xls spreadsheet file}

\usage{
    TSwriteXLS(x, ..., FileName="R.xls", SheetNames=NULL,
               verbose = FALSE)
}

\arguments{
    \item{x}{a time series or matrix of time series.)}
    \item{...}{additional time series objects (like x).}
    \item{FileName}{a string to use for the file name.}
    \item{SheetNames}{Logical indicating if NA should be removed from the beginning
    and end of a series}
    \item{verbose}{logical passed to WriteXLS.}
}
\value{logical}
\details{
  This functions uses \code{WriteXLS} to write time series data to an .xls spreadsheet
  file. If the object \code{x} has mulitple series then they will be put on
  the same sheet. Additional objects in \dots will be put on additional pages.
  
  If \code{SheetNames} is \code{NULL} (the default) then the sheet names will 
  be generated from the first series name in each object. If \code{SheetNames}
  is supplied then it should have an element for \code{x} and for each object 
  in \dots.
  
  If \code{WriteXLS} does not find a version of perl with appropriate modules
  then a work around is attempted using \code{save}.
  }
\seealso{
    \code{\link[WriteXLS]{WriteXLS}}
}
\examples{
 tofile <- tempfile()
 
 z <- ts(1:10, start=c(1999,2), freq=12)
 seriesNames(z) <- "ser 1"
 TSwriteXLS(z, FileName=tofile) 

 zz <- tbind(z, diff(z))
 seriesNames(zz) <- c("ser 1", "diff")
 TSwriteXLS(zz, FileName=tofile,  SheetNames="2 series")

 zz <- ts(1:10, start=c(1999,1), freq=1)
 seriesNames(zz) <- "annual ser"
 TSwriteXLS(z, zz, FileName=tofile,  SheetNames=c("monthly", "annual"))

 unlink(tofile) 
}
\keyword{utilities}
\keyword{ts}
