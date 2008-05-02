\name{as.quarterly}
\alias{as.quarterly}

\title{Convert series to Quarterly}
\description{Convert series to quarterly}

\usage{
    as.quarterly (x, FUN=sum, na.rm=FALSE, ...)
}

\arguments{
    \item{x}{a tframed object. (Only montly is currently working)}
    \item{FUN}{the function to use for aggregating.}
    \item{na.rm}{Logical indicating if NA should be removed from the beginning
    and end of a series}
    \item{...}{additional arguments passed to \code{aggregate}}
}
\value{Depends}
\details{
  This function uses aggregate, but shifts the data to match usualy economic and
  financial quarters (whereas aggregate simply groups three months together
  starting with the first). This function should give the same result as
  aggregate if the data starts and ends on quarter boundaries. If the data does
  not start and end on quarter boundaries then \code{NA} will be put in where 
  data is incomplete, and the quarter removed if \code{na.rm=TRUE}
}

\seealso{
    \code{\link[tframe]{tfwindow}}
    \code{\link[base]{aggregate}}
}
\keyword{utilities}
\keyword{ts}
