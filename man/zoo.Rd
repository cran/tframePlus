\name{zoo}
\alias{tfstart.zoo}
\alias{tfstart.zootframe}
\alias{tfend.zoo}
\alias{tfend.zootframe}
\alias{tfperiods.zoo}
\alias{periods.zootframe}

\alias{tframe.zoo}
\alias{tfSet.zootframe}
\alias{tfwindow.zoo}

\title{zoo Methods for tframe}
\description{Methods to extend tframe of zoo series}

\usage{
    \method{tfstart}{zoo}(x)
    \method{tfstart}{zootframe}(x)
    \method{tfend}{zoo}(x)
    \method{tfend}{zootframe}(x)
    \method{tfperiods}{zoo}(x)
    \method{tframe}{zoo}(x)
    \method{tfSet}{zootframe}(value, x) 
    \method{tfwindow}{zoo}(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
}

\arguments{
    \item{x}{a tframe or a tframed object.}
    \item{value}{a tframe.}
    \item{start}{A start date of a format compatible with the time series}
    \item{end}{An end date of a format compatible with the time series}
    \item{tf}{A tframe or tframed object}
    \item{warn}{A logical indicating if warning should be produced}
}
\value{Depends}
\details{
    These methods extend tframe generics.
}

\seealso{
    \code{\link[tframe]{tframe}}, 
    \code{\link[tframe]{tfwindow}}
    \code{\link[tframe]{tfstart}}, 
    \code{\link[tframe]{tfend}}
    \code{\link[tframe]{time}}
}
\keyword{internal}

