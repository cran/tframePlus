is.tframed.zoo <- function(x) {TRUE}

tframe.zoo <- function (x) {
  tf <- index(x)
  class(tf) <- c( "zootframe", class(tf), "tframe")
  tf
  }

tfUnSet.zoo <- function(x)      {zoo:::coredata(x)}
tfSet.zootframe <- function(value, x) {zoo:::zoo(x, value)}

"seriesNames<-.zoo" <- function (x, value) 
  {if (is.matrix(x)) dimnames(x) <- list(NULL, value)
   else attr(x, "seriesNames") <- value
   x
  }

tfperiods.zoo <- function(x)  NROW(x)

tfstart.zootframe <- function(x) x[1]
tfend.zootframe   <- function(x) x[length(x)]
tfperiods.zootframe   <- function(x) length(x)
periods.zootframe     <- function(x) length(x)

tfwindow.zoo <- function(x, tf=NULL, start=tfstart(tf), end=tfend(tf), warn=TRUE)
  {# With the default warn=T warnings will be issued if no truncation takes
   #  place because start or end is outside the range of data.
   if (!warn) 
     {opts <- options(warn = -1)
      on.exit(options(opts))
     }
   y <- window(x, start=start, end=end)
   seriesNames(y) <- seriesNames(x)
   attr(y, "TSrefperiod") <- attr(x, "TSrefperiod")
   y
  }

tbind.zoo <- function(x, ..., pad.start=TRUE, pad.end=TRUE, warn=TRUE)
 {nm <- seriesNames(x)
  ref <- attr(x, "TSrefperiod")
  for (z in list(...)) {
    if (!is.null(z)) {
      nm  <- c(nm,  seriesNames(z))
      ref <- c(ref, attr(z, "TSrefperiod"))
      x <- cbind(x, z)
      }
    }
  if (!pad.start | !pad.end)
     x <- trimNA(x, startNAs= !pad.start, endNAs= !pad.end)
  seriesNames(x) <- nm
  attr(x, "TSrefperiod") <- ref
  x
 }  

