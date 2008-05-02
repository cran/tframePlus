
tfpersp <- function (x, tf=tfspan(x), start=tfstart(tf), end=tfend(tf),
       theta = -30, phi = 15, scale = FALSE, 
       xlab = "Time", ylab = "", zlab = "", 
       aspect= c(0.5, 0.5), #y/time, z/time,
       ticktype="detailed",ltheta = -120, lphi = 15,
       ...) {
    # 
    if (!is.null(start)) x <- tfwindow(x, start = start, warn = FALSE)
    if (!is.null(end))   x <- tfwindow(x, end = end, warn = FALSE)    
    tline <- time(x)
    rx <- max(tline, na.rm=TRUE ) - min(tline, na.rm=TRUE)
    rz <- max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    ry <- NCOL(x) 
    persp(x=tline, y=(1:ncol(x))* aspect[1]*rx/ry, 
       z=x, expand=aspect[2]*rx/rz,
       theta = theta, phi = phi, scale = scale, 
       xlab=xlab, ylab=ylab,
       ticktype=ticktype,
       ltheta=ltheta, lphi=lphi) #, col = fcol, shade = 0.4,...)
    }

as.quarterly <- function (x, FUN=sum, na.rm=FALSE, ...){
    # convert to quarterly (from monthly only, so far
    #  use aggregate, but shift to match quarters
    if (4 == frequency(x)) return(if(na.rm) trimNA(x) else x) 
    if (12 != frequency(x)) stop("only monthly conversion supported for now.")
    tf <- tframe(x)
    nm <- seriesNames(x)
    x <- tfExpand(x, add.start=(start(x)[2] %% 3)-1,
                     add.end  =(3 - end(x)[2]) %% 3)
    r <- aggregate(x, nfrequency=4, FUN=FUN, 
        ndeltat=1, ts.eps=getOption("ts.eps"), ...) 
    if(na.rm) trimNA(r) else r
    }
