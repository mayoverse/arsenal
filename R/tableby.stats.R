#####################################################
## Testing and Summary stats methods for internal use in tableby
###########################################

## summary stats
## considerations: handling NAs, or other miss.val
#medianDate <- function(x, na.rm=TRUE) {
#  if(na.rm & sum(is.na(x))>0) {
#    x <- x[!is.na(x)]
#  }
#  medint <- median(as.integer(x))
#  browser()
#  return(median(xint))
#  return(as.Date(medint, origin="1970/01/01"))
#}
#range.Date <- function(x, na.rm=TRUE) {
#  if(na.rm & sum(is.na(x))>0) {
#    x <- x[!is.na(x)]
#  }
#  xint <- as.integer(x)
#  return(range(xint))
#}

## paste mean and sd  mean(sd)
#meansd <- function(x, na.rm=TRUE, weights=NULL, ...) {
#  c(mean(x, na.rm=na.rm), stats::sd(x, na.rm=na.rm))
#}

#' tableby Summary Statistics Functions
#' 
#' A collection of functions that will report summary statistics. To create a custom function,
#'   consider using a function with all three arguments and \code{...}. See the \code{\link{tableby}} vignette
#'   for an example.
#' 
#' @param x Usually a vector.
#' @param na.rm Should NAs be removed?
#' @param weights A vector of weights.
#' @param levels A vector of levels that character \code{x}s should have.
#' @param ... Other arguments.
#' @return Usually a vector of the appropriate numbers.
#' @details Not all these functions are exported, in order to avoid conflicting NAMESPACES.
#' @name tableby.stats
NULL
#> NULL

#' @rdname tableby.stats
#' @export
meansd <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  c(wtd.mean(x, weights=weights, na.rm=na.rm, ...), sqrt(wtd.var(x, weights=weights,na.rm=na.rm, ...)))
}

#' @rdname tableby.stats
#' @export
medianrange <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm & length(x)==sum(is.na(x))) {
    return(c(NA,NA,NA))
  } 
  wtd.quantile(x, probs=c(.5,0,1), na.rm, weights=weights, ...)
}

#' @rdname tableby.stats
median <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm & length(x)==sum(is.na(x))) {
    return(NA)
  } 
  if(class(x)=="Date") {
    as.Date(wtd.quantile(as.integer(x), weights=weights, probs=0.5, na.rm=na.rm, ...), origin="1970/01/01")
  } else {
    wtd.quantile(x, weights=weights, probs=0.5, na.rm=na.rm, ...)
  }
}

#' @rdname tableby.stats
range <- function(x, na.rm=TRUE, ...) {
  if(na.rm & length(x)==sum(is.na(x))) {
    return(c(NA,NA))
  } 
  if(class(x)=="Date") {
    as.Date(base::range(as.integer(x), na.rm=na.rm), origin="1970/01/01")
  } else {
    base::range(x, na.rm=na.rm)
  }
}


## survival stats
## implemented with using pre-calculated
##    kmsumm <- summary(survfit(Surv() ~ group))

## ' Nevents
## ' 
## ' Number of events in a survival object, within a group
## ' @param x a thing
## ' @param ... other arguments
## ' @return  the events stat from km$table
#' @rdname tableby.stats
#' @export
Nevents <- function(x, ...) {
  mat <- summary(x, ...)$table
  if(!any(c(grepl("^events", colnames(mat)),grepl("^events",names(mat))))) {
    stop("Survival endpoint may not be coded 0/1.\n")
  }  
  if (!is.null(nrow(mat))) {    
    rownames(mat) <- substr(rownames(mat), regexpr("=", rownames(mat)) + 
                              1, nchar(rownames(mat)))
    return(mat[, "events"])
  }
  return(as.numeric(mat["events"]))
}

## Median survival
## ' medSurv
## ' 
## ' Calculate median survival
## ' 
## ' @param x kaplan-meier summary object, used within tableby
## ' @param ... other arguments
## ' @return vector of median subjects who have survived by time point
#' @rdname tableby.stats
#' @export
medSurv <- function(x, ...) {
  mat <- summary(x, ...)$table
  if(!any(c(grepl("^events", colnames(mat)),grepl("^events",names(mat))))) {
    stop("Survival endpoint may not be coded 0/1.\n")
  }
  if(!is.null(nrow(mat))) {
    rownames(mat) <- substr(rownames(mat), regexpr("=",rownames(mat))+1, nchar(rownames(mat)))
    return(mat[,'median'])
  } 
  return(as.numeric(mat['median']))
}
## 
NeventsSurv <- function(x, times=1:5) {
  ## x is result of survfit() 
  xsumm <- summary(x, times=times)
  if(is.null(x$strata)) {
    byList <- data.frame(n.event=cumsum(xsumm$n.event), surv=100*xsumm$surv, row.names=xsumm$time)
  } else {
    
    mat <- with(xsumm, data.frame(time,n.risk, n.event, n.censor, surv, strata))    
    byList <- list()
    for(strat in unique(mat$strata)) {
      stratTrim <- substr(strat, regexpr("=", strat)+1, nchar(strat))
      ## could add any other column of mat to data.frame
      byList[[stratTrim]] <- with(mat[mat$strata==strat,],
                                  data.frame(n.event=cumsum(n.event),surv=100*surv, row.names=time))
      if(nrow(byList[[stratTrim]]) < length(times)) {
        byList[[stratTrim]] <- rbind.data.frame(byList[[stratTrim]],
                                                byList[[stratTrim]][nrow(byList[[stratTrim]]),])
        rownames(byList[[stratTrim]])[nrow(byList[[stratTrim]])] <- times[length(times)]
      }
    }
  }
  return(byList)
}
NriskSurv <- function(x, times=1:5) {
  ## x is result of survfit()
  xsumm <- summary(x, times=times)
  if(is.null(x$strata)) {
    byList <- data.frame(n.risk=xsumm$n.risk, row.names=xsumm$time)
  } else {    
    xsumm <- summary(x, times=times)
    mat <- with(xsumm, data.frame(time,n.risk, n.event, n.censor, surv, strata))
    byList <- list()
    for(strat in unique(mat$strata)) {
      ## could add any other column of mat to data.frame
      stratTrim <- substr(strat, regexpr("=", strat)+1, nchar(strat))
      byList[[stratTrim]] <- with(mat[mat$strata==strat,], data.frame(n.risk, row.names=time))
      if(nrow(byList[[stratTrim]]) < length(times)) {
        byList[[stratTrim]] <- rbind.data.frame(byList[[stratTrim]],
                                                byList[[stratTrim]][nrow(byList[[stratTrim]]),])
        rownames(byList[[stratTrim]])[nrow(byList[[stratTrim]])] <- times[length(times)]
        
      }
    }
  }
  return(byList)
}



## Can write similar functions for NcensorTime, NriskTime, etc.

## ' survNinterval
## ' 
## ' survival summary stat per N units of time. Default is years.
## ' 
## ' @param x              a Surv() variable within tableby formula
## ' @param x.by           the by-variable in tableby
## ' @param time.interval  the interval of units of time over which to summarize in categories
## ' @return     vector of number of events per time interval
survNinterval <- function(x, x.by, time.interval=1) {
  #kmsumm <- survfit(x~x.by,type="kaplan-meier") 
  nsurv <- as.matrix(x)[,1]
  breaks <- seq(0,max(nsurv)+time.interval, by=time.interval)
  tapply(cut(nsurv, breaks, levels=breaks[1:(length(breaks)-1)]), x.by, table, exclude=NA)
}

## quantiles
#' @rdname tableby.stats
#' @export
q1q3 <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm & length(x)==sum(is.na(x))) {
    return(c(NA,NA))
  } 
  wtd.quantile(x, weights=weights, probs=c(0.25, .75), na.rm=na.rm, ...)  
}

#' @rdname tableby.stats
#' @export
medianq1q3 <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm & length(x)==sum(is.na(x))) {
    return(c(NA,NA,NA))
  } 
  wtd.quantile(x, weights=weights, probs=c(0.5, 0.25, .75), na.rm=na.rm, ...)
}

## Inner-quartile range has a function IQR in R, but a wrapper
## would need to be written with weights in mind

## Count of missings: always show missings
#' @rdname tableby.stats
#' @export
Nmiss <- function(x, levels=NULL, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  sum(weights[is.na(x)])
}

## Nmiss2 make similar, but in tableby, always keep nmiss,
## even if there are no missings
Nmiss2 <- Nmiss

## count of complete samples
#' @rdname tableby.stats
#' @export
N <- function(x, levels=NULL, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  sum(weights[!is.na(x)])    
}

## count (pct) where pct is within group variable total
#' @rdname tableby.stats
#' @export
countpct <- function(x, levels=sort(unique(x)), na.rm=TRUE, weights=rep(1, length(x)), ...) {
  ##  tbl <- table(x[!is.na(x)])
  ## data.frame(count=as.vector(tbl), pct=100*as.vector(tbl)/sum(tbl), row.names=levels)
  wtbl <- wtd.table(factor(x[!is.na(x)], levels=levels), weights=weights[!is.na(x)], ...)
  df <- data.frame(count=as.vector(wtbl$sum.of.weights), pct=100*as.vector(wtbl$sum.of.weights)/sum(wtbl$sum.of.weights), row.names=if(length(wtbl$x)==length(levels)) levels else names(wtbl$sum.of.weights))
  ## make sure all levels are in df. If not, add them and re-order.
  if(nrow(df) < length(levels) ) {
    misslevs <- levels[!(levels %in% rownames(df))]
    df <- rbind.data.frame(df, data.frame(count=rep(0, length(misslevs)), pct=rep(0, length(misslevs)), row.names=misslevs))
  }
  return(df[as.character(levels),])
}
## format the countpct result for better printing (should work for meansd as well)
## Greg to edit this one
format.countpct <- function(x,digits=5, pct='') {
  if(!is.null(ncol(x))) {
    ## multiple rows
    xformat <- cbind.data.frame(format(x[,1], digits=digits), format(x[,2], digits=digits))
    rownames(xformat) <- rownames(x)
    digits <- digits - 2
    return (apply (xformat, 1, function(xrow) paste (xrow[1], " (", format (round (as.numeric (xrow[2]), digits), nsmall = digits), 
                                                     pct, ")", sep = "")))
  } else {
    ## just one row
    return(paste(signif(x[1],digits=digits), "(",signif(x[2],digits=digits), ")",sep=""))
  } 
}
