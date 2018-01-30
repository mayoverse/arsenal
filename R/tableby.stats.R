#####################################################
## Testing and Summary stats methods for internal use in tableby
###########################################

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
#' @param times A vector of times to use for survival summaries.
#' @param ... Other arguments.
#' @return Usually a vector of the appropriate numbers.
#' @details Not all these functions are exported, in order to avoid conflicting NAMESPACES.
#' @name tableby.stats
NULL
#> NULL

#' @rdname tableby.stats
#' @export
meansd <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- c(wtd.mean(x, weights=weights, na.rm=na.rm, ...), sqrt(wtd.var(x, weights=weights, na.rm=na.rm, ...)))
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"))
}

#' @rdname tableby.stats
#' @export
medianrange <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) rep(NA_real_, times = 3) else wtd.quantile(x, probs=c(0.5, 0, 1), na.rm=na.rm, weights=weights, ...)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
}

#' @rdname tableby.stats
median <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm && allNA(x)) {
    NA_real_
  } else if(is.Date(x)) {
    as.Date(wtd.quantile(as.integer(x), weights=weights, probs=0.5, na.rm=na.rm, ...), origin="1970/01/01")
  } else {
    wtd.quantile(x, weights=weights, probs=0.5, na.rm=na.rm, ...)
  }
}

#' @rdname tableby.stats
range <- function(x, na.rm=TRUE, ...) {
  y <- if(na.rm && allNA(x)) {
    c(NA_real_, NA_real_)
  } else if(is.Date(x)) {
    as.Date(base::range(as.integer(x), na.rm=na.rm), origin="1970/01/01")
  } else {
    base::range(x, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, sep = " - ")
}


## survival stats
#' @rdname tableby.stats
#' @export
Nevents <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), ...) {
  mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
  if("events" %nin% names(mat)) stop("Survival endpoint may not be coded 0/1.\n")
  as.numeric(mat["events"])
}

## Median survival
#' @rdname tableby.stats
#' @export
medSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), ...) {
  mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
  if("events" %nin% names(mat)) stop("Survival endpoint may not be coded 0/1.\n")
  as.numeric(mat["median"])
}

##
NeventsSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), times=1:5, ...) {
  xsumm <- summary(survival::survfit(x ~ 1, weights = weights), times=times)
  out <- t(cbind(cumsum(xsumm$n.event), 100*xsumm$surv))
  out <- setNames(as.list(as.data.frame(out)), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct, parens = c("(", ")")))
}

NriskSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), times=1:5, ...) {
  xsumm <- summary(survival::survfit(x ~ 1, weights = weights), times=times)
  out <- setNames(as.list(xsumm$n.risk), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct))
}

medTime <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), ...)
{
  wtd.quantile(as.matrix(x)[,1], weights=weights, probs=0.5, na.rm=na.rm, ...)
}

rangeTime <- function(x, na.rm = TRUE, ...)
{
  as.tbstat(base::range(as.matrix(x)[,1], na.rm=na.rm), sep = " - ")
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
  y <- if(na.rm && allNA(x)) {
    c(NA_real_, NA_real_)
  } else wtd.quantile(x, weights=weights, probs=c(0.25, .75), na.rm=na.rm, ...)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, sep = ", ")
}

#' @rdname tableby.stats
#' @export
medianq1q3 <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) {
    c(NA_real_, NA_real_, NA_real_)
  } else wtd.quantile(x, weights=weights, probs=c(0.5, 0.25, 0.75), na.rm=na.rm, ...)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
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
#' @rdname tableby.stats
#' @export
Nmiss2 <- Nmiss

## count of complete samples
#' @rdname tableby.stats
#' @export
N <- function(x, levels=NULL, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  sum(weights[!is.na(x)])
}

## count within group variable
#' @rdname tableby.stats
#' @export
count <- function (x, levels = sort(unique(x)), na.rm = TRUE, weights = rep(1, length(x)), ...)  {
  as.tbstat_multirow(lapply(as.list(wtd.table(factor(x[!is.na(x)], levels = levels), weights = weights[!is.na(x)], ...)), as.countpct))
}

## count (pct) where pct is within group variable total
#' @rdname tableby.stats
#' @export
countpct <- function(x, levels=sort(unique(x)), na.rm=TRUE, weights=rep(1, length(x)), ...) {
  wtbl <- wtd.table(factor(x[!is.na(x)], levels=levels), weights=weights[!is.na(x)], ...)
  as.tbstat_multirow(lapply(Map(c, wtbl, 100*wtbl/sum(wtbl)), as.countpct, parens = c("(", ")"), pct = "%"))
}
