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
#' @param by a vector of the by-values.
#' @param by.levels a vector of the levels of \code{by}.
#' @param ... Other arguments.
#' @return Usually a vector of the appropriate numbers.
#' @details Not all these functions are exported, in order to avoid conflicting NAMESPACES.
#' @seealso \code{\link{includeNA}}, \code{\link{tableby.control}}
#' @name tableby.stats
NULL
#> NULL

#' @rdname tableby.stats
#' @export
meansd <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x))
  {
    rep(NA_real_, times = 2)
  } else c(wtd.mean(x, weights=weights, na.rm=na.rm), sqrt(wtd.var(x, weights=weights, na.rm=na.rm)))
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"))
}

#' @rdname tableby.stats
#' @export
medianrange <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) rep(NA_real_, times = 3) else wtd.quantile(x, probs=c(0.5, 0, 1), na.rm=na.rm, weights=weights)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
}

#' @rdname tableby.stats
median <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) {
    NA_real_
  } else if(is.Date(x)) {
    as.Date(wtd.quantile(as.integer(x), weights=weights, probs=0.5, na.rm=na.rm), origin="1970/01/01")
  } else {
    wtd.quantile(x, weights=weights, probs=0.5, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL)
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
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
    if("events" %nin% names(mat)) stop("Survival endpoint may not be coded 0/1.\n")
    as.numeric(mat["events"])
  }
  as.countpct(y)
}

## Median survival
#' @rdname tableby.stats
#' @export
medSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
    if("events" %nin% names(mat)) stop("Survival endpoint may not be coded 0/1.\n")
    as.numeric(mat["median"])
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
NeventsSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), times=1:5, ...) {
  y <- if(na.rm && allNA(x))
  {
    matrix(NA_real_, nrow = 2, ncol = length(times))
  } else
  {
    xsumm <- summary(survival::survfit(x ~ 1, weights = weights), times=times)
    t(cbind(cumsum(xsumm$n.event), 100*xsumm$surv))
  }
  out <- stats::setNames(as.list(as.data.frame(y)), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct, parens = c("(", ")")))
}

#' @rdname tableby.stats
#' @export
NriskSurv <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), times=1:5, ...) {
  y <- if(na.rm && allNA(x))
  {
    rep(NA_real_, times = length(times))
  } else summary(survival::survfit(x ~ 1, weights = weights), times=times)$n.risk
  out <- stats::setNames(as.list(y), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct))
}

#' @rdname tableby.stats
#' @export
medTime <- function(x, na.rm = TRUE, weights = rep(1, nrow(x)), ...)
{
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else wtd.quantile(as.matrix(x)[,1], weights=weights, probs=0.5, na.rm=na.rm)
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
rangeTime <- function(x, na.rm = TRUE, ...)
{
  y <- if(na.rm && allNA(x))
  {
    c(NA_real_, NA_real_)
  } else base::range(as.matrix(x)[,1], na.rm=na.rm)
  as.tbstat(y, sep = " - ")
}

## quantiles
#' @rdname tableby.stats
#' @export
q1q3 <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) {
    c(NA_real_, NA_real_)
  } else wtd.quantile(x, weights=weights, probs=c(0.25, .75), na.rm=na.rm)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, sep = ", ")
}

#' @rdname tableby.stats
#' @export
medianq1q3 <- function(x, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  y <- if(na.rm && allNA(x)) {
    c(NA_real_, NA_real_, NA_real_)
  } else wtd.quantile(x, weights=weights, probs=c(0.5, 0.25, 0.75), na.rm=na.rm)
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
}

## Inner-quartile range has a function IQR in R, but a wrapper
## would need to be written with weights in mind

## Count of missings: always show missings
#' @rdname tableby.stats
#' @export
Nmiss <- function(x, levels=NULL, na.rm=TRUE, weights=rep(1, length(x)), ...) {
  as.countpct(sum(weights[is.na(x)]))
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
  as.countpct(sum(weights[!is.na(x)]))
}

## count within group variable
#' @rdname tableby.stats
count <- function (x, levels = sort(unique(x)), na.rm = TRUE, weights = rep(1, length(x)), ...)  {
  as.tbstat_multirow(lapply(as.list(wtd.table(factor(x[!is.na(x)], levels = levels), weights = weights[!is.na(x)])), as.countpct))
}

## count (pct) where pct is within group variable total
#' @rdname tableby.stats
#' @export
countpct <- function(x, levels=sort(unique(x)), na.rm=TRUE, weights=rep(1, length(x)), ...) {
  wtbl <- wtd.table(factor(x[!is.na(x)], levels=levels), weights=weights[!is.na(x)])
  as.tbstat_multirow(lapply(Map(c, wtbl, 100*wtbl/sum(wtbl)), as.countpct, parens = c("(", ")"), pct = "%"))
}

#' @rdname tableby.stats
#' @export
countrowpct <- function(x, levels=sort(unique(x)), by, by.levels=sort(unique(by)), na.rm=TRUE, weights=rep(1, length(x)), ...) {
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by) & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  wtbls <- lapply(levels, function(L) {
    tmp <- wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L])
    c(tmp, Total = sum(tmp))
  })
  pcts <- lapply(wtbls, function(tab) c(100*tab/utils::tail(tab, 1)))

  nms <- c(by.levels, "Total")
  transpose <- function(what) stats::setNames(lapply(nms, function(i) stats::setNames(lapply(what, "[", i), levels)), nms)
  wtbls <- transpose(wtbls)
  pcts <- transpose(pcts)

  combine <- function(elt1, elt2) as.countpct(unname(c(elt1, elt2)), parens = c("(", ")"), pct = "%")
  Map(function(L1, L2) as.tbstat_multirow(Map(combine, L1, L2)), wtbls, pcts)

}



######## internal functions that we use above ########

wtd.table <- function(x, weights = rep(1, length(x)), na.rm = TRUE)
{
  tmp <- tapply(weights, x, sum, na.rm = na.rm)
  tmp[is.na(tmp)] <- 0 # (tapply(default = 0) would be enough in R >= 3.4, but we'll make this backwards-compatible)
  tmp
}

wtd.mean <- function(x, weights = NULL, na.rm = TRUE) {
  if(!length(weights)) return(mean(x, na.rm = na.rm))
  if(na.rm) {
    idx <- !is.na(x + weights)
    x <- x[idx]
    weights <- weights[idx]
  }
  sum(weights * x)/sum(weights)
}

wtd.quantile <- function(x, weights=NULL, probs=c(0,0.25,0.5,0.75,1), na.rm=TRUE) {

  if(!length(weights)) return(stats::quantile(x, probs = probs, na.rm = na.rm))
  if(any(probs < 0) || any(probs > 1)) stop("Probabilities must be between 0 and 1 inclusive")

  wts <- wtd.table(x, weights, na.rm = na.rm)
  x <- as.numeric(names(wts))
  n <- sum(wts)
  order <- 1 + (n - 1) * probs
  low <- pmax(floor(order), 1)
  high <- pmin(low + 1, n)
  order <- order%%1
  allq <- stats::approx(cumsum(wts), x, xout = c(low, high), method = "constant", f = 1, rule = 2)$y
  k <- length(probs)
  stats::setNames((1 - order) * allq[1:k] + order * allq[-(1:k)], probs)
}

wtd.var <- function(x, weights = NULL, na.rm=TRUE, method = c("unbiased", "ML")) {
  method <- match.arg(method)
  if(!length(weights)) return(stats::var(x, na.rm = na.rm))

  if(na.rm) {
    idx <- !is.na(x + weights)
    x <- x[idx]
    weights <- weights[idx]
  }
  as.numeric(stats::cov.wt(matrix(x, ncol = 1), weights, method = method)$cov)
}
