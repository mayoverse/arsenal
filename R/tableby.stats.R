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
#' @param conf.level Numeric, denoting what confidence level to use for confidence intervals.
#' @param ... Other arguments.
#' @param totallab What to call the total "column"
#' @return Usually a vector of the appropriate numbers.
#' @details Not all these functions are exported, in order to avoid conflicting NAMESPACES.
#'   Note also that the functions prefixed with \code{"arsenal_"} can be referred to by their short names
#'   (e.g., \code{"min"} for \code{"arsenal_min"}).
#' @seealso \code{\link{includeNA}}, \code{\link{tableby.control}}
#' @name tableby.stats
NULL
#> NULL

get_stat_function <- function(x) switch(x, sum = , min = , max = , range = , mean = , sd = , var = , median = paste0("arsenal_", x), x)

#' @rdname tableby.stats
arsenal_sum <- function(x, na.rm=TRUE, ...) {
  y <- if(na.rm && allNA(x)) {
    NA_real_
  } else {
    sum(x, na.rm=na.rm)
  }
  as.tbstat(y) # unclear what the sum of dates should be?
}

#' @rdname tableby.stats
arsenal_min <- function(x, na.rm=TRUE, ...) {
  y <- if(na.rm && allNA(x)) {
    NA_real_
  } else {
    min(x, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL)
}

#' @rdname tableby.stats
arsenal_max <- function(x, na.rm=TRUE, ...) {
  y <- if(na.rm && allNA(x)) {
    NA_real_
  } else {
    max(x, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL)
}

#' @rdname tableby.stats
arsenal_mean <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    wtd.mean(x, weights=weights, na.rm=na.rm)
  }
  as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL)
}

#' @rdname tableby.stats
arsenal_sd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    s <- sqrt(wtd.var(x, weights=weights, na.rm=na.rm))
    if(is.Date(x)) list(as.difftime(s, units = "days")) else s
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
arsenal_var <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    wtd.var(x, weights=weights, na.rm=na.rm)
    # if(is.Date(x)) as.difftime(s, units = "days") else s
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
meansd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x))
  {
    as.tbstat(NA_real_)
  } else {
    m <- wtd.mean(x, weights=weights, na.rm=na.rm)
    s <- sqrt(wtd.var(x, weights=weights, na.rm=na.rm))
    y <- if(is.Date(x)) list(as.character(m), as.difftime(s, units = "days")) else c(m, s)
    as.tbstat(y, parens = c("(", ")"))
  }
}

#' @rdname tableby.stats
#' @export
meanse <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x))
  {
    as.tbstat(NA_real_)
  } else {
    if(!is.null(weights)) stop("'meanse' can only be used without weights")
    m <- mean(x, na.rm=na.rm)
    s <- stats::sd(x, na.rm=na.rm)/sqrt(sum(!is.na(x)))
    y <- if(is.Date(x)) list(as.character(m), as.difftime(s, units = "days")) else c(m, s)
    as.tbstat(y, parens = c("(", ")"))
  }
}

#' @rdname tableby.stats
#' @export
meanpmsd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x))
  {
    as.tbstat(NA_real_)
  } else {
    m <- wtd.mean(x, weights=weights, na.rm=na.rm)
    s <- sqrt(wtd.var(x, weights=weights, na.rm=na.rm))
    y <- if(is.Date(x)) list(as.character(m), as.difftime(s, units = "days")) else c(m, s)
    as.tbstat(y, parens = c("&pm; ", ""))
  }
}

#' @rdname tableby.stats
#' @export
meanpmse <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x))
  {
    as.tbstat(NA_real_)
  } else {
    if(!is.null(weights)) stop("'meanse' can only be used without weights")
    m <- mean(x, na.rm=na.rm)
    s <- stats::sd(x, na.rm=na.rm)/sqrt(sum(!is.na(x)))
    y <- if(is.Date(x)) list(as.character(m), as.difftime(s, units = "days")) else c(m, s)
    as.tbstat(y, parens = c("&pm; ", ""))
  }
}

#' @rdname tableby.stats
#' @export
meanCI <- function(x, na.rm=TRUE, weights = NULL, conf.level = 0.95, ...) {
  if(!is.null(weights) || (na.rm && allNA(x)))
  {
    as.tbstat(NA_real_)
  } else
  {
    if(na.rm) x <- x[!is.na(x)]
    s <- stats::sd(x, na.rm = na.rm)
    m <- mean(x, na.rm = na.rm)
    n <- length(x)
    a <- (1 - conf.level)/2
    y <- c(m, m + stats::qt(c(a, 1 - a), df = n - 1) * s / sqrt(n))
    as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
  }
}

#' @rdname tableby.stats
#' @export
medianrange <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x)) {
    as.tbstat(NA_real_)
  } else {
    y <- wtd.quantile(x, probs=c(0.5, 0, 1), na.rm=na.rm, weights=weights)
    as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
  }
}


#' @rdname tableby.stats
#' @export
medianmad <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(!is.null(weights) || (na.rm && allNA(x)))
  {
    as.tbstat(NA_real_)
  } else {
    m <- stats::median(x, na.rm=na.rm)
    s <- stats::mad(x, na.rm=na.rm, constant = 1)
    y <- if(is.Date(x)) list(as.character(m), as.difftime(s, units = "days")) else c(m, s)
    as.tbstat(y, parens = c("(", ")"))
  }
}


#' @rdname tableby.stats
arsenal_median <- function(x, na.rm=TRUE, weights = NULL, ...) {
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
arsenal_range <- function(x, na.rm=TRUE, ...) {
  if(na.rm && allNA(x)) {
    as.tbstat(NA_real_)
  } else {
    y <- range(x, na.rm=na.rm)
    as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, sep = " - ")
  }
}

#' @rdname tableby.stats
#' @export
gmean <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if((na.rm && allNA(x)) || any(x < 0, na.rm = TRUE) || is.Date(x))
  {
    NA_real_
  } else {
    exp(wtd.mean(log(x), weights=weights, na.rm=na.rm))
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
gsd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if((na.rm && allNA(x)) || any(x <= 0, na.rm = TRUE) || is.Date(x))
  {
    NA_real_
  } else {
    n <- sum(!is.na(x))
    exp(sqrt(wtd.var(log(x), weights = weights, na.rm = na.rm) * (n-1)/n))
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
gmeansd <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if((na.rm && allNA(x)) || any(x < 0, na.rm = TRUE) || is.Date(x))
  {
    as.tbstat(NA_real_)
  } else {
    m <- exp(wtd.mean(log(x), weights=weights, na.rm=na.rm))
    n <- sum(!is.na(x))
    s <- if(any(x == 0, na.rm = TRUE)) {
      NA_real_
    } else exp(sqrt(wtd.var(log(x), weights = weights, na.rm = na.rm) * (n-1)/n))
    as.tbstat(c(m, s), parens = c("(", ")"))
  }
}

#' @rdname tableby.stats
#' @export
gmeanCI <- function(x, na.rm=TRUE, weights = NULL, conf.level = 0.95, ...) {
  if(!is.null(weights) || (na.rm && allNA(x)) || any(x < 0, na.rm = TRUE) || is.Date(x))
  {
    as.tbstat(NA_real_)
  } else
  {
    if(na.rm) x <- x[!is.na(x)]
    n <- length(x)
    s <- sqrt(stats::var(log(x), na.rm = na.rm) * (n-1)/n)
    m <- mean(log(x), na.rm = na.rm)
    a <- (1 - conf.level)/2
    ci <- if(any(x == 0, na.rm = TRUE)) NA_real_ else m + stats::qt(c(a, 1 - a), df = n - 1) * s / sqrt(n)
    as.tbstat(exp(c(m, ci)), parens = c("(", ")"), sep2 = ", ")
  }
}

#' @rdname tableby.stats
#' @export
Nsigntest <- function(x, na.rm = TRUE, weights = NULL, ...) {
  if(is.null(weights)) weights <- rep(1, NROW(x))
  as.countpct(sum(weights*(x != 0), na.rm = na.rm))
}

## survival stats
#' @rdname tableby.stats
#' @export
Nevents <- function(x, na.rm = TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    check_pkg("survival")
    mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
    as.numeric(mat["events"])
  }
  as.countpct(y)
}

## Median survival
#' @rdname tableby.stats
#' @export
medSurv <- function(x, na.rm = TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else {
    check_pkg("survival")
    mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
    as.numeric(mat["median"]) # if we don't hit the median, or if all obs are censors, this is NA
  }
  as.tbstat(y)
}

#' @rdname tableby.stats
#' @export
NeventsSurv <- function(x, na.rm = TRUE, weights = NULL, times=1:5, ...) {
  if(na.rm && allNA(x))
  {
    y <- matrix(NA_real_, nrow = 1, ncol = length(times))
    a <- TRUE
  } else
  {
    check_pkg("survival")
    xsumm <- summary(survival::survfit(x ~ 1, weights = weights), times=times)
    y <- t(cbind(cumsum(xsumm$n.event), 100*xsumm$surv))
    a <- FALSE
  }
  out <- stats::setNames(as.list(as.data.frame(y)), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct, parens = if(a) NULL else c("(", ")"), which.pct = 2L))
}

#' @rdname tableby.stats
#' @export
NriskSurv <- function(x, na.rm = TRUE, weights = NULL, times=1:5, ...) {
  if(na.rm && allNA(x))
  {
    y <- matrix(NA_real_, nrow = 1, ncol = length(times))
    a <- TRUE
  } else {
    check_pkg("survival")
    xsumm <- summary(survival::survfit(x ~ 1, weights = weights), times=times)
    y <- t(cbind(xsumm$n.risk, 100*xsumm$surv))
    a <- FALSE
  }
  out <- stats::setNames(as.list(as.data.frame(y)), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct, parens = if(a) NULL else c("(", ")"), which.pct = 2L))
}

#' @rdname tableby.stats
#' @export
Nrisk <- function(x, na.rm = TRUE, weights = NULL, times=1:5, ...) {
  y <- if(na.rm && allNA(x))
  {
    rep(NA_real_, times = length(times))
  } else
  {
    check_pkg("survival")
    summary(survival::survfit(x ~ 1, weights = weights), times=times)$n.risk
  }
  out <- stats::setNames(as.list(y), paste0("time = ", times))
  as.tbstat_multirow(lapply(out, as.countpct))
}

#' @rdname tableby.stats
#' @export
medTime <- function(x, na.rm = TRUE, weights = NULL, ...)
{
  y <- if(na.rm && allNA(x))
  {
    NA_real_
  } else
  {
    x[, 2] <- 1 - x[, 2] # censor events instead
    check_pkg("survival")
    mat <- summary(survival::survfit(x ~ 1, weights = weights))$table
    as.numeric(mat["median"]) # if we don't hit the median, or if all obs are events, this is NA
  }
  as.tbstat(y)
}

## quantiles
#' @rdname tableby.stats
#' @export
q1q3 <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x)) {
    as.tbstat(NA_real_)
  } else {
    y <- wtd.quantile(x, weights=weights, probs=c(0.25, .75), na.rm=na.rm)
    as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, sep = ", ")
  }
}

#' @rdname tableby.stats
#' @export
medianq1q3 <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(na.rm && allNA(x)) {
    as.tbstat(NA_real_)
  } else {
    y <- wtd.quantile(x, weights=weights, probs=c(0.5, 0.25, 0.75), na.rm=na.rm)
    as.tbstat(y, oldClass = if(is.Date(x)) "Date" else NULL, parens = c("(", ")"), sep2 = ", ")
  }
}

#' @rdname tableby.stats
#' @export
iqr <- function(x, na.rm=TRUE, weights = NULL, ...) {
  y <- if(na.rm && allNA(x)) {
    NA_real_
  } else {
    s <- diff(wtd.quantile(x, weights=weights, probs=c(0.25, 0.75), na.rm=na.rm))
    if(is.Date(x)) list(as.difftime(s, units = "days")) else s
  }

  as.tbstat(y)
}

## Count of missings: always show missings
#' @rdname tableby.stats
#' @export
Nmiss <- function(x, weights = NULL, ...) {
  if(is.null(weights)) weights <- rep(1, NROW(x))
  weights <- weights[is.na(x) | is.na(weights)]
  as.countpct(sum(weights))
}

## Nmiss2 make similar, but in tableby, always keep nmiss,
## even if there are no missings
#' @rdname tableby.stats
#' @export
Nmiss2 <- Nmiss

## count of complete samples
#' @rdname tableby.stats
#' @export
N <- function(x, na.rm=TRUE, weights = NULL, ...) {
  if(is.null(weights)) weights <- rep(1, NROW(x))
  if(na.rm) weights <- weights[!is.na(x) & !is.na(weights)]
  as.countpct(sum(weights))
}

#' @rdname tableby.stats
#' @export
Npct <- function(x, levels=NULL, by, by.levels=sort(unique(by)), na.rm=TRUE, weights = NULL, ..., totallab = "Total") {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  tmp <- wtd.table(factor(by, levels = by.levels), weights = weights)
  wtbl <- c(tmp, stats::setNames(sum(tmp), totallab))
  lapply(wtbl, function(elt) as.countpct(c(elt, 100*elt/sum(tmp)), parens = c("(", ")"), pct = "%", which.pct = 2L))
}

## count within group variable
#' @rdname tableby.stats
count <- function (x, levels=NULL, na.rm = TRUE, weights = NULL, ...)  {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    weights <- weights[idx]
  }
  if(is.selectall(x))
  {
    if(is.null(weights)) weights <- rep(1, nrow(x))
    wtbl <- apply(as.matrix(x) == 1, 2, function(y) sum(weights[y]))
  } else wtbl <- wtd.table(factor(x, levels=levels), weights=weights)
  as.tbstat_multirow(lapply(as.list(wtbl), as.countpct))
}

## count (pct) where pct is within group variable total
#' @rdname tableby.stats
#' @export
countpct <- function(x, levels=NULL, na.rm=TRUE, weights = NULL, ...) {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    weights <- weights[idx]
  }
  if(is.selectall(x))
  {
    if(is.null(weights)) weights <- rep(1, nrow(x))
    wtbl <- apply(as.matrix(x) == 1, 2, function(y) sum(weights[y]))
    denom <- sum(weights)
  } else
  {
    wtbl <- wtd.table(factor(x, levels=levels), weights=weights)
    denom <- sum(wtbl)
  }
  a <- any(wtbl > 0)
  as.tbstat_multirow(lapply(Map(c, wtbl, if(a) 100*wtbl/denom else rep(list(NULL), times = length(wtbl))),
                            as.countpct, parens = if(a) c("(", ")") else NULL, pct = if(a) "%" else NULL, which.pct = 2L))
}

#' @rdname tableby.stats
#' @export
pct <- function(x, levels=NULL, na.rm=TRUE, weights = NULL, ...) {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    weights <- weights[idx]
  }
  if(is.selectall(x))
  {
    if(is.null(weights)) weights <- rep(1, nrow(x))
    wtbl <- apply(as.matrix(x) == 1, 2, function(y) sum(weights[y]))
    denom <- sum(weights)
  } else
  {
    wtbl <- wtd.table(factor(x, levels=levels), weights=weights)
    denom <- sum(wtbl)
  }
  as.tbstat_multirow(lapply(if(any(wtbl > 0)) 100*wtbl/denom else rep(list(NULL), times = length(wtbl)),
                            as.countpct, pct = "%", which.pct = 1L))
}

#' @rdname tableby.stats
#' @export
countN <- function(x, levels=NULL, na.rm=TRUE, weights = NULL, ...) {
  if(is.null(levels)) levels <- sort(unique(x))
  wtbl <- wtd.table(factor(x[!is.na(x)], levels=levels), weights=weights[!is.na(x)])
  n <- sum(wtbl)
  as.tbstat_multirow(lapply(Map(c, wtbl, rep(n, times = length(wtbl))), as.countpct, sep = "/"))
}

transpose_list <- function(x, levels, by.levels)
  stats::setNames(lapply(by.levels, function(i) as.tbstat_multirow(stats::setNames(lapply(x, "[[", i), levels))), by.levels)


#' @rdname tableby.stats
#' @export
countrowpct <- function(x, levels=NULL, by, by.levels=sort(unique(by)), na.rm=TRUE, weights = NULL, ..., totallab = "Total") {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  wtbls <- lapply(levels, function(L) {
    tmp <- wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L])
    wtbl <- c(tmp, stats::setNames(sum(tmp), totallab))
    lapply(wtbl, function(elt) as.countpct(c(elt, 100*elt/sum(tmp)), parens = c("(", ")"), pct = "%", which.pct = 2L))
  })
  transpose_list(wtbls, levels, c(by.levels, totallab))
}

#' @rdname tableby.stats
#' @export
rowpct <- function(x, levels=NULL, by, by.levels=sort(unique(by)), na.rm=TRUE, weights = NULL, ..., totallab = "Total") {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  wtbls <- lapply(levels, function(L) {
    tmp <- wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L])
    wtbl <- c(tmp, stats::setNames(sum(tmp), totallab))
    lapply(wtbl, function(elt) as.countpct(100*elt/sum(tmp), pct = "%", which.pct = 1L))
  })
  transpose_list(wtbls, levels, c(by.levels, totallab))
}

#' @rdname tableby.stats
#' @export
countcellpct <- function(x, levels=NULL, by, by.levels=sort(unique(by)), na.rm=TRUE, weights = NULL, ..., totallab = "Total") {
  if(is.null(levels)) levels <- sort(unique(x))
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by)
    if(!is.null(weights)) idx <- idx & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  tot <- sum(vapply(levels, function(L) {
    sum(wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L]))
  }, numeric(1)))

  wtbls <- lapply(levels, function(L) {
    tmp <- wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L])
    wtbl <- c(tmp, stats::setNames(sum(tmp), totallab))
    lapply(wtbl, function(elt) as.countpct(c(elt, 100*elt/tot), parens = c("(", ")"), pct = "%", which.pct = 2L))
  })
  transpose_list(wtbls, levels, c(by.levels, totallab))
}

get_binom_est_ci <- function(x, tot, setNA, conf.level = 0.95) {
  if(setNA) return(NA_real_)

  b <- stats::binom.test(x, tot, conf.level = conf.level)
  unname(c(b$estimate, b$conf.int))
}

#' @rdname tableby.stats
#' @export
binomCI <- function(x, levels=NULL, na.rm=TRUE, weights = NULL, conf.level = 0.95, ...) {
  if(is.null(levels)) levels <- sort(unique(x))
  wtbl <- wtd.table(factor(x[!is.na(x)], levels=levels), weights=weights[!is.na(x)])

  ests <- lapply(wtbl, get_binom_est_ci, tot = sum(wtbl), setNA = !is.null(weights), conf.level = conf.level)
  as.tbstat_multirow(lapply(ests, as.tbstat, parens = c("(", ")"), sep2 = ", "))
}

#' @rdname tableby.stats
#' @export
rowbinomCI <- function(x, levels=NULL, by, by.levels=sort(unique(by)), na.rm=TRUE, weights = NULL, conf.level = 0.95, ..., totallab = "Total") {
  if(is.null(levels)) levels <- sort(unique(x))
  wts <- !is.null(weights)
  if(na.rm)
  {
    idx <- !is.na(x) & !is.na(by)
    if(wts) idx <- idx & !is.na(weights)
    x <- x[idx]
    by <- by[idx]
    weights <- weights[idx]
  }

  wtbls <- lapply(levels, function(L) {
    tmp <- wtd.table(factor(by[x == L], levels = by.levels), weights = weights[x == L])
    wtbl <- c(tmp, stats::setNames(sum(tmp), totallab))
    wtbl <- lapply(wtbl, get_binom_est_ci, tot = sum(tmp), setNA = wts, conf.level = conf.level)
    lapply(wtbl, as.tbstat, parens = c("(", ")"), sep2 = ", ")
  })
  # as.tbstat_multirow(lapply(wtbl, f))
  transpose_list(wtbls, levels, c(by.levels, totallab))
}

######## internal functions that we use above ########

wtd.table <- function(x, weights = NULL, na.rm = TRUE)
{
  if(!length(weights)) return(table(x))
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

  if(!length(weights)) return(stats::quantile(as.numeric(x), probs = probs, na.rm = na.rm))
  if(any(probs < 0) || any(probs > 1)) stop("Probabilities must be between 0 and 1 inclusive")

  wts <- wtd.table(x, weights, na.rm = na.rm)
  x <- if(is.Date(x)) as.numeric(as.Date(names(wts))) else as.numeric(names(wts))
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
  if(length(x) < 2) return(NA_real_)
  as.numeric(stats::cov.wt(matrix(x, ncol = 1), weights, method = method)$cov)
}
