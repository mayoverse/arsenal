### Test functions #######

notest <- function(x, x.by, ...)
{
  list(p.value = NA_real_, method = "No test")
}

## continuous tests:
## 1. anova  (parametric)
## consider allowing glm, for now just lm with gaussian errors
## Would like to just use either "anova" or "aov", anova needs lm(group~x first,
## aov does not return p-value. Could add it after.
## For now, just write our own to avoid over-writing anova R-base function
## also, nice to keep same format to call, eval(call(function, x, x,by)), as other tests
anova <- function(x, x.by, ..., test.always = FALSE) {
  tab <- table(is.na(x), x.by)
  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, statistic.F=NA_real_, method="Linear Model ANOVA"))
  }
  aov.out <- stats::lm(x~x.by)
  test <- stats::anova(aov.out)
  list(p.value = test[1,ncol(test)],
       statistic.F = test[1,ncol(test)-1],
       method = "Linear Model ANOVA")
}
## 2. kruskal-wallis (non-parametric)
kwt <- function(x, x.by, ..., test.always = FALSE) {
  tab <- table(is.na(x), x.by)
  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, statistic.F=NA_real_, method="Kruskal-Wallis rank sum test"))
  }
  stats::kruskal.test(x, as.factor(x.by))
}

## 2. wilcoxon (non-parametric)
wt <- function(x, x.by, ..., wilcox.correct = FALSE, wilcox.exact = NULL, test.always = FALSE) {
  tab <- table(is.na(x), x.by)
  if(ncol(tab) != 2) stop("The Wilcoxon Rank Sum test must have exactly two groups")

  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, statistic.F=NA_real_, method="Wilcoxon rank sum test"))
  }
  stats::wilcox.test(x ~ as.factor(x.by), correct = wilcox.correct, exact = wilcox.exact)
}

## median test
medtest <- function(x, x.by, ..., test.always = FALSE) {
  if(!requireNamespace("coin", quietly = TRUE))
  {
    warning("The \"coin\" package is required to run a median test.", call. = FALSE)
    return(notest(x, x.by, ...))
  }

  tab <- table(is.na(x), x.by)
  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, method = "Median test"))
  }
  ## should be taken care of with coin::
  check_pkg("coin")
  mtest <- coin::median_test(x~as.factor(x.by), teststat="quad")
  list(p.value=coin::pvalue(mtest), method="Median test", statistic=mtest@statistic@teststatistic)
}

## two tests for categorical,
## 1. chisq goodness of fit, equal proportions across table cells
chisq <- function(x, x.by, ..., chisq.correct=FALSE, simulate.p.value=FALSE, B=2000, test.always = FALSE) {
  tab <- table(x, x.by, exclude=NA)
  rs <- rowSums(tab)
  cs <- colSums(tab)

  if(!test.always && (any(rs == 0) || any(cs == 0)) && ncol(tab) > 1 && nrow(tab) > 1) {
    return(list(p.value=NA_real_, method="Pearson's Chi-squared test"))
  }
  if(length(cs) > 1) tab <- tab[rs > 0, , drop = FALSE]
  if(length(rs) > 1) tab <- tab[, cs > 0, drop = FALSE]
  suppressWarnings(stats::chisq.test(tab, correct=chisq.correct, simulate.p.value=simulate.p.value, B=B))
}

## 2. Fisher's exact test for prob of as or more extreme table
fe <- function(x, x.by, ..., simulate.p.value=FALSE, B=2000, test.always = FALSE) {
  tab <- table(x, x.by, exclude=NA)
  rs <- rowSums(tab)
  cs <- colSums(tab)

  if((!test.always && (any(rs == 0) || any(cs == 0))) || ncol(tab) == 1 || nrow(tab) == 1) {
    return(list(p.value=NA_real_, method = "Fisher's Exact Test for Count Data"))
  }
  # this already subsets out rows and cols with all 0's
  stats::fisher.test(tab, simulate.p.value=simulate.p.value, B=B)
}

## trend test for ordinal data
trend <- function(x, x.by, ..., test.always = FALSE) {
  if(!requireNamespace("coin", quietly = TRUE))
  {
    warning("The \"coin\" package is required to run a trend test.", call. = FALSE)
    return(notest(x, x.by, ...))
  }

  tab <- table(x, x.by, exclude=NA)
  rs <- rowSums(tab)
  cs <- colSums(tab)

  if(!test.always && (any(rs == 0) || any(cs == 0))) {
    return(list(p.value=NA_real_, method = "Trend test for ordinal variables"))
  }
  ## should be taken care of with coin::
  check_pkg("coin")
  indtest <- coin::independence_test(x~as.factor(x.by), teststat="quad")
  list(p.value=coin::pvalue(indtest), method="Trend test for ordinal variables", statistic=indtest@statistic@teststatistic)
}

## ' logrank
## '
## ' survdiff logrank test
## ' @param x  surv variable
## ' @param x.by  by, categorical variable
## ' @return   test output with $method and $p.value
logrank <- function(x, x.by, ..., test.always = FALSE) {
  tab <- table(is.na(x), x.by, exclude=NA)
  if(!test.always && (any(tab[1, ] == 0) || any(colSums(tab) == 0))) {
    return(list(p.value=NA_real_, method="survdiff logrank"))
  }
  out <- survival::survdiff(x ~ x.by)
  out$p.value <- 1-stats::pchisq(out$chisq, df=sum(tab[1,] != 0)-1)
  out$method <- "survdiff logrank"
  out
}
