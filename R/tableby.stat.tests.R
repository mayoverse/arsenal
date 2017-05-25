### Test functions #######

## continuous tests:
## 1. anova  (parametric)
## consider allowing glm, for now just lm with gaussian errors
## Would like to just use either "anova" or "aov", anova needs lm(group~x first,
## aov does not return p-value. Could add it after.
## For now, just write our own to avoid over-writing anova R-base function
## also, nice to keep same format to call, eval(call(function, x, x,by)), as other tests
anova <- function(x, x.by) {
  if(any(colSums(table(x, x.by, exclude=NA))==0)) {
    return(list(p.value=NA, statistic.F=NA, method="Linear Model ANOVA"))
  }
  aov.out <- stats::lm(x~x.by)
  test <- stats::anova(aov.out)
  test.out <- list(p.value = test[1,ncol(test)],
                   statistic.F = test[1,ncol(test)-1],
                   method = "Linear Model ANOVA")
}
## 2. kruskal-wallis (non-parametric)
kwt <- function(x, x.by) {
  #  na.ind <- is.na(x)
  # stats::kruskal.test(x[!na.ind], as.factor(x.by[!na.ind]))
  if(any(colSums(table(x, x.by, exclude=NA))==0)) {
    return(list(p.value=NA, statistic.F=NA, method="Kruskal-Wallis rank sum test"))
  }
  stats::kruskal.test(x, as.factor(x.by))
}

## two tests for categorical,
## 1. chisq goodness of fit, equal proportions across table cells
chisq <- function(x, x.by) {
  tab <- table(x, x.by, exclude=NA)
  ctl <- dynGet("control") # envir=parent.frame)
  if(sum(rowSums(tab)>0)>1) {
    stats::chisq.test(tab[rowSums(tab)>0,], correct=ctl$chisq.correct, simulate.p.value=ctl$simulate.p.value, B=ctl$B)
  } else {
    list(statistic=0, p.value=1, method="Pearson's Chi-squared test")
  }
}
## 2. Fisher's exact test for prob of as or more extreme table
fe <- function(x, x.by) {
  tab <- table(x,x.by, exclude=NA)
  ctl <- dynGet("control")
  stats::fisher.test(tab, simulate.p.value=ctl$simulate.p.value, B=ctl$B)
}

## trend test for ordinal data
trend <- function(x, x.by) {
  ## should be taken care of with coin::
  ## require(coin, quietly=TRUE, warn.conflicts=FALSE)
  indtest <- coin::independence_test(x~as.factor(x.by), teststat="quad")
  test <- list(p.value=coin::pvalue(indtest), method="Trend test for ordinal variables", statistic=indtest@statistic@teststatistic)
}

## ' logrank
## '
## ' survdiff logrank test
## ' @param x  surv variable
## ' @param x.by  by, categorical variable
## ' @return   test output with $method and $p.value
logrank <- function(x, x.by) {
  out <- survival::survdiff(x ~ x.by)
  out$p.value <- 1-stats::pchisq(out$chisq, df=length(unique(x.by))-1)
  out$method="survdiff logrank"
  out
}
