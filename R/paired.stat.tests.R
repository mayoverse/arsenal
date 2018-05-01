
paired.t <- function(x, y, ...) {
  if(is.Date(x) && is.Date(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  stats::t.test(x, y, paired = TRUE)
}

mcnemar <- function(x, y, mcnemar.correct = TRUE, ...)
{
  stats::mcnemar.test(x, y, correct = mcnemar.correct)
}

signed.rank <- function(x, y, signed.rank.exact = NULL, signed.rank.correct = TRUE, ...)
{
  if(is.ordered(x) && is.ordered(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  stats::wilcox.test(x, y, paired = TRUE, exact = signed.rank.exact, correct = signed.rank.correct)
}

sign.test <- function(x, y, ...)
{
  stats::binom.test(c(sum(x > y), sum(x < y)))
}
