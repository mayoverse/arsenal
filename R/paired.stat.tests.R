
paired.t <- function(x, y, ...) {
  if(is.Date(x) && is.Date(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  t.test(x, y, paired = TRUE)
}

mcnemar <- function(x, y, mcnemar.correct = TRUE, ...)
{
  mcnemar.test(x, y, correct = mcnemar.correct)
}

signed.rank <- function(x, y, signed.rank.exact = NULL, signed.rank.correct = TRUE, ...)
{
  if(is.ordered(x) && is.ordered(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  wilcox.test(x, y, paired = TRUE, exact = signed.rank.exact, correct = signed.rank.correct)
}

sign.test <- function(x, y, ...)
{
  binom.test(c(sum(x > y), sum(x < y)))
}
