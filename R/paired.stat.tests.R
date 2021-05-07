
paired.t <- function(x, y, ..., na.rm = TRUE) {
  if(is.Date(x) && is.Date(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  if(na.rm) {
    idx <- is.na(x) | is.na(y)
    x <- x[!idx]
    y <- y[!idx]
  }
  stats::t.test(x, y, paired = TRUE)
}

mcnemar <- function(x, y, mcnemar.correct = TRUE, ..., na.rm = TRUE)
{
  if(na.rm) {
    idx <- is.na(x) | is.na(y)
    x <- x[!idx]
    y <- y[!idx]
  }
  stats::mcnemar.test(x, y, correct = mcnemar.correct)
}

signed.rank <- function(x, y, signed.rank.exact = NULL, signed.rank.correct = TRUE, ..., na.rm = TRUE)
{
  if(is.ordered(x) && is.ordered(y))
  {
    x <- as.integer(x)
    y <- as.integer(y)
  }
  if(na.rm) {
    idx <- is.na(x) | is.na(y)
    x <- x[!idx]
    y <- y[!idx]
  }
  stats::wilcox.test(x, y, paired = TRUE, exact = signed.rank.exact, correct = signed.rank.correct)
}

sign.test <- function(x, y, ..., na.rm = TRUE)
{
  if(na.rm) {
    idx <- is.na(x) | is.na(y)
    x <- x[!idx]
    y <- y[!idx]
  }
  stats::binom.test(c(sum(x > y), sum(x < y)))
}
