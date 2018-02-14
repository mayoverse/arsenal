
capture.kable <- function(x)
{
  y <- capture.output(x)
  stopifnot(length(y) > 3)
  if(grepl("^Table:", y[2])) y <- c("", y)
  if(y[1] != "" || y[2] != "" || tail(y, 1) != "") stop("Leading and/or trailing elements aren't blank")
  head(tail(y, -2), -1)
}
