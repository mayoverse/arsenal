raw.print <- function(object, ...)
{
  cat("```\n")
  print(object, ...)
  cat("\n```\n\n")
}

catn <- function(...)
{
  cat(..., sep = "\n")
}

catnn <- function(...)
{
  cat(..., sep = "\n\n")
}
