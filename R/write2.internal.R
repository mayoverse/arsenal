threeticks <- function(object, ...)
{
  cat("```\n")
  print(object, ...)
  cat("\n```\n\n")
}

as.threeticks <- function(object)
{
  class(object) <- c("threeticks", class(object))
  object
}