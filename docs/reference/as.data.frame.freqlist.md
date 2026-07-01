# as.data.frame.freqlist

Convert
[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
object to a data.frame.

## Usage

``` r
# S3 method for class 'freqlist'
as.data.frame(x, ..., labelTranslations = NULL, list.ok = FALSE)
```

## Arguments

- x:

  An object of class `"freqlist"`.

- ...:

  Arguments to pass to
  [`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md)

- labelTranslations:

  A named list (or vector) where the name is the label in the output to
  be replaced in the pretty rendering by the character string value for
  the named element of the list, e.g.,
  `list(age = "Age(Years)", meansd = "Mean(SD)")`.

- list.ok:

  If the object has multiple by-variables, is it okay to return a list
  of data.frames instead of a single data.frame? If `FALSE` but there
  are multiple by-variables, a warning is issued.

## Value

A data.frame corresponding to the `freqlist` object.
