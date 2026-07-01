# Control settings for `comparedf` function

Control tolerance definitions for the
[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
function.

## Usage

``` r
comparedf.control(
  tol.logical = "none",
  tol.num = c("absolute", "percent", "pct"),
  tol.num.val = sqrt(.Machine$double.eps),
  int.as.num = FALSE,
  tol.char = c("none", "trim", "case", "both"),
  tol.factor = c("none", "levels", "labels"),
  factor.as.char = FALSE,
  tol.date = "absolute",
  tol.date.val = 0,
  tol.other = "none",
  tol.vars = "none",
  max.print.vars = NA,
  max.print.obs = NA,
  max.print.diffs.per.var = 10,
  max.print.diffs = 50,
  max.print.attrs = NA,
  ...,
  max.print.diff = 10
)
```

## Arguments

- tol.logical, tol.num, tol.char, tol.factor, tol.date, tol.other:

  A function or one of the shortcut character strings or a list thereof,
  denoting the tolerance function to use for a given data type. See
  "details", below.

- tol.num.val:

  Numeric; maximum value of differences allowed in numerics (fed to the
  function given in `tol.num`).

- int.as.num:

  Logical; should integers be coerced to numeric before comparison?
  Default FALSE.

- factor.as.char:

  Logical; should factors be coerced to character before comparison?
  Default FALSE.

- tol.date.val:

  Numeric; maximum value of differences allowed in dates (fed to the
  function given in `tol.date`).

- tol.vars:

  Either `"none"` (the default), denoting that variable names are to be
  matched as-is, a named vector manually specifying variable names to
  compare (where the names correspond to columns of `x` and the values
  correspond to columns of `y`), or a character vector denoting
  equivalence classes for characters in the variable names. See
  "details", below.

- max.print.vars:

  Integer denoting maximum number of variables to report in the
  "variables not shared" and "variables not compared" output. `NA` will
  print all differences.

- max.print.obs:

  Integer denoting maximum number of not-shared observations to report.
  `NA` will print all differences.

- max.print.diffs.per.var, max.print.diffs:

  Integers denoting the maximum number of differences to report for each
  variable or overall. `NA` will print all differences for each variable
  or overall.

- max.print.attrs:

  Integers denoting the maximum number of non-identical attributes to
  report.`NA` will print all differences.

- ...:

  Other arguments (not in use at this time).

- max.print.diff:

  Deprecated.

## Value

A list containing the necessary parameters for the
[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
function.

## Details

The following character strings are accepted:

- `tol.logical = "none"`: compare logicals exactly as they are.

- `tol.num = "absolute"`: compare absolute differences in numerics.

- `tol.num = "percent"`, `tol.num = "pct"` compare percent differences
  in numerics.

- `tol.char = "none"`: compare character strings exactly as they are.

- `tol.char = "trim"`: left-justify and trim all trailing white space.

- `tol.char = "case"`: allow differences in upper/lower case.

- `tol.char = "both"`: combine `"trim"` and `"case"`.

- `tol.factor = "none"`: match both character labels and numeric levels.

- `tol.factor = "levels"`: match only the numeric levels.

- `tol.factor = "labels"`: match only the labels.

- `tol.date = "absolute"`: compare absolute differences in dates.

- `tol.other = "none"`: expect objects of other classes to be exactly
  identical.

A list with names mapped to `x` can be used to specify tolerances by
variable. One unnamed element is supported as the default.

`tol.vars`: If not set to `"none"` (the default) or a named vector, the
`tol.vars` argument is a character vector denoting equivalence classes
for the characters in the variable names. A single character in this
vector means to replace that character with `""`. All other strings in
this vector are split by character and replaced by the first character
in the string.

E.g., a character vector `c("._", "aA", " ")` would denote that the dot
and underscore are equivalent (to be translated to a dot), that "a" and
"A" are equivalent (to be translated to "a"), and that spaces should be
removed.

The special character string `"case"` in this vector is the same as
specifying `paste0(letters, LETTERS)`.

## See also

[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md),
[`comparedf.tolerances`](https://mayoverse.github.io/arsenal/reference/comparedf.tolerances.md),
[`summary.comparedf`](https://mayoverse.github.io/arsenal/reference/summary.comparedf.md)

## Author

Ethan Heinzen

## Examples

``` r
cntl <- comparedf.control(
  tol.num = "pct",     # calculate percent differences
  tol.vars = c("case", # ignore case
               "._",   # set all underscores to dots.
               "e")    # remove all letter e's
)

cntl <- comparedf.control(tol.char = list(
  "none",      # the default
  x1 = "case", # be case-insensitive for the variable "x1"
  x2 = function(x, y) tol.NA(x, y, x != y | y == "NA") # a custom-defined tolerance
))
```
