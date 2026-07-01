# Extract differences

Extract differences (`diffs()`), number of differences (`n.diffs()`), or
number of not-shared observations (`n.diff.obs()`) from a `comparedf`
object.

## Usage

``` r
n.diff.obs(object, ...)

# S3 method for class 'comparedf'
n.diff.obs(object, ...)

# S3 method for class 'summary.comparedf'
n.diff.obs(object, ...)

n.diffs(object, ...)

# S3 method for class 'comparedf'
n.diffs(object, ...)

# S3 method for class 'summary.comparedf'
n.diffs(object, ...)

diffs(object, ...)

# S3 method for class 'comparedf'
diffs(
  object,
  what = c("differences", "observations"),
  vars = NULL,
  ...,
  by.var = FALSE
)

# S3 method for class 'summary.comparedf'
diffs(
  object,
  what = c("differences", "observations"),
  vars = NULL,
  ...,
  by.var = FALSE
)
```

## Arguments

- object:

  An object of class `comparedf` or `summary.comparedf`.

- ...:

  Other arguments (not in use at this time).

- what:

  Should differences or the not-shared observations be returned?

- vars:

  A character vector of variable names to subset the results to.

- by.var:

  Logical: should the number of differences by variable be reported, or
  should all differences be reported (the default).

## See also

[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
[`summary.comparedf`](https://mayoverse.github.io/arsenal/reference/summary.comparedf.md)

## Author

Ethan Heinzen
