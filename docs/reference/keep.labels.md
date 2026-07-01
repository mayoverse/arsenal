# Keep Labels

Keep the `'label'` attribute on an R object when subsetting.
`loosen.labels` allows the `'label'` attribute to be lost again.

## Usage

``` r
keep.labels(x, ...)

# S3 method for class 'data.frame'
keep.labels(x, ...)

# Default S3 method
keep.labels(x, ...)

# S3 method for class 'keep_labels'
x[...]

# S3 method for class 'keep_labels'
x[i] <- value

loosen.labels(x, ...)

# S3 method for class 'data.frame'
loosen.labels(x, ...)

# Default S3 method
loosen.labels(x, ...)
```

## Arguments

- x:

  An R object

- ...:

  Other arguments (not in use at this time).

- i, value:

  See `[<-`.

## Value

A copy of `x` with a "keep_labels" class appended on or removed. Note
that for the `data.frame` method, only classes on the columns are
changed; the `data.frame` won't have an extra class appended. This is
different from previous versions of `arsenal`.

## See also

[`labels`](https://mayoverse.github.io/arsenal/reference/labels.md)

## Author

Ethan Heinzen
