# Labels

Assign and extract the `'label'` attribute on an R object. `set_labels`
is the same as `labels(x) <- value` but returns `x` for use in a pipe
chain. `set_attr` is the same as `attr(x, which) <- value` but returns
`x` for use in a pipe chain.

## Usage

``` r
# S3 method for class 'data.frame'
labels(object, ...)

# S3 method for class 'keep_labels'
labels(object, ...)

labels(x) <- value

# S3 method for class 'keep_labels'
labels(x) <- value

# Default S3 method
labels(x) <- value

# S3 method for class 'data.frame'
labels(x) <- value

set_labels(x, value)

set_attr(x, which, value)
```

## Arguments

- ...:

  Other arguments (not in use at this time).

- x, object:

  An R object.

- value:

  A vector or list containing labels to assign. Labels are assigned
  based on names, if available; otherwise, they're assigned in order.
  Can pass `NULL` to remove all labels.

- which:

  See `attr<-`

## Value

The labels of `object`, or `object` with new labels.

## Details

The [`data.frame`](https://rdrr.io/r/base/data.frame.html) methods put
labels on and extract labels from the *columns* of `object`.

## See also

[`keep.labels`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)

## Author

Ethan Heinzen
