# Internal Functions

Internal Functions

## Usage

``` r
smart.split(string, width = Inf, min.split = -Inf)

replace2(x, list, values)
```

## Arguments

- string:

  A character vector

- width:

  Either `Inf` or `NULL` to specify no splitting, or a positive integer
  giving the largest allowed string length.

- min.split:

  Either `-Inf` or `NULL` to specify no lower bound on the string
  length, or a positive integer giving the minimum string length.

- x:

  a vector.

- list:

  an index vector.

- values:

  replacement values.

## Value

For `smart.split`, a list of the same length as `string`, with each
element being the "intelligently" split string.

For `replace2`, a vector with the proper values replaced.

## See also

[`replace`](https://rdrr.io/r/base/replace.html)
