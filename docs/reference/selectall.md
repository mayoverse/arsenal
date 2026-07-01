# Make a column for "select all" input

Make a column for "select all" input

## Usage

``` r
selectall(...)

as.selectall(x)

# S3 method for class 'selectall'
as.matrix(x, ...)

# S3 method for class 'selectall'
x[i, j, drop = FALSE]

# S3 method for class 'selectall'
is.na(x)

is.selectall(x)
```

## Arguments

- ...:

  Named arguments of the same length. These should be logical, numeric
  (0/1) or a factor with two levels.

- x:

  An object of class "selectall"

- i, j, drop:

  Arguments to \`\[.matrix\`

## See also

[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md)

## Examples

``` r
d <- data.frame(grp = rep(c("A", "B"), each = 5))
d$s <- selectall(
  `Option 1` = c(rep(1, 4), rep(0, 6)),
  `Option 2` = c(0, 1, 0, 0, 0, 1, 1, 1, 0, 0),
  `Option 3` = 1,
  `Option 4` = 0
)
summary(tableby(grp ~ s, data = d), text = TRUE)
#> 
#> 
#> |            |  A (N=5)   |  B (N=5)   | Total (N=10) | p value|
#> |:-----------|:----------:|:----------:|:------------:|-------:|
#> |s           |            |            |              |        |
#> |-  Option 1 | 4 (80.0%)  |  0 (0.0%)  |  4 (40.0%)   |        |
#> |-  Option 2 | 1 (20.0%)  | 3 (60.0%)  |  4 (40.0%)   |        |
#> |-  Option 3 | 5 (100.0%) | 5 (100.0%) | 10 (100.0%)  |        |
#> |-  Option 4 |  0 (0.0%)  |  0 (0.0%)  |   0 (0.0%)   |        |
#> 
```
