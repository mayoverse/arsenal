# freqlist

Approximate the output from SAS's `PROC FREQ` procedure when using the
`/list` option of the `TABLE` statement.

## Usage

``` r
freqlist(object, ...)

# S3 method for class 'table'
freqlist(
  object,
  na.options = c("include", "showexclude", "remove"),
  strata = NULL,
  labelTranslations = NULL,
  control = NULL,
  ...
)

# S3 method for class 'formula'
freqlist(
  formula,
  data,
  subset,
  na.action,
  na.options = c("include", "showexclude", "remove"),
  strata = NULL,
  labelTranslations = NULL,
  control = NULL,
  addNA,
  exclude,
  drop.unused.levels,
  ...
)
```

## Arguments

- object:

  An R object, usually of class `"table"` or class `"xtabs"`

- ...:

  additional arguments. In the formula method, these are passed to the
  table method. These are also passed to
  [`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md)

- na.options:

  a character string indicating how to handling missing values:
  `"include"` (include values with NAs in counts and percentages),
  `"showexclude"` (show NAs but exclude from cumulative counts and all
  percentages), `"remove"` (remove values with NAs); default is
  `"include"`.

- strata:

  (formerly `groupBy`) an optional character string specifying a
  variable(s) to use for grouping when calculating cumulative counts and
  percentages.
  [`summary.freqlist`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md)
  will also separate by grouping variable for printing. Note that this
  is different from `modelsum` and `tableby`, which take bare column
  names (and only one, at that!)

- labelTranslations:

  an optional character string (or list) of labels to use for variable
  levels when summarizing. Names will be matched appropriately.

- control:

  control parameters to handle optional settings within `freqlist`. See
  [`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md)

- formula, data, subset, na.action, addNA, exclude, drop.unused.levels:

  Arguments passed to [`xtabs`](https://rdrr.io/r/stats/xtabs.html).

## Value

An object of class `c("freqlist", "arsenal_table")`

## See also

[`arsenal_table`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md),
[`summary.freqlist`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md),
[`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md),
[`freqlist.internal`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md),
[`table`](https://rdrr.io/r/base/table.html),
[`xtabs`](https://rdrr.io/r/stats/xtabs.html)

## Author

Tina Gunderson, with revisions by Ethan Heinzen

## Examples

``` r
# load mockstudy data
data(mockstudy)
tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA = "ifany")
noby <- freqlist(tab.ex, na.options = "include")
summary(noby)
#> 
#> 
#> |arm       |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:---------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |A: IFL    |Male   |0           |   29|              29|    1.93|               1.93|
#> |          |       |1           |  214|             243|   14.28|              16.21|
#> |          |       |NA          |   34|             277|    2.27|              18.48|
#> |          |Female |0           |   12|             289|    0.80|              19.28|
#> |          |       |1           |  118|             407|    7.87|              27.15|
#> |          |       |NA          |   21|             428|    1.40|              28.55|
#> |F: FOLFOX |Male   |0           |   31|             459|    2.07|              30.62|
#> |          |       |1           |  285|             744|   19.01|              49.63|
#> |          |       |NA          |   95|             839|    6.34|              55.97|
#> |          |Female |0           |   21|             860|    1.40|              57.37|
#> |          |       |1           |  198|            1058|   13.21|              70.58|
#> |          |       |NA          |   61|            1119|    4.07|              74.65|
#> |G: IROX   |Male   |0           |   17|            1136|    1.13|              75.78|
#> |          |       |1           |  187|            1323|   12.47|              88.26|
#> |          |       |NA          |   24|            1347|    1.60|              89.86|
#> |          |Female |0           |   14|            1361|    0.93|              90.79|
#> |          |       |1           |  121|            1482|    8.07|              98.87|
#> |          |       |NA          |   17|            1499|    1.13|             100.00|
#> 

# show the top 6 rows' frequencies and percents
head(summary(sort(noby, decreasing = TRUE)[c(1:4, 6)]))
#> 
#> 
#> |arm       |sex    |mdquality.s | Freq| Percent|
#> |:---------|:------|:-----------|----:|-------:|
#> |F: FOLFOX |Male   |1           |  285|   19.01|
#> |A: IFL    |Male   |1           |  214|   14.28|
#> |F: FOLFOX |Female |1           |  198|   13.21|
#> |G: IROX   |Male   |1           |  187|   12.47|
#> |          |Female |1           |  121|    8.07|
#> |A: IFL    |Female |1           |  118|    7.87|
#> 

withby <- freqlist(tab.ex, strata = c("arm","sex"), na.options = "showexclude")
summary(withby)
#> 
#> 
#> |arm    |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |A: IFL |Male |0           |   29|              29|   11.93|              11.93|
#> |       |     |1           |  214|             243|   88.07|             100.00|
#> |       |     |NA          |   34|              NA|      NA|                 NA|
#> 
#> 
#> |arm    |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |A: IFL |Female |0           |   12|              12|    9.23|               9.23|
#> |       |       |1           |  118|             130|   90.77|             100.00|
#> |       |       |NA          |   21|              NA|      NA|                 NA|
#> 
#> 
#> |arm       |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:---------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |F: FOLFOX |Male |0           |   31|              31|    9.81|               9.81|
#> |          |     |1           |  285|             316|   90.19|             100.00|
#> |          |     |NA          |   95|              NA|      NA|                 NA|
#> 
#> 
#> |arm       |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:---------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |F: FOLFOX |Female |0           |   21|              21|    9.59|               9.59|
#> |          |       |1           |  198|             219|   90.41|             100.00|
#> |          |       |NA          |   61|              NA|      NA|                 NA|
#> 
#> 
#> |arm     |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:-------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |G: IROX |Male |0           |   17|              17|    8.33|               8.33|
#> |        |     |1           |  187|             204|   91.67|             100.00|
#> |        |     |NA          |   24|              NA|      NA|                 NA|
#> 
#> 
#> |arm     |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:-------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |G: IROX |Female |0           |   14|              14|   10.37|              10.37|
#> |        |       |1           |  121|             135|   89.63|             100.00|
#> |        |       |NA          |   17|              NA|      NA|                 NA|
#> 
```
