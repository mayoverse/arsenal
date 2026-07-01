# summary.freqlist

Summarize the `freqlist` object.

## Usage

``` r
# S3 method for class 'freqlist'
summary(object, ..., labelTranslations = NULL, title = NULL)

# S3 method for class 'summary.freqlist'
as.data.frame(x, ..., list.ok = FALSE)
```

## Arguments

- object:

  an object of class
  [`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md)

- ...:

  For `summary.freqlist`, these are passed to
  [`as.data.frame.freqlist`](https://mayoverse.github.io/arsenal/reference/as.data.frame.freqlist.md)
  (and hence to
  [`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md)).
  For the print method, these are additional arguments passed to the
  [`kable`](https://rdrr.io/pkg/knitr/man/kable.html) function.

- labelTranslations:

  A named list (or vector) where the name is the label in the output to
  be replaced in the pretty rendering by the character string value for
  the named element of the list, e.g.,
  `list(age = "Age(Years)", meansd = "Mean(SD)")`.

- title:

  Title/caption for the table, defaulting to `NULL` (no title). Passed
  to [`kable`](https://rdrr.io/pkg/knitr/man/kable.html). Can be length
  \> 1 if the more than one table is being printed.

- x:

  An object of class `summary.freqlist`.

- list.ok:

  If the object has multiple by-variables, is it okay to return a list
  of data.frames instead of a single data.frame? If `FALSE` but there
  are multiple by-variables, a warning is issued.

## Value

An object of class `"summary.freqlist"` (invisibly for the print
method).

## See also

[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
[`table`](https://rdrr.io/r/base/table.html),
[`xtabs`](https://rdrr.io/r/stats/xtabs.html),
[`kable`](https://rdrr.io/pkg/knitr/man/kable.html),
[`freq.control`](https://mayoverse.github.io/arsenal/reference/freq.control.md),
[`freqlist.internal`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md)

## Author

Tina Gunderson, with major revisions by Ethan Heinzen

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
summary(withby, dupLabels = TRUE)
#> 
#> 
#> |arm    |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |A: IFL |Male |0           |   29|              29|   11.93|              11.93|
#> |A: IFL |Male |1           |  214|             243|   88.07|             100.00|
#> |A: IFL |Male |NA          |   34|              NA|      NA|                 NA|
#> 
#> 
#> |arm    |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |A: IFL |Female |0           |   12|              12|    9.23|               9.23|
#> |A: IFL |Female |1           |  118|             130|   90.77|             100.00|
#> |A: IFL |Female |NA          |   21|              NA|      NA|                 NA|
#> 
#> 
#> |arm       |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:---------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |F: FOLFOX |Male |0           |   31|              31|    9.81|               9.81|
#> |F: FOLFOX |Male |1           |  285|             316|   90.19|             100.00|
#> |F: FOLFOX |Male |NA          |   95|              NA|      NA|                 NA|
#> 
#> 
#> |arm       |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:---------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |F: FOLFOX |Female |0           |   21|              21|    9.59|               9.59|
#> |F: FOLFOX |Female |1           |  198|             219|   90.41|             100.00|
#> |F: FOLFOX |Female |NA          |   61|              NA|      NA|                 NA|
#> 
#> 
#> |arm     |sex  |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:-------|:----|:-----------|----:|---------------:|-------:|------------------:|
#> |G: IROX |Male |0           |   17|              17|    8.33|               8.33|
#> |G: IROX |Male |1           |  187|             204|   91.67|             100.00|
#> |G: IROX |Male |NA          |   24|              NA|      NA|                 NA|
#> 
#> 
#> |arm     |sex    |mdquality.s | Freq| Cumulative Freq| Percent| Cumulative Percent|
#> |:-------|:------|:-----------|----:|---------------:|-------:|------------------:|
#> |G: IROX |Female |0           |   14|              14|   10.37|              10.37|
#> |G: IROX |Female |1           |  121|             135|   89.63|             100.00|
#> |G: IROX |Female |NA          |   17|              NA|      NA|                 NA|
#> 
```
