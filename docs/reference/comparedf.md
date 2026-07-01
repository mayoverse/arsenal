# Compare two data.frames and report differences

Compare two data.frames and report any differences between them, much
like SAS's `PROC COMPARE` procedure.

## Usage

``` r
comparedf(x, y, by = NULL, by.x = by, by.y = by, control = NULL, ...)

# S3 method for class 'comparedf'
print(x, ...)
```

## Arguments

- x, y:

  A data.frame to compare

- by, by.x, by.y:

  Which variables are IDs to merge the two data.frames? If set to
  `"row.names"`, merging will occur over the row.names. If set to `NULL`
  (default), merging will occur row-by-row.

- control:

  A list of control parameters from
  [`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md).

- ...:

  Other arguments, passed to
  [`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)
  when appropriate.

## See also

[`summary.comparedf`](https://mayoverse.github.io/arsenal/reference/summary.comparedf.md),
[`comparedf.control`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md),
[`diffs`](https://mayoverse.github.io/arsenal/reference/diffs.md),
[`n.diffs`](https://mayoverse.github.io/arsenal/reference/diffs.md),
[`n.diff.obs`](https://mayoverse.github.io/arsenal/reference/diffs.md)

## Author

Ethan Heinzen, adapted from code from Andrew Hanson

## Examples

``` r

df1 <- data.frame(id = paste0("person", 1:3), a = c("a", "b", "c"),
                  b = c(1, 3, 4), c = c("f", "e", "d"),
                  row.names = paste0("rn", 1:3), stringsAsFactors = FALSE)
df2 <- data.frame(id = paste0("person", 3:1), a = c("c", "b", "a"),
                  b = c(1, 3, 4), d = paste0("rn", 1:3),
                  row.names = paste0("rn", c(1,3,2)), stringsAsFactors = FALSE)
summary(comparedf(df1, df2))
#> 
#> 
#> Table: Summary of data.frames
#> 
#> version   arg    ncol   nrow
#> --------  ----  -----  -----
#> x         df1       4      3
#> y         df2       4      3
#> 
#> 
#> 
#> Table: Summary of overall comparison
#> 
#> statistic                                                      value
#> ------------------------------------------------------------  ------
#> Number of by-variables                                             0
#> Number of non-by variables in common                               3
#> Number of variables compared                                       3
#> Number of variables in x but not y                                 1
#> Number of variables in y but not x                                 1
#> Number of variables compared with some values unequal              2
#> Number of variables compared with all values equal                 1
#> Number of observations in common                                   3
#> Number of observations in x but not y                              0
#> Number of observations in y but not x                              0
#> Number of observations with some compared variables unequal        2
#> Number of observations with all compared variables equal           1
#> Number of values unequal                                           4
#> 
#> 
#> 
#> Table: Variables not shared
#> 
#> version   variable    position  class     
#> --------  ---------  ---------  ----------
#> x         c                  4  character 
#> y         d                  4  character 
#> 
#> 
#> 
#> Table: Other variables not compared
#> 
#>                                  
#>  --------------------------------
#>  No other variables not compared 
#>  --------------------------------
#> 
#> 
#> 
#> Table: Observations not shared
#> 
#>                             
#>  ---------------------------
#>  No observations not shared 
#>  ---------------------------
#> 
#> 
#> 
#> Table: Differences detected by variable
#> 
#> var.x   var.y     n   NAs
#> ------  ------  ---  ----
#> id      id        2     0
#> a       a         2     0
#> b       b         0     0
#> 
#> 
#> 
#> Table: Differences detected
#> 
#> var.x   var.y    ..row.names..  values.x   values.y    row.x   row.y
#> ------  ------  --------------  ---------  ---------  ------  ------
#> id      id                   1  person1    person3         1       1
#> id      id                   3  person3    person1         3       3
#> a       a                    1  a          c               1       1
#> a       a                    3  c          a               3       3
#> 
#> 
#> 
#> Table: Non-identical attributes
#> 
#>                              
#>  ----------------------------
#>  No non-identical attributes 
#>  ----------------------------
#> 
summary(comparedf(df1, df2, by = "id"))
#> 
#> 
#> Table: Summary of data.frames
#> 
#> version   arg    ncol   nrow
#> --------  ----  -----  -----
#> x         df1       4      3
#> y         df2       4      3
#> 
#> 
#> 
#> Table: Summary of overall comparison
#> 
#> statistic                                                      value
#> ------------------------------------------------------------  ------
#> Number of by-variables                                             1
#> Number of non-by variables in common                               2
#> Number of variables compared                                       2
#> Number of variables in x but not y                                 1
#> Number of variables in y but not x                                 1
#> Number of variables compared with some values unequal              1
#> Number of variables compared with all values equal                 1
#> Number of observations in common                                   3
#> Number of observations in x but not y                              0
#> Number of observations in y but not x                              0
#> Number of observations with some compared variables unequal        2
#> Number of observations with all compared variables equal           1
#> Number of values unequal                                           2
#> 
#> 
#> 
#> Table: Variables not shared
#> 
#> version   variable    position  class     
#> --------  ---------  ---------  ----------
#> x         c                  4  character 
#> y         d                  4  character 
#> 
#> 
#> 
#> Table: Other variables not compared
#> 
#>                                  
#>  --------------------------------
#>  No other variables not compared 
#>  --------------------------------
#> 
#> 
#> 
#> Table: Observations not shared
#> 
#>                             
#>  ---------------------------
#>  No observations not shared 
#>  ---------------------------
#> 
#> 
#> 
#> Table: Differences detected by variable
#> 
#> var.x   var.y     n   NAs
#> ------  ------  ---  ----
#> a       a         0     0
#> b       b         2     0
#> 
#> 
#> 
#> Table: Differences detected
#> 
#> var.x   var.y   id        values.x   values.y    row.x   row.y
#> ------  ------  --------  ---------  ---------  ------  ------
#> b       b       person1   1          4               1       3
#> b       b       person3   4          1               3       1
#> 
#> 
#> 
#> Table: Non-identical attributes
#> 
#>                              
#>  ----------------------------
#>  No non-identical attributes 
#>  ----------------------------
#> 
summary(comparedf(df1, df2, by = "row.names"))
#> 
#> 
#> Table: Summary of data.frames
#> 
#> version   arg    ncol   nrow
#> --------  ----  -----  -----
#> x         df1       4      3
#> y         df2       4      3
#> 
#> 
#> 
#> Table: Summary of overall comparison
#> 
#> statistic                                                      value
#> ------------------------------------------------------------  ------
#> Number of by-variables                                             1
#> Number of non-by variables in common                               3
#> Number of variables compared                                       3
#> Number of variables in x but not y                                 1
#> Number of variables in y but not x                                 1
#> Number of variables compared with some values unequal              3
#> Number of variables compared with all values equal                 0
#> Number of observations in common                                   3
#> Number of observations in x but not y                              0
#> Number of observations in y but not x                              0
#> Number of observations with some compared variables unequal        3
#> Number of observations with all compared variables equal           0
#> Number of values unequal                                           8
#> 
#> 
#> 
#> Table: Variables not shared
#> 
#> version   variable    position  class     
#> --------  ---------  ---------  ----------
#> x         c                  4  character 
#> y         d                  4  character 
#> 
#> 
#> 
#> Table: Other variables not compared
#> 
#>                                  
#>  --------------------------------
#>  No other variables not compared 
#>  --------------------------------
#> 
#> 
#> 
#> Table: Observations not shared
#> 
#>                             
#>  ---------------------------
#>  No observations not shared 
#>  ---------------------------
#> 
#> 
#> 
#> Table: Differences detected by variable
#> 
#> var.x   var.y     n   NAs
#> ------  ------  ---  ----
#> id      id        3     0
#> a       a         3     0
#> b       b         2     0
#> 
#> 
#> 
#> Table: Differences detected
#> 
#> var.x   var.y   ..row.names..   values.x   values.y    row.x   row.y
#> ------  ------  --------------  ---------  ---------  ------  ------
#> id      id      rn1             person1    person3         1       1
#> id      id      rn2             person2    person1         2       3
#> id      id      rn3             person3    person2         3       2
#> a       a       rn1             a          c               1       1
#> a       a       rn2             b          a               2       3
#> a       a       rn3             c          b               3       2
#> b       b       rn2             3          4               2       3
#> b       b       rn3             4          3               3       2
#> 
#> 
#> 
#> Table: Non-identical attributes
#> 
#>                              
#>  ----------------------------
#>  No non-identical attributes 
#>  ----------------------------
#> 
```
