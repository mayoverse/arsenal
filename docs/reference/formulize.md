# formulize

A shortcut to generate one-, two-, or many-sided formulas from vectors
of variable names.

## Usage

``` r
formulize(
  y = "",
  x,
  ...,
  data = NULL,
  collapse = "+",
  collapse.y = collapse,
  escape = FALSE
)
```

## Arguments

- y, x, ...:

  Character vectors, names, or calls to be collapsed (by `"+"`) and put
  left-to-right in the formula. If `data` is supplied, these can also be
  numeric, denoting which column name to use. See examples.

- data:

  An R object with non-null column names.

- collapse:

  How should terms be collapsed? Default is addition.

- collapse.y:

  How should the y-terms be collapsed? Default is addition. Also accepts
  the special string "list", which combines them into a
  multiple-left-hand-side formula, for use in other functions.

- escape:

  A logical indicating whether character vectors should be coerced to
  names (that is, whether names with spaces should be surrounded with
  backticks or not)

## See also

[`reformulate`](https://rdrr.io/r/stats/delete.response.html)

## Author

Ethan Heinzen

## Examples

``` r
## two-sided formula
f1 <- formulize("y", c("x1", "x2", "x3"))

## one-sided formula
f2 <- formulize(x = c("x1", "x2", "x3"))

## multi-sided formula
f3 <- formulize("y", c("x1", "x2", "x3"), c("z1", "z2"), "w1")

## can use numerics for column names
data(mockstudy)
f4 <- formulize(y = 1, x = 2:4, data = mockstudy)

## mix and match
f5 <- formulize(1, c("x1", "x2", "x3"), data = mockstudy)

## get an interaction
f6 <- formulize("y", c("x1*x2", "x3"))

## get only interactions
f7 <- formulize("y", c("x1", "x2", "x3"), collapse = "*")

## no intercept
f8 <- formulize("y", "x1 - 1")
f9 <- formulize("y", c("x1", "x2", "-1"))

## LHS as a list to use in arsenal functions
f10 <- formulize(c("y1", "y2", "y3"), c("x", "z"), collapse.y = "list")

## use in an lm
f11 <- formulize(2, 3:4, data = mockstudy)
summary(lm(f11, data = mockstudy))
#> 
#> Call:
#> lm(formula = f11, data = mockstudy)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -41.800  -7.568   0.892   8.432  29.124 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   60.1075     0.5966 100.744   <2e-16 ***
#> armF: FOLFOX   0.6927     0.7088   0.977   0.3286    
#> armG: IROX     0.1484     0.8118   0.183   0.8550    
#> sexFemale     -1.2319     0.6105  -2.018   0.0438 *  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 11.51 on 1495 degrees of freedom
#> Multiple R-squared:  0.003365,   Adjusted R-squared:  0.001365 
#> F-statistic: 1.683 on 3 and 1495 DF,  p-value: 0.1688
#> 

## using non-syntactic names or calls (like reformulate example)
f12 <- formulize(as.name("+-"), c("`P/E`", "`% Growth`"))
f12 <- formulize("+-", c("P/E", "% Growth"), escape = TRUE)

f <- Surv(ft, case) ~ a + b
f13 <- formulize(f[[2]], f[[3]])
```
