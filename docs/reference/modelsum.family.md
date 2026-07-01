# Family functions for modelsum

A set of family functions for
[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md).

## Usage

``` r
survival()

ordinal(method = c("logistic", "probit", "loglog", "cloglog", "cauchit"))

negbin(link = c("log", "identity", "sqrt"))

clog()

relrisk(link = "log")
```

## Arguments

- method:

  See `MASS::`[`polr`](https://rdrr.io/pkg/MASS/man/polr.html).

- link:

  See `MASS::`[`glm.nb`](https://rdrr.io/pkg/MASS/man/glm.nb.html).

## Value

A list, in particular with element `family`.

## See also

[`family`](https://rdrr.io/r/stats/family.html),
[`coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
[`polr`](https://rdrr.io/pkg/MASS/man/polr.html)
