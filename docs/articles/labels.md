# A Few Notes on Labels

## Introduction

The `arsenal` package relies somewhat heavily on variable labels to make
output more “pretty”. A `label` here is understood to be a single
character string with “pretty” text (i.e., not an “ugly” variable name).
Three of the main `arsenal` function use labels in their
[`summary()`](https://rdrr.io/r/base/summary.html) output. There are
several ways to set these labels.

We’ll use the `mockstudy` dataset for all examples here:

``` r

library(arsenal)
data(mockstudy)
library(magrittr)

# for 'freqlist' examples
tab.ex <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA="ifany")
```

## Examples

### Set labels in the function call

The [`summary()`](https://rdrr.io/r/base/summary.html) method for
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
and
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
objects contains a `labelTranslations =` argument to specify labels in
the function call. Note that the
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
function matches labels in order, whereas the other two match labels by
name. The labels can be input as a list or a character vector.

``` r

summary(freqlist(tab.ex),
        labelTranslations = c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL"))
```

| Treatment Arm | Gender | LASA QOL | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:--------------|:-------|:---------|-----:|----------------:|--------:|-------------------:|
| A: IFL        | Male   | 0        |   29 |              29 |    1.93 |               1.93 |
|               |        | 1        |  214 |             243 |   14.28 |              16.21 |
|               |        | NA       |   34 |             277 |    2.27 |              18.48 |
|               | Female | 0        |   12 |             289 |    0.80 |              19.28 |
|               |        | 1        |  118 |             407 |    7.87 |              27.15 |
|               |        | NA       |   21 |             428 |    1.40 |              28.55 |
| F: FOLFOX     | Male   | 0        |   31 |             459 |    2.07 |              30.62 |
|               |        | 1        |  285 |             744 |   19.01 |              49.63 |
|               |        | NA       |   95 |             839 |    6.34 |              55.97 |
|               | Female | 0        |   21 |             860 |    1.40 |              57.37 |
|               |        | 1        |  198 |            1058 |   13.21 |              70.58 |
|               |        | NA       |   61 |            1119 |    4.07 |              74.65 |
| G: IROX       | Male   | 0        |   17 |            1136 |    1.13 |              75.78 |
|               |        | 1        |  187 |            1323 |   12.47 |              88.26 |
|               |        | NA       |   24 |            1347 |    1.60 |              89.86 |
|               | Female | 0        |   14 |            1361 |    0.93 |              90.79 |
|               |        | 1        |  121 |            1482 |    8.07 |              98.87 |
|               |        | NA       |   17 |            1499 |    1.13 |             100.00 |

``` r

summary(tableby(arm ~ sex + age, data = mockstudy),
        labelTranslations = c(sex = "SEX", age = "Age, yrs"))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **SEX** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

``` r

summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy),
        labelTranslations = list(sexFemale = "Female", age = "Age, yrs"))
```

|              | estimate | std.error | p.value  | adj.r.squared | Nmiss |
|:-------------|:---------|:----------|:---------|:--------------|:------|
| (Intercept)  | 26.793   | 0.766     | \< 0.001 | 0.004         | 33    |
| **Age, yrs** | 0.012    | 0.012     | 0.348    |               |       |
| **Female**   | -0.718   | 0.291     | 0.014    |               |       |

### Modify labels after the fact

Another option is to add labels after you have created the object. To do
this, you can use the form `labels(x) <- value` or use the pipe-able
version,
[`set_labels()`](https://mayoverse.github.io/arsenal/reference/labels.md).

``` r

# the non-pipe version; somewhat clunky
tmp <- freqlist(tab.ex)
labels(tmp) <- c(arm = "Treatment Arm", sex = "Gender", mdquality.s = "LASA QOL")
summary(tmp)
```

| Treatment Arm | Gender | LASA QOL | Freq | Cumulative Freq | Percent | Cumulative Percent |
|:--------------|:-------|:---------|-----:|----------------:|--------:|-------------------:|
| A: IFL        | Male   | 0        |   29 |              29 |    1.93 |               1.93 |
|               |        | 1        |  214 |             243 |   14.28 |              16.21 |
|               |        | NA       |   34 |             277 |    2.27 |              18.48 |
|               | Female | 0        |   12 |             289 |    0.80 |              19.28 |
|               |        | 1        |  118 |             407 |    7.87 |              27.15 |
|               |        | NA       |   21 |             428 |    1.40 |              28.55 |
| F: FOLFOX     | Male   | 0        |   31 |             459 |    2.07 |              30.62 |
|               |        | 1        |  285 |             744 |   19.01 |              49.63 |
|               |        | NA       |   95 |             839 |    6.34 |              55.97 |
|               | Female | 0        |   21 |             860 |    1.40 |              57.37 |
|               |        | 1        |  198 |            1058 |   13.21 |              70.58 |
|               |        | NA       |   61 |            1119 |    4.07 |              74.65 |
| G: IROX       | Male   | 0        |   17 |            1136 |    1.13 |              75.78 |
|               |        | 1        |  187 |            1323 |   12.47 |              88.26 |
|               |        | NA       |   24 |            1347 |    1.60 |              89.86 |
|               | Female | 0        |   14 |            1361 |    0.93 |              90.79 |
|               |        | 1        |  121 |            1482 |    8.07 |              98.87 |
|               |        | NA       |   17 |            1499 |    1.13 |             100.00 |

``` r

# piped--much cleaner
mockstudy %>% 
  tableby(arm ~ sex + age, data = .) %>% 
  set_labels(c(sex = "SEX", age = "Age, yrs")) %>% 
  summary()
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **SEX** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

``` r

mockstudy %>% 
  modelsum(bmi ~ age, adjust = ~ sex, data = .) %>% 
  set_labels(list(sexFemale = "Female", age = "Age, yrs")) %>% 
  summary()
```

|              | estimate | std.error | p.value  | adj.r.squared | Nmiss |
|:-------------|:---------|:----------|:---------|:--------------|:------|
| (Intercept)  | 26.793   | 0.766     | \< 0.001 | 0.004         | 33    |
| **Age, yrs** | 0.012    | 0.012     | 0.348    |               |       |
| **Female**   | -0.718   | 0.291     | 0.014    |               |       |

### Add labels to a `data.frame`

[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
and
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
also allow you to have label attributes on the data. Note that by
default these attributes usually get dropped upon subsetting, but
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
and
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
use the
[`keep.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
function to retain them.

``` r

mockstudy.lab <- keep.labels(mockstudy)
class(mockstudy$age)
```

\[1\] “integer”

``` r

class(mockstudy.lab$age)
```

\[1\] “keep_labels” “integer”

To undo this, simply
[`loosen.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md):

``` r

class(loosen.labels(mockstudy.lab)$age)
```

\[1\] “integer”

You can set attributes one at a time in two ways:

``` r

attr(mockstudy.lab$sex, "label") <- "Sex"
labels(mockstudy.lab$age) <- "Age, yrs"
```

…or all at once:

``` r

labels(mockstudy.lab) <- list(sex = "Sex", age = "Age, yrs")
summary(tableby(arm ~ sex + age, data = mockstudy.lab))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Sex** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

You can pipe this, too.

``` r

mockstudy %>% 
  set_labels(list(sex = "SEX", age = "Age, yrs")) %>% 
  modelsum(bmi ~ age, adjust = ~ sex, data = .) %>% 
  summary()
```

|                | estimate | std.error | p.value  | adj.r.squared | Nmiss |
|:---------------|:---------|:----------|:---------|:--------------|:------|
| (Intercept)    | 26.793   | 0.766     | \< 0.001 | 0.004         | 33    |
| **Age, yrs**   | 0.012    | 0.012     | 0.348    |               |       |
| **SEX Female** | -0.718   | 0.291     | 0.014    |               |       |

To extract labels from a `data.frame`, simply use the
[`labels()`](https://mayoverse.github.io/arsenal/reference/labels.md)
function:

``` r

labels(mockstudy.lab)
```

    ## $case
    ## NULL
    ## 
    ## $age
    ## [1] "Age, yrs"
    ## 
    ## $arm
    ## [1] "Treatment Arm"
    ## 
    ## $sex
    ## [1] "Sex"
    ## 
    ## $race
    ## [1] "Race"
    ## 
    ## $fu.time
    ## NULL
    ## 
    ## $fu.stat
    ## NULL
    ## 
    ## $ps
    ## NULL
    ## 
    ## $hgb
    ## NULL
    ## 
    ## $bmi
    ## [1] "Body Mass Index (kg/m^2)"
    ## 
    ## $alk.phos
    ## NULL
    ## 
    ## $ast
    ## NULL
    ## 
    ## $mdquality.s
    ## NULL
    ## 
    ## $age.ord
    ## NULL

### When labels get long

[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
and
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
both support the wrapping of long labels. Consider the `width=` argument
in the [`print()`](https://rdrr.io/r/base/print.html) function:

``` r

mockstudy %>% 
  set_labels(list(age = "This is a really long label for the arm variable")) %>% 
  tableby(sex ~ age, data = .) %>% 
  summary() %>% 
  print(width = 20)
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **This is a really** |  |  |  | 0.048 |
| **long label for the** |  |  |  |  |
| **arm variable** |  |  |  |  |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |  |
|    Range | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |  |
