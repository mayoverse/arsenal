# The tableby function

## Introduction

One of the most common tables in medical literature includes summary
statistics for a set of variables, often stratified by some group
(e.g. treatment arm). Locally at Mayo, the SAS macros `%table` and
`%summary` were written to create summary tables with a single call.
With the increasing interest in R, we have developed the function
`tableby` to create similar tables within the R environment.

In developing the
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
function, the goal was to bring the best features of these macros into
an R function. However, the task was not simply to duplicate all the
functionality, but rather to make use of R’s strengths (modeling, method
dispersion, flexibility in function definition and output format) and
make a tool that fits the needs of R users. Additionally, the results
needed to fit within the general reproducible research framework so the
tables could be displayed within an R markdown report.

This report provides step-by-step directions for using the functions
associated with
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
All functions presented here are available within the `arsenal` package.
An assumption is made that users are somewhat familiar with R Markdown
documents. For those who are new to the topic, a good initial resource
is available at [rmarkdown.rstudio.com](https://rmarkdown.rstudio.com/).

## Simple Example

The first step when using the `tableby` function is to load the
`arsenal` package. All the examples in this report use a dataset called
`mockstudy` made available by Paul Novotny which includes a variety of
types of variables (character, numeric, factor, ordered factor,
survival) to use as examples.

``` r

library(arsenal)
require(knitr)
```

    ## Loading required package: knitr

``` r

require(survival)
```

    ## Loading required package: survival

``` r

data(mockstudy) ##load data
dim(mockstudy)  ##look at how many subjects and variables are in the dataset 
```

    ## [1] 1499   14

``` r

# help(mockstudy) ##learn more about the dataset and variables
str(mockstudy) ##quick look at the data
```

    ## 'data.frame':    1499 obs. of  14 variables:
    ##  $ case       : int  110754 99706 105271 105001 112263 86205 99508 90158 88989 90515 ...
    ##  $ age        : int  67 74 50 71 69 56 50 57 51 63 ...
    ##   ..- attr(*, "label")= chr "Age in Years"
    ##  $ arm        : chr  "F: FOLFOX" "A: IFL" "A: IFL" "G: IROX" ...
    ##   ..- attr(*, "label")= chr "Treatment Arm"
    ##  $ sex        : Factor w/ 2 levels "Male","Female": 1 2 2 2 2 1 1 1 2 1 ...
    ##  $ race       : chr  "Caucasian" "Caucasian" "Caucasian" "Caucasian" ...
    ##   ..- attr(*, "label")= chr "Race"
    ##  $ fu.time    : int  922 270 175 128 233 120 369 421 387 363 ...
    ##  $ fu.stat    : int  2 2 2 2 2 2 2 2 2 2 ...
    ##  $ ps         : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ hgb        : num  11.5 10.7 11.1 12.6 13 10.2 13.3 12.1 13.8 12.1 ...
    ##  $ bmi        : num  25.1 19.5 NA 29.4 26.4 ...
    ##   ..- attr(*, "label")= chr "Body Mass Index (kg/m^2)"
    ##  $ alk.phos   : int  160 290 700 771 350 569 162 152 231 492 ...
    ##  $ ast        : int  35 52 100 68 35 27 16 12 25 18 ...
    ##  $ mdquality.s: int  NA 1 1 1 NA 1 1 1 1 1 ...
    ##  $ age.ord    : Ord.factor w/ 8 levels "10-19"<"20-29"<..: 6 7 4 7 6 5 4 5 5 6 ...

To create a simple table stratified by treatment arm, use a formula
statement to specify the variables that you want summarized. The example
below uses age (a continuous variable) and sex (a factor).

``` r

tab1 <- tableby(arm ~ sex + age, data=mockstudy)
```

If you want to take a quick look at the table, you can use
[`summary()`](https://rdrr.io/r/base/summary.html) on your tableby
object and the table will print out as text in your R console window. If
you use [`summary()`](https://rdrr.io/r/base/summary.html) without any
options you will see a number of $`\&nbsp;`$ statements which translates
to “space” in HTML.

### Pretty text version of table

If you want a nicer version in your console window then add the
`text=TRUE` option.

``` r

summary(tab1, text=TRUE)
```

    ## 
    ## 
    ## |             | A: IFL (N=428)  | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499)  | p value|
    ## |:------------|:---------------:|:-----------------:|:---------------:|:---------------:|-------:|
    ## |sex          |                 |                   |                 |                 |   0.190|
    ## |-  Male      |   277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |   916 (61.1%)   |        |
    ## |-  Female    |   151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |   583 (38.9%)   |        |
    ## |Age in Years |                 |                   |                 |                 |   0.614|
    ## |-  Mean (SD) | 59.673 (11.365) |  60.301 (11.632)  | 59.763 (11.499) | 59.985 (11.519) |        |
    ## |-  Range     | 27.000 - 88.000 |  19.000 - 88.000  | 26.000 - 85.000 | 19.000 - 88.000 |        |

### Pretty Rmarkdown version of table

In order for the report to look nice within an R markdown (knitr)
report, you just need to specify `results="asis"` when creating the R
chunk. This changes the layout slightly (compresses it) and bolds the
variable names.

``` r

summary(tab1)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **sex** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age in Years** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

### Data frame version of table

If you want a data.frame version, simply use `as.data.frame`.

``` r

as.data.frame(tab1)
```

    ##   group.term   group.label strata.term variable     term        label variable.type
    ## 1        arm Treatment Arm                  sex      sex          sex   categorical
    ## 2        arm Treatment Arm                  sex countpct         Male   categorical
    ## 3        arm Treatment Arm                  sex countpct       Female   categorical
    ## 4        arm Treatment Arm                  age      age Age in Years       numeric
    ## 5        arm Treatment Arm                  age   meansd    Mean (SD)       numeric
    ## 6        arm Treatment Arm                  age    range        Range       numeric
    ##                A: IFL           F: FOLFOX            G: IROX              Total
    ## 1                                                                              
    ## 2 277.00000, 64.71963 411.00000, 59.47902            228, 60  916.0000, 61.1074
    ## 3 151.00000, 35.28037 280.00000, 40.52098            152, 40  583.0000, 38.8926
    ## 4                                                                              
    ## 5  59.67290, 11.36454  60.30101, 11.63225 59.76316, 11.49930 59.98532, 11.51877
    ## 6              27, 88              19, 88             26, 85             19, 88
    ##                         test   p.value
    ## 1 Pearson's Chi-squared test 0.1904388
    ## 2 Pearson's Chi-squared test 0.1904388
    ## 3 Pearson's Chi-squared test 0.1904388
    ## 4         Linear Model ANOVA 0.6143859
    ## 5         Linear Model ANOVA 0.6143859
    ## 6         Linear Model ANOVA 0.6143859

### Summaries using standard R code

``` r

## base R frequency example
tmp <- table(Gender=mockstudy$sex, "Study Arm"=mockstudy$arm)
tmp
```

    ##         Study Arm
    ## Gender   A: IFL F: FOLFOX G: IROX
    ##   Male      277       411     228
    ##   Female    151       280     152

``` r

# Note: The continuity correction is applied by default in R (not used in %table)
chisq.test(tmp)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  tmp
    ## X-squared = 3.3168, df = 2, p-value = 0.1904

``` r

## base R numeric summary example
tapply(mockstudy$age, mockstudy$arm, summary)
```

    ## $`A: IFL`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   27.00   53.00   61.00   59.67   68.00   88.00 
    ## 
    ## $`F: FOLFOX`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    19.0    52.0    61.0    60.3    69.0    88.0 
    ## 
    ## $`G: IROX`
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   26.00   52.00   61.00   59.76   68.00   85.00

``` r

summary(aov(age ~ arm, data=mockstudy))
```

    ##               Df Sum Sq Mean Sq F value Pr(>F)
    ## arm            2    129    64.7   0.487  0.614
    ## Residuals   1496 198628   132.8

## Modifying Output

### Add labels

In the above example, age is shown with a label (Age in Years), but sex
is listed “as is” with lower case letters. This is because the data was
created in SAS and in the SAS dataset, age had a label but sex did not.
The label is stored as an attribute within R.

``` r

## Look at one variable's label
attr(mockstudy$age,'label')
```

    ## [1] "Age in Years"

``` r

## See all the variables with a label
unlist(lapply(mockstudy,'attr','label'))
```

    ##                        age                        arm                       race 
    ##             "Age in Years"            "Treatment Arm"                     "Race" 
    ##                        bmi 
    ## "Body Mass Index (kg/m^2)"

``` r

# Can also use labels(mockstudy)
```

If you want to add labels to other variables, there are a couple of
options. First, you could add labels to the variables in your dataset.

``` r

attr(mockstudy$sex,'label')  <- 'Gender'

tab1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(tab1)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age in Years** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

You can also use the built-in `data.frame` method for `labels<-`:

``` r

labels(mockstudy)  <- c(age = 'Age, yrs', sex = "Gender")

tab1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(tab1)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

Another option is to add labels after you have created the table

``` r

mylabels <- list(sex = "SEX", age = "Age, yrs")
summary(tab1, labelTranslations = mylabels)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **SEX** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

Alternatively, you can check the variable labels and manipulate them
with a function called `labels`, which works on the `tableby` object.

``` r

labels(tab1)
```

    ##             arm             sex             age 
    ## "Treatment Arm"        "Gender"      "Age, yrs"

``` r

labels(tab1) <- c(arm="Treatment Assignment", age="Baseline Age (yrs)")
labels(tab1)
```

    ##                    arm                    sex                    age 
    ## "Treatment Assignment"               "Gender"   "Baseline Age (yrs)"

``` r

summary(tab1)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Baseline Age (yrs)** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

### Change summary statistics globally

Currently the default behavior is to summarize continuous variables
with: Number of missing values, Mean (SD), 25th - 75th quantiles, and
Minimum-Maximum values with an ANOVA (t-test with equal variances)
p-value. For categorical variables the default is to show: Number of
missing values and count (column percent) with a chi-square p-value.
This behavior can be modified using the tableby.control function. In
fact, you can save your standard settings and use that for future
tables. Note that `test=FALSE` and `total=FALSE` results in the total
column and p-value column not being printed.

``` r

mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                               numeric.test="kwt", cat.test="chisq",
                               numeric.stats=c("N", "median", "q1q3"),
                               cat.stats=c("countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))
tab2 <- tableby(arm ~ sex + age, data=mockstudy, control=mycontrols)
summary(tab2)
```

|              | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) |
|:-------------|:--------------:|:-----------------:|:---------------:|
| **Gender**   |                |                   |                 |
|    Male      |  277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |
|    Female    |  151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |
| **Age, yrs** |                |                   |                 |
|    Count     |      428       |        691        |       380       |
|    Median    |     61.000     |      61.000       |     61.000      |
|    Q1,Q3     | 53.000, 68.000 |  52.000, 69.000   | 52.000, 68.000  |

You can also change these settings directly in the tableby call.

``` r

tab3 <- tableby(arm ~ sex + age, data=mockstudy, test=FALSE, total=FALSE, 
                numeric.stats=c("median","q1q3"), numeric.test="kwt")
summary(tab3)
```

|              | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) |
|:-------------|:--------------:|:-----------------:|:---------------:|
| **Gender**   |                |                   |                 |
|    Male      |  277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |
|    Female    |  151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |
| **Age, yrs** |                |                   |                 |
|    Median    |     61.000     |      61.000       |     61.000      |
|    Q1, Q3    | 53.000, 68.000 |  52.000, 69.000   | 52.000, 68.000  |

### Change summary statistics within the formula

In addition to modifying summary options globally, it is possible to
modify the test and summary statistics for specific variables within the
formula statement. For example, both the kwt (Kruskal-Wallis rank-based)
and anova (asymptotic analysis of variance) tests apply to numeric
variables, and we can use one for the variable “age”, another for the
variable “bmi”, and no test for the variable “ast”. A list of all the
options is shown at the end of the vignette.

The `tests` function can do a quick check on what tests were performed
on each variable in tableby.

``` r

tab.test <- tableby(arm ~ kwt(age) + anova(bmi) + notest(ast), data=mockstudy)
tests(tab.test)
```

    ##           Group Variable   p.value                       Method
    ## 1 Treatment Arm      age 0.6390614 Kruskal-Wallis rank sum test
    ## 2 Treatment Arm      bmi 0.8916552           Linear Model ANOVA
    ## 3 Treatment Arm      ast        NA                      No test

``` r

summary(tab.test)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.639 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.892 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 27.290 (5.552) | 27.210 (5.173) | 27.106 (5.751) | 27.206 (5.432) |  |
|    Range | 14.053 - 53.008 | 16.649 - 49.130 | 15.430 - 60.243 | 14.053 - 60.243 |  |
| **ast** |  |  |  |  |  |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 37.292 (28.036) | 35.202 (26.659) | 35.670 (25.807) | 35.933 (26.843) |  |
|    Range | 10.000 - 205.000 | 7.000 - 174.000 | 5.000 - 176.000 | 5.000 - 205.000 |  |

Summary statistics for any individual variable can also be modified, but
it must be done as secondary arguments to the test function. The
function names must be strings that are functions already written for
tableby, built-in R functions like mean and range, or user-defined
functions.

``` r

tab.test <- tableby(arm ~ kwt(ast, "Nmiss2","median") + anova(age, "N","mean") +
                    notest(bmi, "Nmiss","median"), data=mockstudy)
summary(tab.test)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **ast** |  |  |  |  | 0.039 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Median | 29.000 | 25.500 | 27.000 | 27.000 |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    N | 428 | 691 | 380 | 1499 |  |
|    Mean | 59.673 | 60.301 | 59.763 | 59.985 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  |  |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Median | 26.234 | 26.525 | 25.978 | 26.325 |  |

These can also be passed to the `stats=` argument.

``` r

tab.test <- tableby(arm ~ kwt(ast, stats = c("Nmiss2", "median")) + anova(age, stats = c("N","mean")) +
                    notest(bmi, stats = c("Nmiss","median")), data=mockstudy)
summary(tab.test)
```

### Controlling Options for Categorical Tests (Chisq and Fisher’s)

The formal tests for categorical variables against the levels of the by
variable, chisq and fe, have options to simulate p-values. We show how
to turn on the simulations for these with 500 replicates for the
Fisher’s test (fe).

``` r

set.seed(100)
tab.catsim <- tableby(arm ~ sex + race, cat.test="fe", simulate.p.value=TRUE, B=500, data=mockstudy)
tests(tab.catsim)
```

          Group Variable   p.value

1 Treatment Arm sex 0.2195609 2 Treatment Arm race 0.3093812 Method 1
Fisher’s Exact Test for Count Data with simulated p-valuebased on 500
replicates) 2 Fisher’s Exact Test for Count Data with simulated
p-valuebased on 500 replicates)

The chi-square test on 2x2 tables applies Yates’ continuity correction
by default, so we provide an option to turn off the correction. We show
the results with and without the correction that is applied to treatment
arm by sex, if we use subset to ignore one of the three treatment arms.

``` r

cat.correct <- tableby(arm ~ sex + race, cat.test="chisq", subset = !grepl("^F", arm), data=mockstudy)
tests(cat.correct)
```

          Group Variable   p.value                     Method

1 Treatment Arm sex 0.1666280 Pearson’s Chi-squared test 2 Treatment Arm
race 0.8108543 Pearson’s Chi-squared test

``` r

cat.nocorrect <- tableby(arm ~ sex + race, cat.test="chisq", subset = !grepl("^F", arm),
     chisq.correct=FALSE, data=mockstudy)
tests(cat.nocorrect)
```

          Group Variable   p.value                     Method

1 Treatment Arm sex 0.1666280 Pearson’s Chi-squared test 2 Treatment Arm
race 0.8108543 Pearson’s Chi-squared test

### Modifying the look & feel in Word documents

You can easily create Word versions of `tableby` output via an Rmarkdown
report and the default options will give you a reasonable table in
Word - just select the “Knit Word” option in RStudio.

**The functionality listed in this next paragraph is coming soon but
needs an upgraded version of RStudio** If you want to modify fonts used
for the table, then you’ll need to add an extra line to your header at
the beginning of your file. You can take the
`WordStylesReference01.docx` file and modify the fonts (storing the
format preferences in your project directory). To see how this works,
run your report once using WordStylesReference01.docx and then
WordStylesReference02.docx.

    output: word_document
      reference_docx: /projects/bsi/gentools/R/lib320/arsenal/doc/WordStylesReference01.docx 

For more information on changing the look/feel of your Word document,
see the [Rmarkdown
documentation](https://bookdown.org/yihui/rmarkdown/word-document.html)
website.

## Additional Examples

Here are multiple examples showing how to use some of the different
options.

### 1. Summarize without a group/by variable

``` r

tab.noby <- tableby(~ bmi + sex + age, data=mockstudy)
summary(tab.noby)
```

|                              | Overall (N=1499) |
|:-----------------------------|:----------------:|
| **Body Mass Index (kg/m^2)** |                  |
|    N-Miss                    |        33        |
|    Mean (SD)                 |  27.206 (5.432)  |
|    Range                     | 14.053 - 60.243  |
| **Gender**                   |                  |
|    Male                      |   916 (61.1%)    |
|    Female                    |   583 (38.9%)    |
| **Age, yrs**                 |                  |
|    Mean (SD)                 | 59.985 (11.519)  |
|    Range                     | 19.000 - 88.000  |

### 2. Display footnotes indicating which “test” was used

``` r

summary(tab.test, pfootnote=TRUE)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **ast** |  |  |  |  | 0.039¹ |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Median | 29.000 | 25.500 | 27.000 | 27.000 |  |
| **Age, yrs** |  |  |  |  | 0.614² |
|    N | 428 | 691 | 380 | 1499 |  |
|    Mean | 59.673 | 60.301 | 59.763 | 59.985 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  |  |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Median | 26.234 | 26.525 | 25.978 | 26.325 |  |

1.  Kruskal-Wallis rank sum test
2.  Linear Model ANOVA

### 3. Summarize an ordered factor

When comparing groups of ordered data there are a couple of options. The
**default** uses a general independence test available from the `coin`
package. For two-group comparisons, this is essentially the Armitage
trend test. The other option is to specify the Kruskal Wallis test. The
example below shows both options.

``` r

mockstudy$age.ordnew <- ordered(c("a",NA,as.character(mockstudy$age.ord[-(1:2)])))
table(mockstudy$age.ord, mockstudy$sex)
```

    ##        
    ##         Male Female
    ##   10-19    1      0
    ##   20-29    8     11
    ##   30-39   37     30
    ##   40-49  127     83
    ##   50-59  257    179
    ##   60-69  298    170
    ##   70-79  168    101
    ##   80-89   20      9

``` r

table(mockstudy$age.ordnew, mockstudy$sex)
```

    ##        
    ##         Male Female
    ##   10-19    1      0
    ##   20-29    8     11
    ##   30-39   37     30
    ##   40-49  127     83
    ##   50-59  257    179
    ##   60-69  297    170
    ##   70-79  168    100
    ##   80-89   20      9
    ##   a        1      0

``` r

class(mockstudy$age.ord)
```

    ## [1] "ordered" "factor"

``` r

summary(tableby(sex ~ age.ordnew, data = mockstudy), pfootnote = TRUE)
```

|                | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---------------|:------------:|:--------------:|:--------------:|--------:|
| **age.ordnew** |              |                |                |  0.040¹ |
|    N-Miss      |      0       |       1        |       1        |         |
|    10-19       |   1 (0.1%)   |    0 (0.0%)    |    1 (0.1%)    |         |
|    20-29       |   8 (0.9%)   |   11 (1.9%)    |   19 (1.3%)    |         |
|    30-39       |  37 (4.0%)   |   30 (5.2%)    |   67 (4.5%)    |         |
|    40-49       | 127 (13.9%)  |   83 (14.3%)   |  210 (14.0%)   |         |
|    50-59       | 257 (28.1%)  |  179 (30.8%)   |  436 (29.1%)   |         |
|    60-69       | 297 (32.4%)  |  170 (29.2%)   |  467 (31.2%)   |         |
|    70-79       | 168 (18.3%)  |  100 (17.2%)   |  268 (17.9%)   |         |
|    80-89       |  20 (2.2%)   |    9 (1.5%)    |   29 (1.9%)    |         |
|    a           |   1 (0.1%)   |    0 (0.0%)    |    1 (0.1%)    |         |

1.  Trend test for ordinal variables

``` r

summary(tableby(sex ~ age.ord, data = mockstudy), pfootnote = TRUE)
```

|             | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:------------|:------------:|:--------------:|:--------------:|--------:|
| **age.ord** |              |                |                |  0.049¹ |
|    10-19    |   1 (0.1%)   |    0 (0.0%)    |    1 (0.1%)    |         |
|    20-29    |   8 (0.9%)   |   11 (1.9%)    |   19 (1.3%)    |         |
|    30-39    |  37 (4.0%)   |   30 (5.1%)    |   67 (4.5%)    |         |
|    40-49    | 127 (13.9%)  |   83 (14.2%)   |  210 (14.0%)   |         |
|    50-59    | 257 (28.1%)  |  179 (30.7%)   |  436 (29.1%)   |         |
|    60-69    | 298 (32.5%)  |  170 (29.2%)   |  468 (31.2%)   |         |
|    70-79    | 168 (18.3%)  |  101 (17.3%)   |  269 (17.9%)   |         |
|    80-89    |  20 (2.2%)   |    9 (1.5%)    |   29 (1.9%)    |         |

1.  Trend test for ordinal variables

### 4. Summarize a survival variable

First look at the information that is presented by the
[`survfit()`](https://rdrr.io/pkg/survival/man/survfit.html) function,
then see how the same results can be seen with tableby. The default is
to show the median survival (time at which the probability of survival =
50%).

``` r

survfit(Surv(fu.time, fu.stat)~sex, data=mockstudy)
```

    ## Call: survfit(formula = Surv(fu.time, fu.stat) ~ sex, data = mockstudy)
    ## 
    ##              n events median 0.95LCL 0.95UCL
    ## sex=Male   916    829    550     515     590
    ## sex=Female 583    527    543     511     575

``` r

survdiff(Surv(fu.time, fu.stat)~sex, data=mockstudy)
```

    ## Call:
    ## survdiff(formula = Surv(fu.time, fu.stat) ~ sex, data = mockstudy)
    ## 
    ##              N Observed Expected (O-E)^2/E (O-E)^2/V
    ## sex=Male   916      829      830  0.000370  0.000956
    ## sex=Female 583      527      526  0.000583  0.000956
    ## 
    ##  Chisq= 0  on 1 degrees of freedom, p= 1

``` r

summary(tableby(sex ~ Surv(fu.time, fu.stat), data=mockstudy))
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Surv(fu.time, fu.stat)** |  |  |  | 0.975 |
|    Events | 829 | 527 | 1356 |  |
|    Median Survival | 550.000 | 543.000 | 546.000 |  |

It is also possible to obtain summaries of the % survival at certain
time points (say the probability of surviving 1-year).

``` r

summary(survfit(Surv(fu.time/365.25, fu.stat)~sex, data=mockstudy), times=1:5)
```

    ## Call: survfit(formula = Surv(fu.time/365.25, fu.stat) ~ sex, data = mockstudy)
    ## 
    ##                 sex=Male 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     1    626     286   0.6870  0.0153       0.6576       0.7177
    ##     2    309     311   0.3437  0.0158       0.3142       0.3761
    ##     3    152     151   0.1748  0.0127       0.1516       0.2015
    ##     4     57      61   0.0941  0.0104       0.0759       0.1168
    ##     5     24      16   0.0628  0.0095       0.0467       0.0844
    ## 
    ##                 sex=Female 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     1    380     202   0.6531  0.0197       0.6155        0.693
    ##     2    190     189   0.3277  0.0195       0.2917        0.368
    ##     3     95      90   0.1701  0.0157       0.1420        0.204
    ##     4     51      32   0.1093  0.0133       0.0861        0.139
    ##     5     18      12   0.0745  0.0126       0.0534        0.104

``` r

summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, times=1:3, surv.stats=c("NeventsSurv")))
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Surv(fu.time/365.25, fu.stat)** |  |  |  | 0.975 |
|    time = 1 | 286 (68.7) | 202 (65.3) | 488 (67.4) |  |
|    time = 2 | 597 (34.4) | 391 (32.8) | 988 (33.7) |  |
|    time = 3 | 748 (17.5) | 481 (17.0) | 1229 (17.3) |  |

``` r

summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, times=1:5, surv.stats=c("NriskSurv")))
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Surv(fu.time/365.25, fu.stat)** |  |  |  | 0.975 |
|    time = 1 | 626 (68.7) | 380 (65.3) | 1006 (67.4) |  |
|    time = 2 | 309 (34.4) | 190 (32.8) | 499 (33.7) |  |
|    time = 3 | 152 (17.5) | 95 (17.0) | 247 (17.3) |  |
|    time = 4 | 57 (9.4) | 51 (10.9) | 108 (10.1) |  |
|    time = 5 | 24 (6.3) | 18 (7.4) | 42 (6.8) |  |

``` r

summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, surv.stats="medSurvCI", survconf.type='log-log'), digits=2)
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Surv(fu.time/365.25, fu.stat)** |  |  |  | 0.975 |
|    Median (CI) | 1.51 (1.41, 1.62) | 1.49 (1.40, 1.57) | 1.49 (1.42, 1.56) |  |

``` r

summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, surv.stats="medSurvQuant"), digits=2)
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Surv(fu.time/365.25, fu.stat)** |  |  |  | 0.975 |
|    Median (Q1, Q3) Survival | 1.51 (0.87, 2.46) | 1.49 (0.82, 2.41) | 1.49 (0.86, 2.43) |  |

### 5. Summarize date variables

Date variables by default are summarized with the number of missing
values, the median, and the range. For example purposes we’ve created a
random date. Missing values are introduced for impossible February
dates.

``` r

set.seed(100)
N <- nrow(mockstudy)
mockstudy$dtentry <- mdy.Date(month=sample(1:12,N,replace=T), day=sample(1:29,N,replace=T), 
                              year=sample(2005:2009,N,replace=T))
summary(tableby(sex ~ dtentry, data=mockstudy))
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **dtentry** |  |  |  | 0.661 |
|    N-Miss | 2 | 3 | 5 |  |
|    Median | 2007-05-25 | 2007-05-08 | 2007-05-22 |  |
|    Range | 2005-01-02 - 2009-12-28 | 2005-01-01 - 2009-12-25 | 2005-01-01 - 2009-12-28 |  |

### 6. Summarize multiple variables without typing them out

Often one wants to summarize a number of variables. Instead of typing by
hand each individual variable, an alternative approach is to create a
formula using the `paste` command with the `collapse="+"` option.

``` r

## create a vector specifying the variable names
myvars <- names(mockstudy)

## select the 8th through the last variables
## paste them together, separated by the + sign
RHS <- paste(myvars[8:10], collapse="+")
RHS
```

\[1\] “ps+hgb+bmi”

``` r

## create a formula using the as.formula function
as.formula(paste('arm ~ ', RHS))
```

arm ~ ps + hgb + bmi

``` r

## use the formula in the tableby function
summary(tableby(as.formula(paste('arm ~', RHS)), data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **ps** |  |  |  |  | 0.903 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 0.529 (0.597) | 0.547 (0.595) | 0.537 (0.606) | 0.539 (0.598) |  |
|    Range | 0.000 - 2.000 | 0.000 - 2.000 | 0.000 - 2.000 | 0.000 - 2.000 |  |
| **hgb** |  |  |  |  | 0.639 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 12.276 (1.686) | 12.381 (1.763) | 12.373 (1.680) | 12.348 (1.719) |  |
|    Range | 9.060 - 17.300 | 9.000 - 18.200 | 9.000 - 17.000 | 9.000 - 18.200 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.892 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 27.290 (5.552) | 27.210 (5.173) | 27.106 (5.751) | 27.206 (5.432) |  |
|    Range | 14.053 - 53.008 | 16.649 - 49.130 | 15.430 - 60.243 | 14.053 - 60.243 |  |

These steps can also be done using the `formulize` function.

``` r

## The formulize function does the paste and as.formula steps
tmp <- formulize('arm',myvars[8:10])
tmp
```

arm ~ ps + hgb + bmi

``` r

## More complex formulas could also be written using formulize
tmp2 <- formulize('arm',c('ps','hgb^2','bmi'))

## use the formula in the tableby function
summary(tableby(tmp, data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **ps** |  |  |  |  | 0.903 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 0.529 (0.597) | 0.547 (0.595) | 0.537 (0.606) | 0.539 (0.598) |  |
|    Range | 0.000 - 2.000 | 0.000 - 2.000 | 0.000 - 2.000 | 0.000 - 2.000 |  |
| **hgb** |  |  |  |  | 0.639 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 12.276 (1.686) | 12.381 (1.763) | 12.373 (1.680) | 12.348 (1.719) |  |
|    Range | 9.060 - 17.300 | 9.000 - 18.200 | 9.000 - 17.000 | 9.000 - 18.200 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.892 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 27.290 (5.552) | 27.210 (5.173) | 27.106 (5.751) | 27.206 (5.432) |  |
|    Range | 14.053 - 53.008 | 16.649 - 49.130 | 15.430 - 60.243 | 14.053 - 60.243 |  |

To change summary statistics or statistical tests en masse, consider
using [`paste0()`](https://rdrr.io/r/base/paste.html) together with
[`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md):

``` r

varlist1 <- c('age','sex','hgb')
varlist2 <- paste0("anova(", c('bmi','alk.phos','ast'), ", 'meansd')")

summary(tableby(formulize("arm", c(varlist1, varlist2)),
                data = mockstudy, numeric.test = "kwt"), pfootnote = TRUE)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.639¹ |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |
| **Gender** |  |  |  |  | 0.190² |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **hgb** |  |  |  |  | 0.570¹ |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 12.276 (1.686) | 12.381 (1.763) | 12.373 (1.680) | 12.348 (1.719) |  |
|    Range | 9.060 - 17.300 | 9.000 - 18.200 | 9.000 - 17.000 | 9.000 - 18.200 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.892³ |
|    Mean (SD) | 27.290 (5.552) | 27.210 (5.173) | 27.106 (5.751) | 27.206 (5.432) |  |
| **alk.phos** |  |  |  |  | 0.226³ |
|    Mean (SD) | 175.577 (128.608) | 161.984 (121.978) | 173.506 (138.564) | 168.969 (128.492) |  |
| **ast** |  |  |  |  | 0.507³ |
|    Mean (SD) | 37.292 (28.036) | 35.202 (26.659) | 35.670 (25.807) | 35.933 (26.843) |  |

1.  Kruskal-Wallis rank sum test
2.  Pearson’s Chi-squared test
3.  Linear Model ANOVA

### 7. Subset the dataset used in the analysis

Here are two ways to get the same result (limit the analysis to subjects
age\>5 and in the F: FOLFOX treatment group).

- The first approach uses the subset function applied to the dataset
  `mockstudy`. This example also selects a subset of variables. The
  `tableby` function is then applied to this subsetted data.

``` r

newdata <- subset(mockstudy, subset=age>50 & arm=='F: FOLFOX', select = c(sex,ps:bmi))
dim(mockstudy)
```

    ## [1] 1499   16

``` r

table(mockstudy$arm)
```

    ## 
    ##    A: IFL F: FOLFOX   G: IROX 
    ##       428       691       380

``` r

dim(newdata)
```

    ## [1] 557   4

``` r

names(newdata)
```

    ## [1] "sex" "ps"  "hgb" "bmi"

``` r

summary(tableby(sex ~ ., data=newdata))
```

|              |  Male (N=333)   | Female (N=224)  |  Total (N=557)  |  p value |
|:-------------|:---------------:|:---------------:|:---------------:|---------:|
| **ps**       |                 |                 |                 |    0.652 |
|    N-Miss    |       64        |       44        |       108       |          |
|    Mean (SD) |  0.554 (0.600)  |  0.528 (0.602)  |  0.543 (0.600)  |          |
|    Range     |  0.000 - 2.000  |  0.000 - 2.000  |  0.000 - 2.000  |          |
| **hgb**      |                 |                 |                 | \< 0.001 |
|    N-Miss    |       64        |       44        |       108       |          |
|    Mean (SD) | 12.720 (1.925)  | 12.063 (1.395)  | 12.457 (1.760)  |          |
|    Range     | 9.000 - 18.200  | 9.100 - 15.900  | 9.000 - 18.200  |          |
| **bmi**      |                 |                 |                 |    0.650 |
|    N-Miss    |        9        |        6        |       15        |          |
|    Mean (SD) | 27.539 (4.780)  | 27.337 (5.508)  | 27.458 (5.081)  |          |
|    Range     | 17.927 - 47.458 | 16.649 - 49.130 | 16.649 - 49.130 |          |

- The second approach does the same analysis but uses the subset
  argument within `tableby` to subset the data.

``` r

summary(tableby(sex ~ ps + hgb + bmi, subset=age>50 & arm=="F: FOLFOX", data=mockstudy))
```

|  | Male (N=333) | Female (N=224) | Total (N=557) | p value |
|:---|:--:|:--:|:--:|---:|
| **ps** |  |  |  | 0.652 |
|    N-Miss | 64 | 44 | 108 |  |
|    Mean (SD) | 0.554 (0.600) | 0.528 (0.602) | 0.543 (0.600) |  |
|    Range | 0.000 - 2.000 | 0.000 - 2.000 | 0.000 - 2.000 |  |
| **hgb** |  |  |  | \< 0.001 |
|    N-Miss | 64 | 44 | 108 |  |
|    Mean (SD) | 12.720 (1.925) | 12.063 (1.395) | 12.457 (1.760) |  |
|    Range | 9.000 - 18.200 | 9.100 - 15.900 | 9.000 - 18.200 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.650 |
|    N-Miss | 9 | 6 | 15 |  |
|    Mean (SD) | 27.539 (4.780) | 27.337 (5.508) | 27.458 (5.081) |  |
|    Range | 17.927 - 47.458 | 16.649 - 49.130 | 16.649 - 49.130 |  |

### 8. Create combinations of variables on the fly

``` r

## create a variable combining the levels of mdquality.s and sex
with(mockstudy, table(interaction(mdquality.s,sex)))
```

    ## 
    ##   0.Male   1.Male 0.Female 1.Female 
    ##       77      686       47      437

``` r

summary(tableby(arm ~ interaction(mdquality.s,sex), data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **interaction(mdquality.s, sex)** |  |  |  |  | 0.493 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    0.Male | 29 (7.8%) | 31 (5.8%) | 17 (5.0%) | 77 (6.2%) |  |
|    1.Male | 214 (57.4%) | 285 (53.3%) | 187 (55.2%) | 686 (55.0%) |  |
|    0.Female | 12 (3.2%) | 21 (3.9%) | 14 (4.1%) | 47 (3.8%) |  |
|    1.Female | 118 (31.6%) | 198 (37.0%) | 121 (35.7%) | 437 (35.0%) |  |

``` r

## create a new grouping variable with combined levels of arm and sex
summary(tableby(interaction(mdquality.s, sex) ~  age + bmi, data=mockstudy, subset=arm=="F: FOLFOX"))
```

|  | 0.Male (N=31) | 1.Male (N=285) | 0.Female (N=21) | 1.Female (N=198) | Total (N=535) | p value |
|:---|:--:|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  |  | 0.190 |
|    Mean (SD) | 63.065 (11.702) | 60.653 (11.833) | 60.810 (10.103) | 58.924 (11.366) | 60.159 (11.612) |  |
|    Range | 41.000 - 82.000 | 19.000 - 88.000 | 42.000 - 81.000 | 29.000 - 83.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  |  | 0.894 |
|    N-Miss | 0 | 6 | 1 | 5 | 12 |  |
|    Mean (SD) | 26.633 (5.094) | 27.387 (4.704) | 27.359 (4.899) | 27.294 (5.671) | 27.307 (5.100) |  |
|    Range | 20.177 - 41.766 | 17.927 - 47.458 | 19.801 - 39.369 | 16.799 - 44.841 | 16.799 - 47.458 |  |

### 9. Transform variables on the fly

Certain transformations need to be surrounded by
[`I()`](https://rdrr.io/r/base/AsIs.html) so that R knows to treat it as
a variable transformation and not some special model feature. If the
transformation includes any of the symbols `/ - + ^ *` then surround the
new variable by [`I()`](https://rdrr.io/r/base/AsIs.html).

``` r

trans <- tableby(arm ~ I(age/10) + log(bmi) + factor(mdquality.s, levels=0:1, labels=c('N','Y')),
                 data=mockstudy)
summary(trans)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 5.967 (1.136) | 6.030 (1.163) | 5.976 (1.150) | 5.999 (1.152) |  |
|    Range | 2.700 - 8.800 | 1.900 - 8.800 | 2.600 - 8.500 | 1.900 - 8.800 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.811 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 3.287 (0.197) | 3.286 (0.183) | 3.279 (0.200) | 3.285 (0.192) |  |
|    Range | 2.643 - 3.970 | 2.812 - 3.894 | 2.736 - 4.098 | 2.643 - 4.098 |  |
| **factor(mdquality.s, levels = 0:1, labels = c(“N”, “Y”))** |  |  |  |  | 0.694 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    N | 41 (11.0%) | 52 (9.7%) | 31 (9.1%) | 124 (9.9%) |  |
|    Y | 332 (89.0%) | 483 (90.3%) | 308 (90.9%) | 1123 (90.1%) |  |

The labels for these variables aren’t exactly what we’d like, so we can
change modify those after the fact. Instead of typing out the very long
variable names, you can modify specific labels by position.

``` r

labels(trans)
```

    ##                                                           arm 
    ##                                               "Treatment Arm" 
    ##                                                     I(age/10) 
    ##                                                    "Age, yrs" 
    ##                                                      log(bmi) 
    ##                                    "Body Mass Index (kg/m^2)" 
    ##       factor(mdquality.s, levels = 0:1, labels = c("N", "Y")) 
    ## "factor(mdquality.s, levels = 0:1, labels = c(\"N\", \"Y\"))"

``` r

labels(trans)[2:4] <- c('Age per 10 yrs', 'log(BMI)', 'MD Quality')
labels(trans)
```

    ##                                                     arm 
    ##                                         "Treatment Arm" 
    ##                                               I(age/10) 
    ##                                        "Age per 10 yrs" 
    ##                                                log(bmi) 
    ##                                              "log(BMI)" 
    ## factor(mdquality.s, levels = 0:1, labels = c("N", "Y")) 
    ##                                            "MD Quality"

``` r

summary(trans)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age per 10 yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 5.967 (1.136) | 6.030 (1.163) | 5.976 (1.150) | 5.999 (1.152) |  |
|    Range | 2.700 - 8.800 | 1.900 - 8.800 | 2.600 - 8.500 | 1.900 - 8.800 |  |
| **log(BMI)** |  |  |  |  | 0.811 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 3.287 (0.197) | 3.286 (0.183) | 3.279 (0.200) | 3.285 (0.192) |  |
|    Range | 2.643 - 3.970 | 2.812 - 3.894 | 2.736 - 4.098 | 2.643 - 4.098 |  |
| **MD Quality** |  |  |  |  | 0.694 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    N | 41 (11.0%) | 52 (9.7%) | 31 (9.1%) | 124 (9.9%) |  |
|    Y | 332 (89.0%) | 483 (90.3%) | 308 (90.9%) | 1123 (90.1%) |  |

Note that if we had not changed `mdquality.s` to a factor, it would have
been summarized as though it were a continuous variable.

``` r

class(mockstudy$mdquality.s)
```

\[1\] “integer”

``` r

summary(tableby(arm~mdquality.s, data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **mdquality.s** |  |  |  |  | 0.695 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    Mean (SD) | 0.890 (0.313) | 0.903 (0.297) | 0.909 (0.289) | 0.901 (0.299) |  |
|    Range | 0.000 - 1.000 | 0.000 - 1.000 | 0.000 - 1.000 | 0.000 - 1.000 |  |

Another option would be to specify the test and summary statistics. In
fact, if I had a set of variables coded 0/1 and that was all I was
summarizing, then I could change the global option for continuous
variables to use the chi-square test and show countpct.

``` r

summary(tableby(arm ~ chisq(mdquality.s, "Nmiss","countpct"), data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **mdquality.s** |  |  |  |  | 0.694 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    0 | 41 (11.0%) | 52 (9.7%) | 31 (9.1%) | 124 (9.9%) |  |
|    1 | 332 (89.0%) | 483 (90.3%) | 308 (90.9%) | 1123 (90.1%) |  |

### 10. Subsetting (change the ordering of the variables, delete a variable, sort by p-value, filter by p-value, show only certain by-levels)

``` r

mytab <- tableby(arm ~ sex + alk.phos + age, data=mockstudy)
mytab2 <- mytab[c('age','sex','alk.phos')]
summary(mytab2)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **alk.phos** |  |  |  |  | 0.226 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 175.577 (128.608) | 161.984 (121.978) | 173.506 (138.564) | 168.969 (128.492) |  |
|    Range | 11.000 - 858.000 | 10.000 - 1014.000 | 7.000 - 982.000 | 7.000 - 1014.000 |  |

``` r

summary(mytab[c('age','sex')], digits = 2)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.67 (11.36) | 60.30 (11.63) | 59.76 (11.50) | 59.99 (11.52) |  |
|    Range | 27.00 - 88.00 | 19.00 - 88.00 | 26.00 - 85.00 | 19.00 - 88.00 |  |
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |

``` r

summary(mytab[c(3,1)], digits = 3)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |

``` r

summary(sort(mytab, decreasing = TRUE))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |
| **alk.phos** |  |  |  |  | 0.226 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 175.577 (128.608) | 161.984 (121.978) | 173.506 (138.564) | 168.969 (128.492) |  |
|    Range | 11.000 - 858.000 | 10.000 - 1014.000 | 7.000 - 982.000 | 7.000 - 1014.000 |  |
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |

``` r

summary(mytab[mytab < 0.5])
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **alk.phos** |  |  |  |  | 0.226 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 175.577 (128.608) | 161.984 (121.978) | 173.506 (138.564) | 168.969 (128.492) |  |
|    Range | 11.000 - 858.000 | 10.000 - 1014.000 | 7.000 - 982.000 | 7.000 - 1014.000 |  |

``` r

head(mytab, 1) # can also use tail()
```

tableby Object

Function Call: tableby(formula = arm ~ sex + alk.phos + age, data =
mockstudy)

Variable(s): arm ~ sex

``` r

summary(tableby(list(arm, sex) ~ sex + alk.phos + age, data=mockstudy)[, "sex"])
```

|              |   Male (N=916)    |  Female (N=583)   |  Total (N=1499)   | p value |
|:-------------|:-----------------:|:-----------------:|:-----------------:|--------:|
| **alk.phos** |                   |                   |                   |   0.712 |
|    N-Miss    |        162        |        104        |        266        |         |
|    Mean (SD) | 167.893 (130.754) | 170.664 (124.965) | 168.969 (128.492) |         |
|    Range     | 10.000 - 1014.000 |  7.000 - 771.000  | 7.000 - 1014.000  |         |
| **Age, yrs** |                   |                   |                   |   0.048 |
|    Mean (SD) |  60.455 (11.369)  |  59.247 (11.722)  |  59.985 (11.519)  |         |
|    Range     |  19.000 - 88.000  |  22.000 - 88.000  |  19.000 - 88.000  |         |

``` r

summary(tableby(list(arm, sex) ~ sex + alk.phos + age, data=mockstudy)[, list(sex = "Female", arm = c("F: FOLFOX", "Total"))])
```

|              |  Female (N=583)   | p value |
|:-------------|:-----------------:|--------:|
| **alk.phos** |                   |   0.712 |
|    N-Miss    |        104        |         |
|    Mean (SD) | 170.664 (124.965) |         |
|    Range     |  7.000 - 771.000  |         |
| **Age, yrs** |                   |   0.048 |
|    Mean (SD) |  59.247 (11.722)  |         |
|    Range     |  22.000 - 88.000  |         |

|              | F: FOLFOX (N=691) |  Total (N=1499)   | p value |
|:-------------|:-----------------:|:-----------------:|--------:|
| **Gender**   |                   |                   |   0.190 |
|    Male      |    411 (59.5%)    |    916 (61.1%)    |         |
|    Female    |    280 (40.5%)    |    583 (38.9%)    |         |
| **alk.phos** |                   |                   |   0.226 |
|    N-Miss    |        141        |        266        |         |
|    Mean (SD) | 161.984 (121.978) | 168.969 (128.492) |         |
|    Range     | 10.000 - 1014.000 | 7.000 - 1014.000  |         |
| **Age, yrs** |                   |                   |   0.614 |
|    Mean (SD) |  60.301 (11.632)  |  59.985 (11.519)  |         |
|    Range     |  19.000 - 88.000  |  19.000 - 88.000  |         |

### 11. Merge two `tableby` objects together

It is possible to combine two tableby objects so that they print out
together. Overlapping by-variables will have their x-variables
concatenated, and (if `all=TRUE`) non-overlapping by-variables will have
their tables printed separately.

``` r

## demographics
tab1 <- tableby(arm ~ sex + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss","meansd"), total=FALSE))
## lab data
tab2 <- tableby(arm ~ hgb + alk.phos, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss","median","q1q3"),
                                        numeric.test="kwt", total=FALSE))
tab12 <- merge(tab1, tab2)
class(tab12)
```

\[1\] “tableby” “arsenal_table”

``` r

summary(tab12)
```

|              | A: IFL (N=428)  | F: FOLFOX (N=691) | G: IROX (N=380) | p value |
|:-------------|:---------------:|:-----------------:|:---------------:|--------:|
| **Gender**   |                 |                   |                 |   0.190 |
|    Male      |   277 (64.7%)   |    411 (59.5%)    |   228 (60.0%)   |         |
|    Female    |   151 (35.3%)   |    280 (40.5%)    |   152 (40.0%)   |         |
| **Age, yrs** |                 |                   |                 |   0.614 |
|    Mean (SD) | 59.673 (11.365) |  60.301 (11.632)  | 59.763 (11.499) |         |
| **hgb**      |                 |                   |                 |   0.570 |
|    N-Miss    |       69        |        141        |       56        |         |
|    Median    |     12.100      |      12.200       |     12.400      |         |
|    Q1, Q3    | 11.000, 13.450  |  11.100, 13.600   | 11.175, 13.625  |         |
| **alk.phos** |                 |                   |                 |   0.104 |
|    N-Miss    |       69        |        141        |       56        |         |
|    Median    |     133.000     |      116.000      |     122.000     |         |
|    Q1, Q3    | 89.000, 217.000 |  85.000, 194.750  | 87.750, 210.250 |         |

For tables with two different outcomes, consider the `all=TRUE`
argument:

``` r

summary(merge(
  tableby(sex ~ age, data = mockstudy),
  tableby(arm ~ bmi, data = mockstudy),
  all = TRUE
))
```

|              |  Male (N=916)   | Female (N=583)  | Total (N=1499)  | p value |
|:-------------|:---------------:|:---------------:|:---------------:|--------:|
| **Age, yrs** |                 |                 |                 |   0.048 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |         |
|    Range     | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |         |

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.892 |
|    N-Miss | 9 | 20 | 4 | 33 |  |
|    Mean (SD) | 27.290 (5.552) | 27.210 (5.173) | 27.106 (5.751) | 27.206 (5.432) |  |
|    Range | 14.053 - 53.008 | 16.649 - 49.130 | 15.430 - 60.243 | 14.053 - 60.243 |  |

### 12. Add a title to the table

When creating a pdf the tables are automatically numbered and the title
appears below the table. In Word and HTML, the titles appear un-numbered
and above the table.

``` r

t1 <- tableby(arm ~ sex + age, data=mockstudy)
summary(t1, title='Demographics')
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

Demographics {.table style="width:100%;"}

With multiple left-hand sides, you can pass a vector or list to
determine labels for each table:

``` r

summary(tableby(list(arm, sex) ~ age, data = mockstudy), title = c("arm table", "sex table"))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) |  |
|    Range | 27.000 - 88.000 | 19.000 - 88.000 | 26.000 - 85.000 | 19.000 - 88.000 |  |

arm table {.table style="width:100%;"}

|              |  Male (N=916)   | Female (N=583)  | Total (N=1499)  | p value |
|:-------------|:---------------:|:---------------:|:---------------:|--------:|
| **Age, yrs** |                 |                 |                 |   0.048 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |         |
|    Range     | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |         |

sex table {.table}

### 13. Modify how missing values are displayed

Depending on the report you are writing you have the following options:

- Show how many subjects have each variable

- Show how many subjects are missing each variable

- Show how many subjects are missing each variable only if there are any
  missing values

- Don’t indicate missing values at all

``` r

## look at how many missing values there are for each variable
apply(is.na(mockstudy),2,sum)
```

    ##        case         age         arm         sex        race     fu.time     fu.stat          ps 
    ##           0           0           0           0           7           0           0         266 
    ##         hgb         bmi    alk.phos         ast mdquality.s     age.ord  age.ordnew     dtentry 
    ##         266          33         266         266         252           0           1           5

``` r

## Show how many subjects have each variable (non-missing)
summary(tableby(sex ~ ast + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("N","median"), total=FALSE)))
```

|              | Male (N=916) | Female (N=583) | p value |
|:-------------|:------------:|:--------------:|--------:|
| **ast**      |              |                |   0.921 |
|    N         |     754      |      479       |         |
|    Median    |    27.000    |     27.000     |         |
| **Age, yrs** |              |                |   0.048 |
|    N         |     916      |      583       |         |
|    Median    |    61.000    |     60.000     |         |

``` r

## Always list the number of missing values
summary(tableby(sex ~ ast + age, data=mockstudy,
                control=tableby.control(numeric.stats=c("Nmiss2","median"), total=FALSE)))
```

|              | Male (N=916) | Female (N=583) | p value |
|:-------------|:------------:|:--------------:|--------:|
| **ast**      |              |                |   0.921 |
|    N-Miss    |     162      |      104       |         |
|    Median    |    27.000    |     27.000     |         |
| **Age, yrs** |              |                |   0.048 |
|    N-Miss    |      0       |       0        |         |
|    Median    |    61.000    |     60.000     |         |

``` r

## Only show the missing values if there are some (default)
summary(tableby(sex ~ ast + age, data=mockstudy, 
                control=tableby.control(numeric.stats=c("Nmiss","mean"),total=FALSE)))
```

|              | Male (N=916) | Female (N=583) | p value |
|:-------------|:------------:|:--------------:|--------:|
| **ast**      |              |                |   0.921 |
|    N-Miss    |     162      |      104       |         |
|    Mean      |    35.873    |     36.029     |         |
| **Age, yrs** |              |                |   0.048 |
|    Mean      |    60.455    |     59.247     |         |

``` r

## Don't show N at all
summary(tableby(sex ~ ast + age, data=mockstudy, 
                control=tableby.control(numeric.stats=c("mean"),total=FALSE)))
```

|              | Male (N=916) | Female (N=583) | p value |
|:-------------|:------------:|:--------------:|--------:|
| **ast**      |              |                |   0.921 |
|    Mean      |    35.873    |     36.029     |         |
| **Age, yrs** |              |                |   0.048 |
|    Mean      |    60.455    |     59.247     |         |

One might also consider the use of
[`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
to include NAs in the counts and percents for categorical variables.

``` r

mockstudy$ps.cat <- factor(mockstudy$ps)
attr(mockstudy$ps.cat, "label") <- "ps"
summary(tableby(sex ~ includeNA(ps.cat), data = mockstudy, cat.stats = "countpct"))
```

|              | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:-------------|:------------:|:--------------:|:--------------:|--------:|
| **ps**       |              |                |                |   0.354 |
|    0         | 391 (42.7%)  |  244 (41.9%)   |  635 (42.4%)   |         |
|    1         | 329 (35.9%)  |  202 (34.6%)   |  531 (35.4%)   |         |
|    2         |  34 (3.7%)   |   33 (5.7%)    |   67 (4.5%)    |         |
|    (Missing) | 162 (17.7%)  |  104 (17.8%)   |  266 (17.7%)   |         |

### 14. Modify the number of digits used

Within tableby.control function there are 4 options for controlling the
number of significant digits shown.

- digits: controls the number of digits after the decimal place for
  continuous values

- digits.count: controls the number of digits after the decimal point
  for counts

- digits.pct: controls the number of digits after the decimal point for
  percents

- digits.p: controls the number of digits after the decimal point for
  p-values

``` r

summary(tableby(arm ~ sex + age + fu.time, data=mockstudy), digits=4, digits.p=2, digits.pct=1)
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.19 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.61 |
|    Mean (SD) | 59.6729 (11.3645) | 60.3010 (11.6323) | 59.7632 (11.4993) | 59.9853 (11.5188) |  |
|    Range | 27.0000 - 88.0000 | 19.0000 - 88.0000 | 26.0000 - 85.0000 | 19.0000 - 88.0000 |  |
| **fu.time** |  |  |  |  | \< 0.01 |
|    Mean (SD) | 553.5841 (419.6065) | 731.2460 (487.7443) | 607.2421 (435.5092) | 649.0841 (462.5109) |  |
|    Range | 9.0000 - 2170.0000 | 0.0000 - 2472.0000 | 17.0000 - 2118.0000 | 0.0000 - 2472.0000 |  |

All of these can be specified on a per-variable basis using the
in-formula functions that specify which tests are run:

``` r

summary(tableby(arm ~ chisq(sex, digits.pct=1) + anova(age, digits=4) +
                  anova(fu.time, digits = 1, digits.p = 6, format.p = FALSE), data=mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender** |  |  |  |  | 0.190 |
|    Male | 277 (64.7%) | 411 (59.5%) | 228 (60.0%) | 916 (61.1%) |  |
|    Female | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) |  |
| **Age, yrs** |  |  |  |  | 0.614 |
|    Mean (SD) | 59.6729 (11.3645) | 60.3010 (11.6323) | 59.7632 (11.4993) | 59.9853 (11.5188) |  |
|    Range | 27.0000 - 88.0000 | 19.0000 - 88.0000 | 26.0000 - 85.0000 | 19.0000 - 88.0000 |  |
| **fu.time** |  |  |  |  | 3.17796e-10 |
|    Mean (SD) | 553.6 (419.6) | 731.2 (487.7) | 607.2 (435.5) | 649.1 (462.5) |  |
|    Range | 9.0 - 2170.0 | 0.0 - 2472.0 | 17.0 - 2118.0 | 0.0 - 2472.0 |  |

### 15. Create a user-defined summary statistic

For purposes of this example, the code below creates a trimmed mean
function (trims 10%) and use that to summarize the data. Note the use of
the `...` which tells R to pass extra arguments on - this is required
for user-defined functions. In this case, `na.rm=T` is passed to
`myfunc`. The *weights* argument is also required, even though it isn’t
passed on to the internal function in this particular example.

``` r

trim10 <- function(x, weights=NULL, ...){
  mean(x, trim=.1, ...)
}

summary(tableby(sex ~ hgb, data=mockstudy, 
                control=tableby.control(numeric.stats=c("Nmiss","trim10"), numeric.test="kwt",
                    stats.labels=list(Nmiss='Missing values', trim10="Trimmed Mean, 10%"))))
```

|                      | Male (N=916) | Female (N=583) | Total (N=1499) |  p value |
|:---------------------|:------------:|:--------------:|:--------------:|---------:|
| **hgb**              |              |                |                | \< 0.001 |
|    Missing values    |     162      |      104       |      266       |          |
|    Trimmed Mean, 10% |     12.6     |      11.9      |      12.3      |          |

For statistics to be formatted appropriately, you may want to use
[`as.tbstat()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md).
For example, suppose you want to create a trimmed mean function that
trims by both 5 and 10 percent. The first example shows them separated
by a comma; the second puts the 10% trimmed mean in brackets.
[`as.tbstat()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md)
takes a `fmt=` argument with a `glue` string specification, where the
current value is exposed as `x` and a formatted value (using
[`tbfmt()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md))
is exposed as `y`. See
[`?as.tbstat`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md)
for details.

``` r

trim510comma <- function(x, ...){
  tmp <- c(mean(x, trim = 0.05, ...), mean(x, trim = 0.1, ...))
  as.tbstat(tmp, fmt = "{y[1]}, {y[2]}")
}
trim510bracket <- function(x, ...){
  tmp <- c(mean(x, trim = 0.05, ...), mean(x, trim = 0.1, ...))
  as.tbstat(tmp, fmt = "{y[1]} [{y[2]}]")
}

summary(tableby(sex ~ hgb, data=mockstudy, numeric.stats=c("Nmiss", "trim510comma", "trim510bracket"), test = FALSE))
```

|                   |   Male (N=916)    |  Female (N=583)   |  Total (N=1499)   |
|:------------------|:-----------------:|:-----------------:|:-----------------:|
| **hgb**           |                   |                   |                   |
|    N-Miss         |        162        |        104        |        266        |
|    trim510comma   |  12.570, 12.564   |  11.924, 11.910   |  12.308, 12.291   |
|    trim510bracket | 12.570 \[12.564\] | 11.924 \[11.910\] | 12.308 \[12.291\] |

Or perhaps it’s useful to put the amount of trimming in parentheses.
Since it is a percent, we can flag it as such:

``` r

trim10pct <- function(x, ...){
  tmp <- mean(x, trim = 0.05, ...)
  as.tbstat(c(tmp, 10), fmt = "{y[1]} ({y[2]}%)", which.pct = 2L)
}
summary(tableby(sex ~ hgb, data=mockstudy, numeric.stats=c("Nmiss", "trim10pct"),
                digits = 2, digits.pct = 0, test = FALSE))
```

|              | Male (N=916) | Female (N=583) | Total (N=1499) |
|:-------------|:------------:|:--------------:|:--------------:|
| **hgb**      |              |                |                |
|    N-Miss    |     162      |      104       |      266       |
|    trim10pct | 12.57 (10%)  |  11.92 (10%)   |  12.31 (10%)   |

Finally, if there’s a pre-computed summary statistic, you can easily
change the formatting, like in
[`meanpmsd()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md):

``` r

meanpmsd
```

    ## function (x, na.rm = TRUE, weights = NULL, ...) 
    ## {
    ##     if (na.rm && allNA(x)) {
    ##         as.tbstat(NA_real_)
    ##     }
    ##     else {
    ##         y <- meansd(x = x, na.rm = na.rm, weights = weights, 
    ##             ...)
    ##         attr(y, "fmt") <- "{y[1]} &pm; {y[2]}"
    ##         y
    ##     }
    ## }
    ## <bytecode: 0x11153300>
    ## <environment: namespace:arsenal>

For example:

``` r

dollarmean <- function(x, na.rm = TRUE, ...) {
  if(na.rm && allNA(x)) {
    as.tbstat(NA_real_)
  } else {
    out <- meansd(x, na.rm = na.rm, ...)
    attr(out, "fmt") <- "${y[1]}"
    out
  }
}

summary(tableby(sex ~ notest(age, "dollarmean", digits = 2), data = mockstudy))
```

    ## 
    ## 
    ## |                             | Male (N=916) | Female (N=583) | Total (N=1499) | p value|
    ## |:----------------------------|:------------:|:--------------:|:--------------:|-------:|
    ## |**Age, yrs**                 |              |                |                |        |
    ## |&nbsp;&nbsp;&nbsp;dollarmean |    $60.46    |     $59.25     |     $59.99     |        |

### 16. Use case-weights for creating summary statistics

When comparing groups, they are often unbalanced when it comes to
nuisances such as age and sex. The `tableby` function allows you to
create weighted summary statistics. If this option us used then p-values
are not calculated (`test=FALSE`).

``` r

##create fake group that is not balanced by age/sex 
set.seed(200)
mockstudy$fake_arm <- ifelse(mockstudy$age>60 & mockstudy$sex=='Female',sample(c('A','B'),replace=T, prob=c(.2,.8)),
                            sample(c('A','B'),replace=T, prob=c(.8,.4)))

mockstudy$agegp <- cut(mockstudy$age, breaks=c(18,50,60,70,90), right=FALSE)

## create weights based on agegp and sex distribution
tab1 <- with(mockstudy,table(agegp, sex))
tab2 <- with(mockstudy, table(agegp, sex, fake_arm))
tab2
```

    ## , , fake_arm = A
    ## 
    ##          sex
    ## agegp     Male Female
    ##   [18,50)   73     62
    ##   [50,60)  128     94
    ##   [60,70)  139      7
    ##   [70,90)  102      0
    ## 
    ## , , fake_arm = B
    ## 
    ##          sex
    ## agegp     Male Female
    ##   [18,50)   79     48
    ##   [50,60)  130     84
    ##   [60,70)  156    166
    ##   [70,90)  109    122

``` r

gpwts <- rep(tab1, length(unique(mockstudy$fake_arm)))/tab2
gpwts[gpwts>50] <- 30

## apply weights to subjects
index <- with(mockstudy, cbind(as.numeric(agegp), as.numeric(sex), as.numeric(as.factor(fake_arm)))) 
mockstudy$wts <- gpwts[index]

## show weights by treatment arm group
tapply(mockstudy$wts,mockstudy$fake_arm, summary)
```

    ## $A
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.774   1.894   2.069   2.276   2.082  24.714 
    ## 
    ## $B
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.042   1.924   1.677   1.985   2.292

``` r

orig <- tableby(fake_arm ~ age + sex + Surv(fu.time/365, fu.stat), data=mockstudy, test=FALSE)
summary(orig, title='No Case Weights used')
```

|  | A (N=605) | B (N=894) | Total (N=1499) |
|:---|:--:|:--:|:--:|
| **Age, yrs** |  |  |  |
|    Mean (SD) | 57.413 (11.618) | 61.726 (11.125) | 59.985 (11.519) |
|    Range | 22.000 - 85.000 | 19.000 - 88.000 | 19.000 - 88.000 |
| **Gender** |  |  |  |
|    Male | 442 (73.1%) | 474 (53.0%) | 916 (61.1%) |
|    Female | 163 (26.9%) | 420 (47.0%) | 583 (38.9%) |
| **Surv(fu.time/365, fu.stat)** |  |  |  |
|    Events | 554 | 802 | 1356 |
|    Median Survival | 1.504 | 1.493 | 1.496 |

No Case Weights used {.table}

``` r

tab1 <- tableby(fake_arm ~ age + sex + Surv(fu.time/365, fu.stat), data=mockstudy, weights=wts)
summary(tab1, title='Case Weights used')
```

|  | A (N=1377) | B (N=1499) | Total (N=2876) |
|:---|:--:|:--:|:--:|
| **Age, yrs** |  |  |  |
|    Mean (SD) | 58.009 (10.925) | 60.151 (11.428) | 59.126 (11.235) |
|    Range | 22.000 - 85.000 | 19.000 - 88.000 | 19.000 - 88.000 |
| **Gender** |  |  |  |
|    Male | 916 (66.5%) | 916 (61.1%) | 1832 (63.7%) |
|    Female | 461 (33.5%) | 583 (38.9%) | 1044 (36.3%) |
| **Surv(fu.time/365, fu.stat)** |  |  |  |
|    Events | 1252 | 1348 | 2599 |
|    Median Survival | 1.534 | 1.496 | 1.532 |

Case Weights used {.table}

### 17. Create your own p-value and add it to the table

When using weighted summary statistics, it is often desirable to then
show a p-value from a model that corresponds to the weighted analysis.
It is possible to add your own p-value and modify the column title for
that new p-value. Another use for this would be to add standardized
differences or confidence intervals instead of a p-value.

To add the p-value, you simply need to create a data frame and use the
function
[`modpval.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md).
The first few columns in the data.frame are required: (1) the
by-variable, (2) the strata value (if the table has a strata term), (3)
the x-variable, and (4) the new p-value (or test statistic). Another
optional column can be used to indicate what method was used to
calculate the p-value. If you specify `use.pname=TRUE` then the column
name indicating the p-value will be also be used in the tableby summary.

``` r

mypval <- data.frame(
  byvar = "fake_arm",
  variable = c('age','sex','Surv(fu.time/365, fu.stat)'), 
  adj.pvalue = c(.953,.811,.01), 
  method = c('Age/Sex adjusted model results')
)
tab2 <- modpval.tableby(tab1, mypval, use.pname=TRUE)
summary(tab2, title='Case Weights used, p-values added', pfootnote=TRUE)
```

|  | A (N=1377) | B (N=1499) | Total (N=2876) | adj.pvalue |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.953¹ |
|    Mean (SD) | 58.009 (10.925) | 60.151 (11.428) | 59.126 (11.235) |  |
|    Range | 22.000 - 85.000 | 19.000 - 88.000 | 19.000 - 88.000 |  |
| **Gender** |  |  |  | 0.811¹ |
|    Male | 916 (66.5%) | 916 (61.1%) | 1832 (63.7%) |  |
|    Female | 461 (33.5%) | 583 (38.9%) | 1044 (36.3%) |  |
| **Surv(fu.time/365, fu.stat)** |  |  |  | 0.010¹ |
|    Events | 1252 | 1348 | 2599 |  |
|    Median Survival | 1.534 | 1.496 | 1.532 |  |

Case Weights used, p-values added {.table}

1.  Age/Sex adjusted model results

### 18. For two-level categorical variables or one-line numeric variables, simplify the output.

If the `cat.simplify` option is set to `TRUE`, then only the second
level of two-level categorical varialbes is shown. In the example below,
`sex` has two levels, and “Female” is the second level, hence only the
counts and percents for Female are shown. Similarly, “mdquality.s” was
turned to a factor, and “1” is the second level, but since there are
missings, the table ignores `cat.simplify` and displays all levels
(since the output can no longer be displayed on one line).

``` r

table2 <- tableby(arm~sex + factor(mdquality.s), data=mockstudy, cat.simplify=TRUE)
summary(table2, labelTranslations=c(sex="Female", "factor(mdquality.s)"="MD Quality"))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Female** | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) | 0.190 |
| **MD Quality** |  |  |  |  | 0.694 |
|    N-Miss | 55 | 156 | 41 | 252 |  |
|    0 | 41 (11.0%) | 52 (9.7%) | 31 (9.1%) | 124 (9.9%) |  |
|    1 | 332 (89.0%) | 483 (90.3%) | 308 (90.9%) | 1123 (90.1%) |  |

Similarly, if `numeric.simplify` is set to `TRUE`, then any numerics
which only have one row of summary statistics are simplified into a
single row. Note again that `ast` has missing values and so is not
simplified to a single row.

``` r

summary(tableby(arm ~ age + ast, data = mockstudy,
                numeric.simplify=TRUE, numeric.stats=c("Nmiss", "meansd")))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) | 0.614 |
| **ast** |  |  |  |  | 0.507 |
|    N-Miss | 69 | 141 | 56 | 266 |  |
|    Mean (SD) | 37.292 (28.036) | 35.202 (26.659) | 35.670 (25.807) | 35.933 (26.843) |  |

The in-formula functions to change which tests are run can also be used
to specify these options for each variable at a time.

``` r

summary(tableby(arm ~ anova(age, "meansd", numeric.simplify=TRUE) +
                  chisq(sex, cat.simplify=TRUE), data = mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** | 59.673 (11.365) | 60.301 (11.632) | 59.763 (11.499) | 59.985 (11.519) | 0.614 |
| **Gender** | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) | 0.190 |

The `cat.simplify` and `ord.simplify` argument also accept the special
string `"label"`, which appends the shown level’s label to the overall
label:

``` r

summary(tableby(arm ~ sex, cat.simplify = "label", data = mockstudy))
```

|  | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Gender (Female)** | 151 (35.3%) | 280 (40.5%) | 152 (40.0%) | 583 (38.9%) | 0.190 |

### 19. Use `tableby` within an Sweave document

For those users who wish to create tables within an Sweave document, the
following code seems to work.

    \documentclass{article}

    \usepackage{longtable}
    \usepackage{pdfpages}

    \begin{document}

    \section{Read in Data}
    <<echo=TRUE>>=
    require(arsenal)
    require(knitr)
    require(rmarkdown)
    data(mockstudy)

    tab1 <- tableby(arm~sex+age, data=mockstudy)
    @

    \section{Convert Summary.Tableby to LaTeX}
    <<echo=TRUE, results='hide', message=FALSE>>=
    capture.output(summary(tab1), file="Test.md")

    ## Convert R Markdown Table to LaTeX
    render("Test.md", pdf_document(keep_tex=TRUE))
    @ 

    \includepdf{Test.pdf}

    \end{document}

### 20. Export `tableby` object to a .CSV file

When looking at multiple variables it is sometimes useful to export the
results to a csv file. The `as.data.frame` function creates a data frame
object that can be exported or further manipulated within R.

``` r

tab1 <- summary(tableby(arm~sex+age, data=mockstudy), text = NULL)
as.data.frame(tab1)
```

    ##              A: IFL (N=428) F: FOLFOX (N=691) G: IROX (N=380)  Total (N=1499) p value
    ## 1    Gender                                                                     0.190
    ## 2      Male     277 (64.7%)       411 (59.5%)     228 (60.0%)     916 (61.1%)        
    ## 3    Female     151 (35.3%)       280 (40.5%)     152 (40.0%)     583 (38.9%)        
    ## 4  Age, yrs                                                                     0.614
    ## 5 Mean (SD) 59.673 (11.365)   60.301 (11.632) 59.763 (11.499) 59.985 (11.519)        
    ## 6     Range 27.000 - 88.000   19.000 - 88.000 26.000 - 85.000 19.000 - 88.000

``` r

# write.csv(tab1, '/my/path/here/my_table.csv')
```

### 21. Write `tableby` object to a separate Word or HTML file

``` r

## write to an HTML document
tab1 <- tableby(arm ~ sex + age, data=mockstudy)
write2html(tab1, "~/trash.html")

## write to a Word document
write2word(tab1, "~/trash.doc", title="My table in Word")
```

### 22. Use `tableby` in R Shiny

The easiest way to output a
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
object in an R Shiny app is to use the
[`tableOutput()`](https://rdrr.io/pkg/shiny/man/renderTable.html) UI in
combination with the
[`renderTable()`](https://rdrr.io/pkg/shiny/man/renderTable.html) server
function and `as.data.frame(summary(tableby()))`:

``` r

# A standalone shiny app
library(shiny)
library(arsenal)
data(mockstudy)

shinyApp(
  ui = fluidPage(tableOutput("table")),
  server = function(input, output) {
    output$table <- renderTable({
      as.data.frame(summary(tableby(sex ~ age, data = mockstudy), text = "html"))
    }, sanitize.text.function = function(x) x)
  }
)
```

This can be especially powerful if you feed the selections from a
`selectInput(multiple = TRUE)` into
[`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md)
to make the table dynamic!

### 23. Use `tableby` in bookdown

Since the backbone of
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
is [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html), tables
still render well in bookdown. However, `print.summary.tableby()`
doesn’t use the `caption=` argument of
[`kable()`](https://rdrr.io/pkg/knitr/man/kable.html), so some tables
may not have a properly numbered caption. To fix this, use the method
described [on the bookdown
site](https://bookdown.org/yihui/bookdown/tables.html) to give the table
a tag/ID.

``` r

summary(tableby(sex ~ age, data = mockstudy), title="(\\#tab:mytableby) Caption here")
```

### 24. Adjust `tableby` for multiple p-values

The
[`padjust()`](https://mayoverse.github.io/arsenal/reference/padjust.md)
function is a new S3 generic piggybacking off of
[`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html). It works on both
`tableby` and `summary.tableby` objects:

``` r

tab <- summary(tableby(sex ~ age + fu.time + bmi + mdquality.s, data = mockstudy))
tab
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.048 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |  |
|    Range | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |  |
| **fu.time** |  |  |  | 0.978 |
|    Mean (SD) | 649.345 (454.332) | 648.674 (475.472) | 649.084 (462.511) |  |
|    Range | 0.000 - 2472.000 | 9.000 - 2441.000 | 0.000 - 2472.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.012 |
|    N-Miss | 22 | 11 | 33 |  |
|    Mean (SD) | 27.491 (5.030) | 26.760 (5.984) | 27.206 (5.432) |  |
|    Range | 14.053 - 60.243 | 15.430 - 53.008 | 14.053 - 60.243 |  |
| **mdquality.s** |  |  |  | 0.827 |
|    N-Miss | 153 | 99 | 252 |  |
|    Mean (SD) | 0.899 (0.301) | 0.903 (0.296) | 0.901 (0.299) |  |
|    Range | 0.000 - 1.000 | 0.000 - 1.000 | 0.000 - 1.000 |  |

``` r

padjust(tab, method = "bonferroni")
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.191 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |  |
|    Range | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |  |
| **fu.time** |  |  |  | 1.000 |
|    Mean (SD) | 649.345 (454.332) | 648.674 (475.472) | 649.084 (462.511) |  |
|    Range | 0.000 - 2472.000 | 9.000 - 2441.000 | 0.000 - 2472.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.048 |
|    N-Miss | 22 | 11 | 33 |  |
|    Mean (SD) | 27.491 (5.030) | 26.760 (5.984) | 27.206 (5.432) |  |
|    Range | 14.053 - 60.243 | 15.430 - 53.008 | 14.053 - 60.243 |  |
| **mdquality.s** |  |  |  | 1.000 |
|    N-Miss | 153 | 99 | 252 |  |
|    Mean (SD) | 0.899 (0.301) | 0.903 (0.296) | 0.901 (0.299) |  |
|    Range | 0.000 - 1.000 | 0.000 - 1.000 | 0.000 - 1.000 |  |

### 25. Tabulate multiple endpoints

You can now use [`list()`](https://rdrr.io/r/base/list.html) on the
left-hand side of
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
to give multiple endpoints.

``` r

summary(tableby(list(sex, mdquality.s, ps) ~ age + bmi, data = mockstudy))
```

|  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.048 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |  |
|    Range | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.012 |
|    N-Miss | 22 | 11 | 33 |  |
|    Mean (SD) | 27.491 (5.030) | 26.760 (5.984) | 27.206 (5.432) |  |
|    Range | 14.053 - 60.243 | 15.430 - 53.008 | 14.053 - 60.243 |  |

|  | 0 (N=124) | 1 (N=1123) | Total (N=1247) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.766 |
|    Mean (SD) | 60.089 (11.627) | 59.763 (11.537) | 59.796 (11.542) |  |
|    Range | 29.000 - 82.000 | 19.000 - 88.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.225 |
|    N-Miss | 3 | 18 | 21 |  |
|    Mean (SD) | 26.684 (6.331) | 27.309 (5.274) | 27.247 (5.388) |  |
|    Range | 16.071 - 60.243 | 14.053 - 53.008 | 14.053 - 60.243 |  |

|  | 0 (N=635) | 1 (N=531) | 2 (N=67) | Total (N=1233) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.335 |
|    Mean (SD) | 59.935 (11.261) | 60.800 (11.721) | 59.254 (12.090) | 60.271 (11.507) |  |
|    Range | 22.000 - 85.000 | 26.000 - 88.000 | 28.000 - 80.000 | 22.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.028 |
|    N-Miss | 7 | 20 | 1 | 28 |  |
|    Mean (SD) | 27.539 (5.222) | 26.842 (5.436) | 26.178 (5.808) | 27.169 (5.358) |  |
|    Range | 14.053 - 48.384 | 15.430 - 60.243 | 16.071 - 44.922 | 14.053 - 60.243 |  |

To avoid confusion about which table is which endpoint, you can set
`term.name=TRUE` in [`summary()`](https://rdrr.io/r/base/summary.html).
This takes the labels for each by-variable and puts them in the top-left
of the table.

``` r

summary(tableby(list(sex, mdquality.s, ps) ~ age + bmi, data = mockstudy), term.name = TRUE)
```

| Gender | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.048 |
|    Mean (SD) | 60.455 (11.369) | 59.247 (11.722) | 59.985 (11.519) |  |
|    Range | 19.000 - 88.000 | 22.000 - 88.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.012 |
|    N-Miss | 22 | 11 | 33 |  |
|    Mean (SD) | 27.491 (5.030) | 26.760 (5.984) | 27.206 (5.432) |  |
|    Range | 14.053 - 60.243 | 15.430 - 53.008 | 14.053 - 60.243 |  |

| mdquality.s | 0 (N=124) | 1 (N=1123) | Total (N=1247) | p value |
|:---|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  | 0.766 |
|    Mean (SD) | 60.089 (11.627) | 59.763 (11.537) | 59.796 (11.542) |  |
|    Range | 29.000 - 82.000 | 19.000 - 88.000 | 19.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  | 0.225 |
|    N-Miss | 3 | 18 | 21 |  |
|    Mean (SD) | 26.684 (6.331) | 27.309 (5.274) | 27.247 (5.388) |  |
|    Range | 16.071 - 60.243 | 14.053 - 53.008 | 14.053 - 60.243 |  |

| ps | 0 (N=635) | 1 (N=531) | 2 (N=67) | Total (N=1233) | p value |
|:---|:--:|:--:|:--:|:--:|---:|
| **Age, yrs** |  |  |  |  | 0.335 |
|    Mean (SD) | 59.935 (11.261) | 60.800 (11.721) | 59.254 (12.090) | 60.271 (11.507) |  |
|    Range | 22.000 - 85.000 | 26.000 - 88.000 | 28.000 - 80.000 | 22.000 - 88.000 |  |
| **Body Mass Index (kg/m^2)** |  |  |  |  | 0.028 |
|    N-Miss | 7 | 20 | 1 | 28 |  |
|    Mean (SD) | 27.539 (5.222) | 26.842 (5.436) | 26.178 (5.808) | 27.169 (5.358) |  |
|    Range | 14.053 - 48.384 | 15.430 - 60.243 | 16.071 - 44.922 | 14.053 - 60.243 |  |

### 26. Tabulate data by a non-test group (strata)

You can also specify a second grouping variable that doesn’t get tested
(but instead separates results): a *strata* variable.

``` r

summary(tableby(list(sex, ps) ~ age + bmi, strata = arm, data = mockstudy))
```

| Treatment Arm |  | Male (N=916) | Female (N=583) | Total (N=1499) | p value |
|:---|:---|:--:|:--:|:--:|:--:|
| A: IFL | **Age, yrs** |  |  |  | 0.572 |
|  |    Mean (SD) | 59.903 (11.347) | 59.252 (11.422) | 59.673 (11.365) |  |
|  |    Range | 28.000 - 83.000 | 27.000 - 88.000 | 27.000 - 88.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  | 0.050 |
|  |    N-Miss | 7 | 2 | 9 |  |
|  |    Mean (SD) | 27.685 (5.072) | 26.575 (6.287) | 27.290 (5.552) |  |
|  |    Range | 14.053 - 48.384 | 16.880 - 53.008 | 14.053 - 53.008 |  |
| F: FOLFOX | **Age, yrs** |  |  |  | 0.286 |
|  |    Mean (SD) | 60.691 (11.598) | 59.729 (11.679) | 60.301 (11.632) |  |
|  |    Range | 19.000 - 88.000 | 22.000 - 83.000 | 19.000 - 88.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  | 0.768 |
|  |    N-Miss | 12 | 8 | 20 |  |
|  |    Mean (SD) | 27.259 (4.715) | 27.139 (5.789) | 27.210 (5.173) |  |
|  |    Range | 17.927 - 47.458 | 16.649 - 49.130 | 16.649 - 49.130 |  |
| G: IROX | **Age, yrs** |  |  |  | 0.051 |
|  |    Mean (SD) | 60.702 (10.999) | 58.355 (12.113) | 59.763 (11.499) |  |
|  |    Range | 29.000 - 85.000 | 26.000 - 82.000 | 26.000 - 85.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  | 0.020 |
|  |    N-Miss | 3 | 1 | 4 |  |
|  |    Mean (SD) | 27.672 (5.505) | 26.262 (6.021) | 27.106 (5.751) |  |
|  |    Range | 17.377 - 60.243 | 15.430 - 45.354 | 15.430 - 60.243 |  |

| Treatment Arm |  | 0 (N=635) | 1 (N=531) | 2 (N=67) | Total (N=1233) | p value |
|:---|:---|:--:|:--:|:--:|:--:|:--:|
| A: IFL | **Age, yrs** |  |  |  |  | 0.413 |
|  |    Mean (SD) | 60.101 (10.948) | 60.579 (12.026) | 56.842 (13.226) | 60.131 (11.535) |  |
|  |    Range | 27.000 - 81.000 | 28.000 - 88.000 | 34.000 - 75.000 | 27.000 - 88.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  |  | 0.023 |
|  |    N-Miss | 1 | 6 | 1 | 8 |  |
|  |    Mean (SD) | 27.850 (5.318) | 26.224 (5.347) | 26.954 (5.560) | 27.128 (5.385) |  |
|  |    Range | 14.053 - 48.384 | 17.029 - 53.008 | 17.177 - 37.223 | 14.053 - 53.008 |  |
| F: FOLFOX | **Age, yrs** |  |  |  |  | 0.272 |
|  |    Mean (SD) | 60.173 (11.096) | 61.342 (11.918) | 63.138 (9.303) | 60.845 (11.391) |  |
|  |    Range | 22.000 - 82.000 | 26.000 - 88.000 | 44.000 - 80.000 | 22.000 - 88.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  |  | 0.225 |
|  |    N-Miss | 5 | 11 | 0 | 16 |  |
|  |    Mean (SD) | 27.569 (5.004) | 27.192 (5.248) | 25.904 (5.338) | 27.315 (5.134) |  |
|  |    Range | 16.649 - 43.867 | 16.799 - 49.130 | 20.833 - 44.922 | 16.649 - 49.130 |  |
| G: IROX | **Age, yrs** |  |  |  |  | 0.312 |
|  |    Mean (SD) | 59.361 (11.904) | 60.081 (11.037) | 55.737 (13.523) | 59.451 (11.653) |  |
|  |    Range | 26.000 - 85.000 | 28.000 - 84.000 | 28.000 - 76.000 | 26.000 - 85.000 |  |
|  | **Body Mass Index (kg/m^2)** |  |  |  |  | 0.642 |
|  |    N-Miss | 1 | 3 | 0 | 4 |  |
|  |    Mean (SD) | 27.143 (5.462) | 26.910 (5.824) | 25.861 (6.890) | 26.970 (5.694) |  |
|  |    Range | 17.615 - 46.204 | 15.430 - 60.243 | 16.071 - 44.734 | 15.430 - 60.243 |  |

## Available Function Options

### Summary statistics

The **default** summary statistics, by varible type, are:

- `numeric.stats`: Continuous variables will show by default
  `Nmiss, meansd, range`
- `cat.stats`: Categorical and factor variables will show by default
  `Nmiss, countpct`
- `ordered.stats`: Ordered factors will show by default
  `Nmiss, countpct`
- `surv.stats`: Survival variables will show by default
  `Nmiss, Nevents, medsurv`
- `date.stats`: Date variables will show by default
  `Nmiss, median, range`

There are a number of extra functions defined specifically for the
tableby function.

- `N`: a count of the non-missing number of observations for a
  particular group
- `Npct`: a count of the non-missing number of observations and the
  percentage of the column total (missing + non-missing) in the format
  `N (%)`
- `Nrowpct`: a count of the non-missing number of observations and the
  row-percentage (of non-missings) in the format `N (%)`
- `Nmiss`: only show the count of the number of missing values if there
  are some missing values
- `Nmiss2`: always show a count of the number of missing values for a
  variable within each group
- `Nmisspct`: show the count of the number of missing values and the
  percentage of the column total (missing+non-missing) if there are some
  missing values
- `Nmisspct2`: The same as `Nmisspct`, but always show.
- `meanse`: print the mean and standard error in the format `mean(se)`
- `meanCI`: print the mean and a (t) confidence interval
- `count`: print the number of values in a category
- `countN`: print the number of values in a category plus the total N
  for the group in the format `N/Total`
- `countpct`: print the number of values in a category plus the
  column-percentage in the format `N (%)`
- `pct`: print the column-percentage
- `countrowpct`: print the number of values in a category plus the
  row-percentage in the format `N (%)`
- `rowpct`: print the row-percentage
- `countcellpct`: print the number of values in a category plus the
  cell-percentage in the format `N (%)`
- `binomCI`: print the proportion in a category plus a binomial
  confidence interval.
- `rowbinomCI`: print the row proportion in a category plus a binomial
  confidence interval.
- `medianq1q3`: print the median, 25th, and 75th quantiles
  `median (Q1, Q3)`
- `q1q3`: print the 25th and 75th quantiles `Q1, Q3`
- `iqr`: print the inter-quartile range.
- `medianrange`: print the median, minimum and maximum values
  `median (minimum, maximum)`
- `medianmad`: print the median and median absolute deviation (mad)
- `Nevents`: print number of events for a survival object within each
  grouping level
- `medSurv`: print the median survival
- `NeventsSurv`: print number of events and survival at given times
- `NriskSurv`: print the number still at risk and survival at given
  times
- `Nrisk`: print the number still at risk at given times
- `medTime`: print the median follow-up time
- `sum`
- `max`
- `min`
- `mean`
- `sd`
- `var`
- `median`
- `range`
- `gmean`, `gsd`, `gmeansd`, `gmeanCI`: geometric means, sds, and
  confidence intervals.

### Testing options

The tests used to calculate p-values differ by the variable type, but
can be specified explicitly in the formula statement or in the control
function.

The following tests are accepted:

- `anova`: analysis of variance test; the default test for continuous
  variables. When the grouping variable has two levels, it is equivalent
  to the two-sample t-test with equal variance.

- `kwt`: Kruskal-Wallis test, optional test for continuous variables.
  When the grouping variable has two levels, it is equivalent to the
  Wilcoxon Rank Sum test.

- `wt`: An explicit Wilcoxcon test.

- `medtest`: Median test test, optional test for continuous variables.

- `chisq`: chi-square goodness of fit test or Pearson chi-squared test;
  the default for categorical or factor variables

- `fe`: Fisher’s exact test for categorical variables; optional

- `logrank`: log-rank test, the default test for time-to-event variables

- `trend`: The `independence_test` function from the `coin` is used to
  test for trends. Whenthe grouping variable has two levels, it is
  equivalent to the Armitage trend test. This is the default for ordered
  factors

- `stddiff`: perform standardized differences.

- `notest`: Don’t perform a test.

### `tableby.control` settings

A quick way to see what arguments are possible to utilize in a function
is to use the [`args()`](https://rdrr.io/r/base/args.html) command.
Settings involving the number of digits can be set in `tableby.control`
or in `summary.tableby`.

``` r

args(tableby.control)
```

    ## function (test = TRUE, total = TRUE, total.pos = c("after", "before"), 
    ##     test.pname = NULL, numeric.simplify = FALSE, cat.simplify = FALSE, 
    ##     cat.droplevels = FALSE, ordered.simplify = FALSE, date.simplify = FALSE, 
    ##     numeric.test = "anova", cat.test = "chisq", ordered.test = "trend", 
    ##     surv.test = "logrank", date.test = "kwt", selectall.test = "notest", 
    ##     test.always = FALSE, numeric.stats = c("Nmiss", "meansd", 
    ##         "range"), cat.stats = c("Nmiss", "countpct"), ordered.stats = c("Nmiss", 
    ##         "countpct"), surv.stats = c("Nmiss", "Nevents", "medSurv"), 
    ##     date.stats = c("Nmiss", "median", "range"), selectall.stats = c("Nmiss", 
    ##         "countpct"), stats.labels = list(), digits = 3L, digits.count = 0L, 
    ##     digits.pct = 1L, digits.p = 3L, format.p = TRUE, digits.n = 0L, 
    ##     conf.level = 0.95, wilcox.correct = FALSE, wilcox.exact = NULL, 
    ##     chisq.correct = FALSE, simulate.p.value = FALSE, B = 2000, 
    ##     times = 1:5, ...) 
    ## NULL

### `summary.tableby` settings

The `summary.tableby` function has options that modify how the table
appears (such as adding a title or modifying labels).

``` r

args(arsenal:::summary.tableby)
```

    ## function (object, ..., labelTranslations = NULL, text = FALSE, 
    ##     title = NULL, pfootnote = FALSE, term.name = "") 
    ## NULL
