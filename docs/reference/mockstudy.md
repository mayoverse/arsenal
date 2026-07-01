# Mock study data for examples

Mock clinical study data for examples to test data manipulation and
statistical functions. The function `muck_up_mockstudy()` is used in
examples for
[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md).

## Usage

``` r
mockstudy

muck_up_mockstudy()
```

## Format

A data frame with 1499 observations on the following 15 variables:

- `case`:

  a numeric identifier-patient ID

- `age`:

  age in years

- `arm`:

  treatment arm divided into 3 groups, character string

- `sex`:

  a factor with levels `Male` `Female`

- `race`:

  self-reported race/ethnicity, character string

- `fu.time`:

  survival or censoring time in years

- `fu.stat`:

  censoring status; 1=censor, 2=death

- `ps`:

  integer, ECOG performance score

- `hgb`:

  numeric, hemoglobin count

- `bmi`:

  numeric, body mass index, kg/m^2

- `alk.phos`:

  numeric, alkaline phosphatase

- `ast`:

  numeric, aspartate transaminase

- `mdquality.s`:

  integer, LASA QOL 0=Clinically Deficient, 1=Not Clinically Deficient

- `age.ord`:

  an ordered factor split of age, with levels `10-19` \< `20-29` \<
  `30-39` \< `40-49` \< `50-59` \< `60-69` \< `70-79` \< `80-89`

An object of class `data.frame` with 1499 rows and 14 columns.

## Examples

``` r
data(mockstudy)
str(mockstudy)
#> 'data.frame':    1499 obs. of  14 variables:
#>  $ case       : int  110754 99706 105271 105001 112263 86205 99508 90158 88989 90515 ...
#>  $ age        : int  67 74 50 71 69 56 50 57 51 63 ...
#>   ..- attr(*, "label")= chr "Age in Years"
#>  $ arm        : chr  "F: FOLFOX" "A: IFL" "A: IFL" "G: IROX" ...
#>   ..- attr(*, "label")= chr "Treatment Arm"
#>  $ sex        : Factor w/ 2 levels "Male","Female": 1 2 2 2 2 1 1 1 2 1 ...
#>  $ race       : chr  "Caucasian" "Caucasian" "Caucasian" "Caucasian" ...
#>   ..- attr(*, "label")= chr "Race"
#>  $ fu.time    : int  922 270 175 128 233 120 369 421 387 363 ...
#>  $ fu.stat    : int  2 2 2 2 2 2 2 2 2 2 ...
#>  $ ps         : int  0 1 1 1 0 0 0 0 1 1 ...
#>  $ hgb        : num  11.5 10.7 11.1 12.6 13 10.2 13.3 12.1 13.8 12.1 ...
#>  $ bmi        : num  25.1 19.5 NA 29.4 26.4 ...
#>   ..- attr(*, "label")= chr "Body Mass Index (kg/m^2)"
#>  $ alk.phos   : int  160 290 700 771 350 569 162 152 231 492 ...
#>  $ ast        : int  35 52 100 68 35 27 16 12 25 18 ...
#>  $ mdquality.s: int  NA 1 1 1 NA 1 1 1 1 1 ...
#>  $ age.ord    : Ord.factor w/ 8 levels "10-19"<"20-29"<..: 6 7 4 7 6 5 4 5 5 6 ...
```
