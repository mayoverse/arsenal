# Convert numeric dates to Date object, and vice versa

Convert numeric dates for month, day, and year to Date object, and vice
versa.

## Usage

``` r
mdy.Date(month, day, year, yearcut = 120)

Date.mdy(date)

is.Date(x)
```

## Arguments

- month:

  integer, month (1-12).

- day:

  integer, day of the month (1-31, depending on the month).

- year:

  integer, either 2- or 4-digit year. If two-digit number, will add 1900
  onto it, depending on range.

- yearcut:

  cutoff for method to know if to convert to 4-digit year.

- date:

  A date value.

- x:

  An object.

## Value

`mdy.Date` returns a Date object, and Date.mdy returns a list with
integer values for month, day, and year. `is.Date` returns a single
logical value.

## Details

Test if an object is a date.

More work may need to be done with yearcut and 2-digit years. Best to
give a full 4-digit year.

## See also

[`Date`](https://rdrr.io/r/base/Dates.html),
[`DateTimeClasses`](https://rdrr.io/r/base/DateTimeClasses.html)

## Examples

``` r
mdy.Date(9, 2, 2013)
#> [1] "2013-09-02"

tmp <- mdy.Date(9, 2, 2013)
Date.mdy(tmp)
#> $month
#> [1] 9
#> 
#> $day
#> [1] 2
#> 
#> $year
#> [1] 2013
#> 

is.Date(tmp)
#> [1] TRUE
```
