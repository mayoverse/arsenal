# Control settings for `freqlist` function

Control test and summary settings for the
[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
function.

## Usage

``` r
freq.control(
  sparse = FALSE,
  single = FALSE,
  dupLabels = FALSE,
  digits.count = 0L,
  digits.pct = 2L,
  ...,
  digits = NULL
)
```

## Arguments

- sparse:

  a logical value indicating whether to keep rows with counts of zero.
  The default is `FALSE` (drop zero-count rows).

- single:

  logical, indicating whether to collapse results created using a strata
  variable into a single table for printing

- dupLabels:

  logical: should labels which are the same as the row above be printed?
  The default (`FALSE`) more closely approximates `PROC FREQ` output
  from SAS, where a label carried down from the row above is left blank.

- digits.count:

  Number of decimal places for count values.

- digits.pct:

  Number of decimal places for percents.

- ...:

  additional arguments.

- digits:

  A deprecated argument

## Value

A list with settings to be used within the `freqlist` function.

## See also

[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
[`summary.freqlist`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md),
[`freqlist.internal`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md)

## Author

Ethan Heinzen
