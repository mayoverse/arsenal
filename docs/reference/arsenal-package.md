# An Arsenal of 'R' Functions for Large-Scale Statistical Summaries

An Arsenal of 'R' functions for large-scale statistical summaries, which
are streamlined to work within the latest reporting tools in 'R' and
'RStudio' and which use formulas and versatile summary statistics for
summary tables and models.

## Details

The package download, NEWS, and README are available on CRAN:
<https://cran.r-project.org/package=arsenal>

## Functions

Below are listed some of the most widely used functions available in
`arsenal`:

[`tableby`](https://mayoverse.github.io/arsenal/reference/tableby.md):
Summary statistics of a set of independent variables by a categorical
variable.

[`paired`](https://mayoverse.github.io/arsenal/reference/paired.md):
Summary statistics of a set of independent variables paired across two
timepoints.

[`modelsum`](https://mayoverse.github.io/arsenal/reference/modelsum.md):
Fit models over each of a set of independent variables with a response
variable.

[`freqlist`](https://mayoverse.github.io/arsenal/reference/freqlist.md):
Approximate the output from SAS's `PROC FREQ` procedure when using the
`/list` option of the `TABLE` statement.

[`comparedf`](https://mayoverse.github.io/arsenal/reference/comparedf.md):
Compare two data.frames and report any differences between them, much
like SAS's `PROC COMPARE` procedure.

[`write2word`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`write2html`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`write2pdf`](https://mayoverse.github.io/arsenal/reference/write2specific.md):
Functions to output tables to a single Word, HTML, or PDF document.

[`write2`](https://mayoverse.github.io/arsenal/reference/write2.md):
Functions to output tables to a single document. (Also the S3 backbone
behind the `write2*` functions.)

[`keep.labels`](https://mayoverse.github.io/arsenal/reference/keep.labels.md):
Keep the `'label'` attribute on an R object when subsetting.

[`formulize`](https://mayoverse.github.io/arsenal/reference/formulize.md):
A shortcut to generate one-, two-, or many-sided formulas.

[`mdy.Date`](https://mayoverse.github.io/arsenal/reference/mdy.Date.md)
and
[`Date.mdy`](https://mayoverse.github.io/arsenal/reference/mdy.Date.md):
Convert numeric dates for month, day, and year to Date object, and vice
versa.

[`is.Date`](https://mayoverse.github.io/arsenal/reference/mdy.Date.md):
Test if an object is a date.

[`%nin%`](https://mayoverse.github.io/arsenal/reference/grapes-nin-grapes.md):
Test for "not in".

[`allNA`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
and
[`includeNA`](https://mayoverse.github.io/arsenal/reference/NA.operations.md):
some useful functions for dealing with NAs.

## Data

[`mockstudy`](https://mayoverse.github.io/arsenal/reference/mockstudy.md):
Mock study data for examples.

## See also

Useful links:

- <https://github.com/mayoverse/arsenal>

- <https://cran.r-project.org/package=arsenal>

- <https://mayoverse.github.io/arsenal/>

- Report bugs at <https://github.com/mayoverse/arsenal/issues>

## Author

**Maintainer**: Ethan Heinzen <heinzen.ethan@mayo.edu>

Authors:

- Ethan Heinzen <heinzen.ethan@mayo.edu>

- Jason Sinnwell

- Elizabeth Atkinson

- Tina Gunderson

- Gregory Dougherty

Other contributors:

- Patrick Votruba \[contributor\]

- Ryan Lennon \[contributor\]

- Andrew Hanson \[contributor\]

- Krista Goergen \[contributor\]

- Emily Lundt \[contributor\]

- Brendan Broderick \[contributor\]

- Maddie McCullough \[artist\]

## Examples

``` r
library(arsenal)
```
