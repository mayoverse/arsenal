# The `arsenal` Package

## Overview

The goal of [`library(arsenal)`](https://github.com/mayoverse/arsenal)
is to make statistical reporting easy. It includes many functions which
the useR will find useful to have in his/her “arsenal” of functions.
There are, at this time, 6 main functions, documented below. Each of
these functions is motivated by a local SAS macro or procedure of
similar functionality.

Note that `arsenal` v3.0.0 is not backwards compatible with previous
versions (mainly because `compare()` got renamed to
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)).
See the `NEWS` file for more details.

`arsenal` now has a `pkgdown` site:
<https://mayoverse.github.io/arsenal/>

## The `tableby()` Function

[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
is a function to easily summarize a set of independent variables by one
or more categorical variables. Optionally, an appropriate test is
performed to test the distribution of the independent variables across
the levels of the categorical variable. Options for this function are
easily controlled using
[`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

The
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
output is easily knitted in an Rmarkdown document or displayed in the
command line using the
[`summary()`](https://rdrr.io/r/base/summary.html) function. Other S3
methods are implemented for objects from
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
including [`print()`](https://rdrr.io/r/base/print.html), `[`,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`sort()`](https://rdrr.io/r/base/sort.html),
[`merge()`](https://rdrr.io/r/base/merge.html),
[`padjust()`](https://mayoverse.github.io/arsenal/reference/padjust.md),
[`head()`](https://rdrr.io/r/utils/head.html), and
[`tail()`](https://rdrr.io/r/utils/head.html).

## The `paired()` Function

[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md) is
a function to easily summarize a set of independent variables across two
time points. Optionally, an appropriate test is performed to test the
distribution of the independent variables across the time points.
Options for this function are easily controlled using
[`paired.control()`](https://mayoverse.github.io/arsenal/reference/paired.control.md).

The
[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md)
output is easily knitted in an Rmarkdown document or displayed in the
command line using the
[`summary()`](https://rdrr.io/r/base/summary.html) function. It has the
same S3 methods as
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
since it’s a special case of the
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
object.

## The `modelsum()` Function

[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
is a function to fit and summarize models for each independent variable
with one or more response variables, with options to adjust for
covariates for each model. Options for this function are easily
controlled using
[`modelsum.control()`](https://mayoverse.github.io/arsenal/reference/modelsum.control.md).

The `modelsum` output is easily knitted in an Rmarkdown document or
displayed in the command line using the
[`summary()`](https://rdrr.io/r/base/summary.html) function. Other S3
methods are implemented for objects from
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
including [`print()`](https://rdrr.io/r/base/print.html), `[`,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html), and
[`merge()`](https://rdrr.io/r/base/merge.html).

## The `freqlist()` Function

[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
is a function to approximate the output from SAS’s `PROC FREQ` procedure
when using the `/list` option of the `TABLE` statement. Options for this
function are easily controlled using
[`freq.control()`](https://mayoverse.github.io/arsenal/reference/freq.control.md).

The
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
output is easily knitted in an Rmarkdown document or displayed in the
command line using the
[`summary()`](https://rdrr.io/r/base/summary.html) function. Other S3
methods are implemented for objects from
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
including [`print()`](https://rdrr.io/r/base/print.html), `[`,
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html),
[`sort()`](https://rdrr.io/r/base/sort.html), and
[`merge()`](https://rdrr.io/r/base/merge.html). Additionally, the
[`summary()`](https://rdrr.io/r/base/summary.html) output can be used
with [`head()`](https://rdrr.io/r/utils/head.html) or
[`tail()`](https://rdrr.io/r/utils/head.html).

## The `comparedf()` Function

[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
compares two data.frames and reporting any differences between them,
much like SAS’s `PROC COMPARE` procedure.

The
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
output is easily knitted in an Rmarkdown document or displayed in the
command line using the
[`summary()`](https://rdrr.io/r/base/summary.html) function. Other S3
methods are implemented for objects of class `"comparedf"`, including
[`print()`](https://rdrr.io/r/base/print.html),
[`n.diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md),
[`n.diff.obs()`](https://mayoverse.github.io/arsenal/reference/diffs.md),
and [`diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md).

## The `write2*()` Family of Functions

[`write2word()`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
[`write2pdf()`](https://mayoverse.github.io/arsenal/reference/write2specific.md),
and
[`write2html()`](https://mayoverse.github.io/arsenal/reference/write2specific.md)
are functions to output a table into a document, much like SAS’s `ODS`
procedure. The S3 method behind them is
[`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md).
There are methods implemented for
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
and
[`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md),
and also methods for
[`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html),
[`xtable::xtable()`](https://rdrr.io/pkg/xtable/man/xtable.html), and
[`pander::pander_return()`](https://rdrr.io/pkg/pander/man/pander_return.html).
Another option is to coerce an object using
[`verbatim()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md)
to print out the results monospaced (as if they were in the
terminal)–the default method does this automatically. To output multiple
tables into a document, simply make a list of them and call the same
function as before. A YAML header can be added using
[`yaml()`](https://mayoverse.github.io/arsenal/reference/yaml.md). Code
chunks can be written using
[`code.chunk()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md).

For more information, see
[`vignette("write2")`](https://mayoverse.github.io/arsenal/articles/write2.md).

## Other Notable Functions

- [`keep.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
  keeps the `'label'` attribute on an R object when subsetting.
  [`loosen.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
  allows the labels to drop again.

- [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md)
  is a shortcut to collapse variable names into a formula.

- [`mdy.Date()`](https://mayoverse.github.io/arsenal/reference/mdy.Date.md)
  and
  [`Date.mdy()`](https://mayoverse.github.io/arsenal/reference/mdy.Date.md)
  convert numeric dates for month, day, and year to Date object, and
  vice versa.

- `is.Date`: tests if an object is a date.

- `%nin%` tests for “not in”, the negation of `%in%`.

- [`allNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
  tests for all elements being NA, and
  [`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
  makes NAs explicit values.
