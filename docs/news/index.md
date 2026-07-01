# Changelog

## arsenal v3.7.0

This is the first release in several years.

- [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):
  `sign.test()` is now officially `signtest()`, but the former is
  supported for backwards compatibility.

- Added
  [`pct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
  and
  [`rowpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
  ([\#337](https://github.com/mayoverse/arsenal/issues/337))

- Fixed one bug in `[.arsenal_table()`.
  ([\#338](https://github.com/mayoverse/arsenal/issues/338))

- Allowed for by-level subsetting in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  by implementing a `[.tableby()` method.
  ([\#336](https://github.com/mayoverse/arsenal/issues/336))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):
  implemented `stddiff()`, which computes standardized differences,
  instead of p-values.

- Added `stddiff` as a “Suggests”.

- Remove
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md)
  default for `x=`.

- Change logic for `escape=TRUE` when zero-length variables are
  involved.

- Added
  [`meanpmsd()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
  and
  [`meanpmse()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
  ([\#343](https://github.com/mayoverse/arsenal/issues/343))

- Fixed an issue with long variable names and
  [`deparse()`](https://rdrr.io/r/base/deparse.html).
  ([\#342](https://github.com/mayoverse/arsenal/issues/342))

- Missings and non-missings:

  - `weights=` is no longer allowed to have NAs

  - Renamed
    [`Npct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    to
    [`Nrowpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    for consistency. It calculates the number of non-missing
    observations and the percentage of the row-total of non-missing
    values.

  - Removed the `na.rm=` arguments from
    [`N()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`Nrowpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)

  - Added
    [`Nmisspct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`Nmisspct2()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
    ([\#346](https://github.com/mayoverse/arsenal/issues/346))

  - Changed the label for
    [`Nrowpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)

- Allowed for `stats=` argument to inline functions in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md).
  ([\#348](https://github.com/mayoverse/arsenal/issues/348))

- Overhauled the
  [`as.tbstat()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md)
  framework ([\#351](https://github.com/mayoverse/arsenal/issues/351)):

  - Edited some summary statistic definitions to borrow from others

  - Added `print.tbstat()`

  - Removed `as.countpct()`.

  - Added
    [`tbfmt()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md).

  - Added the `fmt=` argument and attribute, giving a
    `glue`-specification for how to format.

  - Added `glue` to the imports.

- Custom-formatted p-values
  ([\#345](https://github.com/mayoverse/arsenal/issues/345)):

  - `format.p=` and `digits.p=` are now arguments to the inline
    functions.

  - `format.p=` now accepts `glue` specifications.

## arsenal v3.6.3

CRAN release: 2021-06-04

- Added `medtest()` to
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  for a median test.
  ([\#327](https://github.com/mayoverse/arsenal/issues/327))

- Account for NAs in `sign.test()` in
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md).
  ([\#326](https://github.com/mayoverse/arsenal/issues/326))

- Add
  [`Nsigntest()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
  for
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md).
  ([\#326](https://github.com/mayoverse/arsenal/issues/326))

- Add `"Nevents"` for binomial GLMs.
  ([\#325](https://github.com/mayoverse/arsenal/issues/325))

- A fix for R devel when digits=0.

## arsenal v3.6.2

CRAN release: 2021-02-17

- Fixed one URL

- Fixed two `modelsum` test errors that were a result of rounding on
  macos.

## arsenal v3.6.1

CRAN release: 2021-02-06

- Fixed two URLs

## arsenal v3.6.0

Possible breaking change:

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
  and
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
  now use the `caption=` argument in
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to
  generate captions
  ([`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
  already does).
  ([\#310](https://github.com/mayoverse/arsenal/issues/310))

- Changes to `DESCRIPTION` file:

  - Changed `broom` requirement to \>= 0.7.1, in which a bug with
    [`geepack::geeglm`](https://rdrr.io/pkg/geepack/man/geeglm.html) was
    fixed. This affected one test in the test suite.

  - Added `geepack` package to “Suggests”
    ([\#279](https://github.com/mayoverse/arsenal/issues/279)).

  - Removed `gam` from the “Suggests”, in favor of `splines`, which the
    [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    vignette actually uses.

  - Changed `survival` requirement to `>= 2.43-1`.

Other changes:

- Added code to error informatively when “Suggests” aren’t available.
  ([\#317](https://github.com/mayoverse/arsenal/issues/317))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - Added
    [`selectall()`](https://mayoverse.github.io/arsenal/reference/selectall.md).

  - Added “label” option to `cat.simplify=` and `ord.simplify=` for
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)/[`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md).
    ([\#288](https://github.com/mayoverse/arsenal/issues/288))

  - Fixed a bug in
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    /
    [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md)
    where `stats.labels=` specification would remove all default labels.
    ([\#316](https://github.com/mayoverse/arsenal/issues/316))

  - Added `wt()` (Wilcoxon test) option for
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
    ([\#321](https://github.com/mayoverse/arsenal/issues/321))

  - Fixed a bug in
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    relating to a weird edge case when a by-level is “Total” and the
    total label is set to something else.

  - Added a feature to
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
    to allow for the total column to be moved before the other columns.
    ([\#320](https://github.com/mayoverse/arsenal/issues/320))

  - Added a feature to
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
    to allow for dropping of categorical levels.
    ([\#318](https://github.com/mayoverse/arsenal/issues/318))

  - Added
    [`meanse()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    for
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
    ([\#315](https://github.com/mayoverse/arsenal/issues/315))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added
    [`relrisk()`](https://mayoverse.github.io/arsenal/reference/modelsum.family.md)
    to
    [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    families (with corresponding addition of `geepack` package to
    “Suggests”).
    ([\#279](https://github.com/mayoverse/arsenal/issues/279))

  - Fixed a bug in
    [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    with confidence level for survival.

  - Suppressed warnings with new broom version when using
    [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html).

- [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):

  - Added option to
    [`diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md)
    for extracting not-shared observations.
    ([\#305](https://github.com/mayoverse/arsenal/issues/305))

  - Fixed bug in
    [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
    when things are infinite.
    ([\#306](https://github.com/mayoverse/arsenal/issues/306))

- [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md):

  - Added note to
    [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md)
    vignette about a global option for R Markdown documents in R Studio.
    ([\#312](https://github.com/mayoverse/arsenal/issues/312))

## arsenal v3.5.0

CRAN release: 2020-07-13

- Change R requirement to \>= 3.4.0.

- Fixed a bug to conform with new `broom` publication, and change
  `broom` requirement to \>= 0.7.0.
  ([\#296](https://github.com/mayoverse/arsenal/issues/296))

- Fixed a bug to conform with new `knitr` publication, and change
  `knitr` requirement to \>= 1.29.
  ([\#299](https://github.com/mayoverse/arsenal/issues/299),
  [\#300](https://github.com/mayoverse/arsenal/issues/300),
  [\#301](https://github.com/mayoverse/arsenal/issues/301))

- The GitHub repository was moved from
  <https://github.com/eheinzen/arsenal/> to
  <https://github.com/mayoverse/arsenal/>. The corresponding `pkgdown`
  site is now at <https://mayoverse.github.io/arsenal/>

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):
  Changed the default for `addNA` to be `TRUE` in the formula method, so
  that you only have to specify `na.options` like in the table method.

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added support for conditional logistic regressions.
    ([\#275](https://github.com/mayoverse/arsenal/issues/275))

  - Fixed a bug in
    [`modelsum.control()`](https://mayoverse.github.io/arsenal/reference/modelsum.control.md)
    with confidence interval specification.

  - Expanded the statistic list for survival models.

  - Eliminated call to
    [`broom::confint_tidy()`](https://broom.tidymodels.org/reference/confint_tidy.html).
    ([\#296](https://github.com/mayoverse/arsenal/issues/296))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Removed checks for existance of stat functions (the check wasn’t
    working anyway) and search both the enclosing environment as well as
    [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) (for
    custom stat functions).

  - [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):
    better described
    [`as.tbstat()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.internal.md)
    and `as.countpct()` in the vignette, and make `as.countpct()`
    slightly more flexible.
    ([\#283](https://github.com/mayoverse/arsenal/issues/283))

  - [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):
    fixed a bug with detecting stat functions.

  - [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):
    added an error if reserved terms are used in the by-variable.
    ([\#277](https://github.com/mayoverse/arsenal/issues/277))

  - Added an option for HTML footnotes (and superscripts) in
    [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md).
    ([\#298](https://github.com/mayoverse/arsenal/issues/298))

## arsenal v3.4.0

CRAN release: 2020-02-15

- Added a `pkgdown` site: <https://eheinzen.github.io/arsenal/>

- Moved knitr to an import.
  ([\#255](https://github.com/mayoverse/arsenal/issues/255))

- [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):
  Added support for tolerances by variable.
  ([\#167](https://github.com/mayoverse/arsenal/issues/167))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

    - Allowed for changing of “Overall” and “Total” labels.
      ([\#253](https://github.com/mayoverse/arsenal/issues/253),
      [\#261](https://github.com/mayoverse/arsenal/issues/261))

    - Allowed for suppression of N’s in the header.
      ([\#256](https://github.com/mayoverse/arsenal/issues/256),
      [\#36](https://github.com/mayoverse/arsenal/issues/36))

    - Allowed for digits formatting of N’s in the header.
      ([\#257](https://github.com/mayoverse/arsenal/issues/257))

    - Escaped `%` for `text="latex"`.
      ([\#258](https://github.com/mayoverse/arsenal/issues/258))

    - Added to vignette describing `merge(all=TRUE)`.

    - Fixed vignette re: outputting to CSV.
      ([\#278](https://github.com/mayoverse/arsenal/issues/278))

  - [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

    - Allowed for changing of “Difference” label.
      ([\#271](https://github.com/mayoverse/arsenal/issues/271))

    - Removed “…” from the documentation for `paired.internal` per
      CRAN’s request (PR#16223 for R-devel).

  - Added support for “min”, “max”, “sd”, “mean”, and “var”
    ([\#259](https://github.com/mayoverse/arsenal/issues/259)) and
    “gmean”, “gsd”, “gmeansd”, “gmeanCI”
    ([\#260](https://github.com/mayoverse/arsenal/issues/260)) and
    “Npct” ([\#263](https://github.com/mayoverse/arsenal/issues/263))
    and “sum”
    ([\#281](https://github.com/mayoverse/arsenal/issues/281)).

  - Added a more informative error message when no summary statistic is
    computed. ([\#273](https://github.com/mayoverse/arsenal/issues/273))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Fixed “statistic.F” for family=“gaussian”.
    ([\#262](https://github.com/mayoverse/arsenal/issues/262))

  - Fixed “Nevents” for family=“survival”.
    ([\#266](https://github.com/mayoverse/arsenal/issues/266))

  - Fixed vignette re: outputting to CSV.
    ([\#278](https://github.com/mayoverse/arsenal/issues/278))

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):
  Updated
  [`head.summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md)
  and
  [`tail.summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md)
  to comply with new R-devel definitions.

- Updated “labels” vignette.
  ([\#267](https://github.com/mayoverse/arsenal/issues/267))

- Added `escape =` argument to
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md).
  ([\#282](https://github.com/mayoverse/arsenal/issues/282))

## arsenal v3.3.0

CRAN release: 2019-09-07

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Redid how weights are handled. The only user-visible changes should
    be that standard deviations on length-1 groups are now reported as
    `NA` instead of `NaN`.

  - Fixed a bug with
    [`modpval.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    when factors are involved.
    ([\#239](https://github.com/mayoverse/arsenal/issues/239))

  - Added
    [`meanCI()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`medianmad()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
    ([\#230](https://github.com/mayoverse/arsenal/issues/230),
    [\#232](https://github.com/mayoverse/arsenal/issues/232))

  - Added the units for `difftime` statistics when using dates (e.g.,
    `meansd`, `medianmad`, `iqr`).

  - Fixed Chi-square and Fisher’s Exact test for one-level categorical
    variables.
    ([\#227](https://github.com/mayoverse/arsenal/issues/227),
    [\#228](https://github.com/mayoverse/arsenal/issues/228))

  - Fixed the n’s in the header when using weights.
    ([\#229](https://github.com/mayoverse/arsenal/issues/229))

  - Fixed a bug with confidence levels supplied through the control
    argument. ([\#234](https://github.com/mayoverse/arsenal/issues/234))

  - [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):
    fixed a bug when using
    [`count()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    with factors.
    ([\#235](https://github.com/mayoverse/arsenal/issues/235))

  - [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md):
    added explicit `times=` argument for survival summaries.

  - Added option to run statistical tests even if one by-group has 0
    observations.
    ([\#233](https://github.com/mayoverse/arsenal/issues/233),
    [\#250](https://github.com/mayoverse/arsenal/issues/250),
    [\#251](https://github.com/mayoverse/arsenal/issues/251))

  - Stopped the formatting of p-values when they’re not numeric (if,
    say, they’re pre-formatted by the user).
    ([\#249](https://github.com/mayoverse/arsenal/issues/249))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added functionality for multiple adjustor sets.
    ([\#240](https://github.com/mayoverse/arsenal/issues/240))

  - Fixed “Nmiss” and “N” when used with strata, which now both report
    the missings for the entire fit.
    ([\#241](https://github.com/mayoverse/arsenal/issues/241),
    [\#242](https://github.com/mayoverse/arsenal/issues/242),
    [\#243](https://github.com/mayoverse/arsenal/issues/243))

  - Suppressed messages from
    [`pROC::auc()`](https://rdrr.io/pkg/pROC/man/auc.html) when
    calculating AUC.
    ([\#244](https://github.com/mayoverse/arsenal/issues/244))

  - Fixed confidence level for survival models.
    ([\#245](https://github.com/mayoverse/arsenal/issues/245))

  - Added an option for the likelihood ratio test for the main effect
    (but not the adjustors): `p.value.lrt`
    ([\#238](https://github.com/mayoverse/arsenal/issues/238))

  - Blanked out p-values that are NA.
    ([\#246](https://github.com/mayoverse/arsenal/issues/246))

- [`code.chunk()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md):

  - Fixed logic checking the length of `chunk.opts=`.

  - Allowed for empty code chunks.
    ([\#236](https://github.com/mayoverse/arsenal/issues/236))

- [`verbatim()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md):
  removed named argument in favor of the dots; reworked the object
  structure to fix edge case printing oddities.
  ([\#248](https://github.com/mayoverse/arsenal/issues/248))

- Removed defunct functions.

## arsenal v3.2.0

CRAN release: 2019-06-13

- [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):

  - Fixed a bug when “row.names” was used in combination with other
    by-variables.
    ([\#212](https://github.com/mayoverse/arsenal/issues/212))

  - Allowed for comparison of variables which have any class in common.
    ([\#216](https://github.com/mayoverse/arsenal/issues/216))

  - [`summary.comparedf()`](https://mayoverse.github.io/arsenal/reference/summary.comparedf.md):

    - Removed the “comparedf.frame.summary” class from the first element
      to allow it to print.
      ([\#211](https://github.com/mayoverse/arsenal/issues/211))

    - Fixed a bug with reporting blank by-variables.
      ([\#213](https://github.com/mayoverse/arsenal/issues/213))

    - Fixed a bug with reporting by-variables as variables in common.
      ([\#214](https://github.com/mayoverse/arsenal/issues/214))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):
  added Wald confidence intervals to `binom.stats=`.
  ([\#219](https://github.com/mayoverse/arsenal/issues/219))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Fixed a bug with
    [`merge.arsenal_table()`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md)
    losing control parameters for
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    objects. ([\#221](https://github.com/mayoverse/arsenal/issues/221))

  - Allowed for variable-name-only `labelTranslations=` assignment for
    terms with inline statistical test specification. Backward
    compatibility should be maintained here.
    ([\#220](https://github.com/mayoverse/arsenal/issues/220))

  - Fixed a bug with assigning `NULL` labels with inline statistical
    test specifications.
    ([\#222](https://github.com/mayoverse/arsenal/issues/222))

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    fixed a bug with formatting when strata aren’t in alphabetical order
    and have different number of elements (e.g., if only one includes
    missing values).
    ([\#215](https://github.com/mayoverse/arsenal/issues/215))

## arsenal v3.1.0

CRAN release: 2019-05-03

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Added “Nmiss” to default `surv.stats=` in
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

  - Fixed a bug when some
    [`Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) elements are
    NA. ([\#208](https://github.com/mayoverse/arsenal/issues/208))

  - [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md):
    fixed a bug with simplifying categorical and numeric output.
    ([\#199](https://github.com/mayoverse/arsenal/issues/199),
    [\#203](https://github.com/mayoverse/arsenal/issues/203)) This fix
    also allows for simplification of custom statistics.
    ([\#200](https://github.com/mayoverse/arsenal/issues/200))

  - [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md):
    added `date.simplify=` and `ordered.simplify=` arguments.
    ([\#202](https://github.com/mayoverse/arsenal/issues/202)) The order
    of arguments has changed slightly for consistency.

  - [`paired.control()`](https://mayoverse.github.io/arsenal/reference/paired.control.md):
    took away the arguments that should be the same as
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md),
    and only included arguments with new defaults or which don’t appear
    in
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).

  - Added the functions
    [`countN()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`Nrisk()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    ([\#201](https://github.com/mayoverse/arsenal/issues/201)).
    [`Nrisk()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    now outputs what
    [`NriskSurv()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    used to;
    [`NriskSurv()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    now outputs what its name suggests: the number at risk, and the
    survival. Additionally, `as.countpct()` gains the `which.pct=`
    argument, whose default of `0` may break the formatting of percents
    (`digits.pct=`).

- [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):

  - Added additional summary table to the
    [`summary()`](https://rdrr.io/r/base/summary.html) output.

  - Moved the `max.print...=` arguments to
    [`comparedf.control()`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md).
    `max.print.diff=` is now deprecated and is replaced by
    `max.print.diffs.per.var=`. `max.print.diffs=` was also added to
    control overall number of differences printed.

  - Fixed a bug with numeric percent tolerances when both values being
    compared are 0.
    ([\#206](https://github.com/mayoverse/arsenal/issues/206))

  - Fixed a bug in
    [`diffs()`](https://mayoverse.github.io/arsenal/reference/diffs.md)
    (and hence [`summary()`](https://rdrr.io/r/base/summary.html)) when
    no variables are compared
    ([\#207](https://github.com/mayoverse/arsenal/issues/207)). Note
    that this change also included a change to the by-variables reported
    in the
    [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
    object when merging over row.names.

## arsenal v3.0.0

CRAN release: 2019-03-25

**There are a few non-backwards-compatible updates.**

Major changes:

- Renamed `compare()` -\>
  [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md)
  and `comparison.control()` -\>
  [`comparedf.control()`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md).
  ([\#179](https://github.com/mayoverse/arsenal/issues/179))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):
  Fixed bug(s) with interaction terms.
  ([\#173](https://github.com/mayoverse/arsenal/issues/173),
  [\#177](https://github.com/mayoverse/arsenal/issues/177))

- Added a new function
  [`loosen.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
  which removes the classes added by
  [`keep.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
  and thereby speeds up subsetting when labels are no longer needed.
  This is now used in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
  and
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md).

Smaller changes:

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  /
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Fixed two bugs relating to
    [`modpval.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md):
    one which didn’t properly assign the p-value name
    ([\#174](https://github.com/mayoverse/arsenal/issues/174)), and one
    which broke
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) when
    assigning custom p-values for only one strata
    ([\#175](https://github.com/mayoverse/arsenal/issues/175)).

  - These now issue informative error when class isn’t recognized.
    ([\#180](https://github.com/mayoverse/arsenal/issues/180))

  - Fixed two bugs in the
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    vignette:
    [`modpval.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    wasn’t working properly
    ([\#170](https://github.com/mayoverse/arsenal/issues/170)), and
    `pfootnote=TRUE` was commented out
    ([\#169](https://github.com/mayoverse/arsenal/issues/169)).

  - Fixed a bug with per-variable stats and digit specifications being
    lost when using the `subset=` argument.
    ([\#182](https://github.com/mayoverse/arsenal/issues/182),
    [\#183](https://github.com/mayoverse/arsenal/issues/183))

  - Made all-NA summaries prettier.
    ([\#190](https://github.com/mayoverse/arsenal/issues/190))

  - This now issues a warning when `coin` isn’t available for the trend
    test. ([\#193](https://github.com/mayoverse/arsenal/issues/193))

- [`comparedf()`](https://mayoverse.github.io/arsenal/reference/comparedf.md):

  - This now allows for zero-row data.frames.
    ([\#166](https://github.com/mayoverse/arsenal/issues/166))

  - [`comparedf.control()`](https://mayoverse.github.io/arsenal/reference/comparedf.control.md)
    now allows for named `tol.vars=` argument to manually match column
    names. ([\#165](https://github.com/mayoverse/arsenal/issues/165))

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - Fixed a bug where labels would get dropped when using the `subset=`
    argument. ([\#184](https://github.com/mayoverse/arsenal/issues/184))

  - Fixed a bug where labels were lost when subsetting the table and
    using strata terms.
    ([\#196](https://github.com/mayoverse/arsenal/issues/196))

  - [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):
    implemented a [`sort()`](https://rdrr.io/r/base/sort.html) method to
    sort tables on frequency.
    ([\#187](https://github.com/mayoverse/arsenal/issues/187))

  - [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md):
    Implemented [`head()`](https://rdrr.io/r/utils/head.html) and
    [`tail()`](https://rdrr.io/r/utils/head.html).
    ([\#188](https://github.com/mayoverse/arsenal/issues/188))

  - [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md):
    fixed a bug when all table counts are 0 and `sparse=FALSE`.
    ([\#186](https://github.com/mayoverse/arsenal/issues/186),
    [\#194](https://github.com/mayoverse/arsenal/issues/194))

- [`keep.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md):

  - This no longer sticks another class on data.frames.

  - Fixed a bug with replacement for objects of class `"keep_labels"`.

- [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md):
  added the `collapse=` and `collapse.y=` arguments.
  ([\#197](https://github.com/mayoverse/arsenal/issues/197))

## arsenal v2.0.0

CRAN release: 2019-01-16

There is a new class system (`"arsenal_table"`) which unifies
[`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
[`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
and
[`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md).

- `arsenal` now imports and re-exports
  [`utils::head()`](https://rdrr.io/r/utils/head.html) and
  [`utils::tail()`](https://rdrr.io/r/utils/head.html).

- `arsenal` now has a sticker!

- `arsenal_table`:

  - Implemented a new class (without a constructor).

  - `labels<-.arsenal_table()` doesn’t support unnamed labels, as it’s
    unclear how to assign them to multiple by-variables and strata. It
    also doesn’t give warnings if your labels are not used.

  - `[.arsenal_table()` has an argument `j=` to select the by-variables.

  - [`merge.arsenal_table()`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md)
    has arguments to select which by-variables to keep if not all are in
    common. It also checks to make sure that strata, weights, and
    by-variables are all identical.

  - [`print.arsenal_table()`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md)
    shows y- and x-variables, plus any strata.

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  and
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Added functionality for multiple by-variables and strata terms. This
    required completely reworking the innards of the `tableby` object.

  - Removed `length.tableby()` (because it was messing up
    [`str()`](https://rdrr.io/r/utils/str.html) and R Studio) and
    replaced with
    [`head.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    and
    [`tail.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    (the original purpose to having
    [`length()`](https://rdrr.io/r/base/length.html) defined).

  - Implemented
    [`sort.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md),
    which errors out if the object has strata or multiple by-variables,
    and then runs the default method.

  - [`modpval.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    now requires the first column to be the by-variable, and if the
    object has a strata, the second column is required to be the
    corresponding strata value.

  - [`tests.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    now returns a data.frame with a by-variable column and (if
    applicable) a strata column.

  - [`na.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    now generates functions. The “lhs=” argument determines whether to
    remove NAs from the first column of the data. If
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    detects a one-sided formula, it sets this to FALSE. Both versions
    now remove rows with NAs in the strata column (when applicable).

  - [`na.paired()`](https://mayoverse.github.io/arsenal/reference/paired.internal.md)
    now removes rows with NAs in the strata column (when applicable).

  - [`padjust.tableby()`](https://mayoverse.github.io/arsenal/reference/padjust.md)
    and
    [`padjust.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/padjust.md)
    will error if fed an object with strata or multiple by-variables.

  - [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
    and
    [`as.data.frame.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    gain the `list.ok=` argument, for when there are multiple
    left-hand-sides.

  - Added logic to statistical tests to detect missing levels of the
    by-variable.

  - Fixed a bug with LaTeX formatting involving the `align=` argument to
    [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

  - Passing `term.name=TRUE` to
    [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    or
    [`as.data.frame.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    will now put the term name in the top left corner of each table.

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added functionality for multiple by-variables and strata terms. This
    required completely reworking the innards of the `modelsum` object.

  - [`na.modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.internal.md)
    now removes rows with NAs in the strata column (when applicable).

  - [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)
    and
    [`as.data.frame.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    gain the `list.ok=` argument, for when there are multiple
    left-hand-sides.

  - Passing `term.name=TRUE` to
    [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    or
    [`as.data.frame.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    will now put the term name in the top left corner of each table.

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - Added functionality for multiple by-variables. This required
    completely reworking the innards of the `freqlist` object.

  - Changed the argument `groupBy=` to `strata=` to match
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    and
    [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md).

  - Added
    [`merge.freqlist()`](https://mayoverse.github.io/arsenal/reference/arsenal_table.md)
    and
    [`as.data.frame.summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md).
    Note that `[.arsenal_table()` now allows you to remove the
    cumulative and percent columns.

  - Note that `labels<-.arsenal_table()` no longer supports unnamed
    labels, but now accepts labels for the frequency, cumulative, and
    percent columns for `freqlist` objects.

  - Removed the `digits=`, `sparse=`, `single=`, and `dupLabels=`
    arguments from
    [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    and
    [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md).
    These are now arguments to the new
    [`freq.control()`](https://mayoverse.github.io/arsenal/reference/freq.control.md),
    and are passed through the dots (for backwards compatibility).
    [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    also gained the `control=` argument for objects from
    [`freq.control()`](https://mayoverse.github.io/arsenal/reference/freq.control.md).

  - [`as.data.frame.freqlist()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.freqlist.md)
    no longer rounds its digits, nor does it label its columns. Use
    [`as.data.frame.summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md)
    for that instead. It also gained the `list.ok=` argument, for when
    there are multiple left-hand-sides.

- [`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md):
  removed the “character” and “numeric” methods, replacing them with a
  default. In particular, this changes the default label of what used to
  be `includeNA.numeric()`.

- [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md):

  - Changed the output to an `.Rmd` file instead of a `.md`. This
    shouldn’t break anything, unless you’re relying on the intermediate
    file.

  - Replaced the `keep.md=` argument with `keep.rmd=` (since we’re not
    using `.md` files directly anymore).

  - Added the function
    [`code.chunk()`](https://mayoverse.github.io/arsenal/reference/write2.internal.md)
    to write executable code chunks to the `.Rmd`.

## arsenal v1.5.0

CRAN release: 2018-11-09

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  and
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - fixed a bug with specifying individual statistics for character and
    logical vectors.
    ([\#142](https://github.com/mayoverse/arsenal/issues/142))

  - [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    and
    [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):
    added a function (`notest()`) to prevent performing a test on an
    individual variable.
    ([\#144](https://github.com/mayoverse/arsenal/issues/144))

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    changed NA p-values to blanks.
    ([\#145](https://github.com/mayoverse/arsenal/issues/145))

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    added documentation on `bookdown`.
    ([\#147](https://github.com/mayoverse/arsenal/issues/147))

  - Wrote
    [`padjust()`](https://mayoverse.github.io/arsenal/reference/padjust.md),
    an S3 wrapper for
    [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html), which can
    also adjust
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    (and hence
    [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md))
    objects. ([\#146](https://github.com/mayoverse/arsenal/issues/146))

  - `print.summary.tableby()`,
    [`as.data.frame.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    added `width=` and `min.split=` as formal arguments.

  - Fixed
    [`medSurv()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    which was calculating the median survival incorrectly, and removed
    `rangeTime()`, an ambiguous survival statistic.
    ([\#32](https://github.com/mayoverse/arsenal/issues/32))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md):
    added documentation on `bookdown`.
    ([\#147](https://github.com/mayoverse/arsenal/issues/147))

  - `print.summary.modelsum()`,
    [`as.data.frame.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md):
    added `width=` and `min.split=` as formal arguments.

- [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md):
  added documentation on `bookdown`.
  ([\#147](https://github.com/mayoverse/arsenal/issues/147))

- [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md):
  added support for names and calls.
  ([\#152](https://github.com/mayoverse/arsenal/issues/152),
  [\#153](https://github.com/mayoverse/arsenal/issues/153))

## arsenal v1.4.0

CRAN release: 2018-09-18

**There are a few non-backwards-compatible updates.**

Major changes:

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added `family="ordinal"` to do ordinal logistic regression using
    [`MASS::polr()`](https://rdrr.io/pkg/MASS/man/polr.html).
    ([\#130](https://github.com/mayoverse/arsenal/issues/130))

  - Added `family="negbin"` to do negative binomial regression using
    [`MASS::glm.nb()`](https://rdrr.io/pkg/MASS/man/glm.nb.html).
    ([\#15](https://github.com/mayoverse/arsenal/issues/15))

  - Added support for ordinal regressors and adjustment terms (by adding
    support for their associated contrasts).
    ([\#133](https://github.com/mayoverse/arsenal/issues/133))

  - Allowed for LaTeX formatting. NOTE: this changes (hence possibly
    breaking old code) the formatting behavior when specifying
    `text="html"`.
    ([\#123](https://github.com/mayoverse/arsenal/issues/123))

- `summary.compare.data.frame()`: Added a small summary of the input
  data.frames as the first table.
  ([\#126](https://github.com/mayoverse/arsenal/issues/126)) NOTE: this
  changes the structure and printed output of
  [`summary()`](https://rdrr.io/r/base/summary.html)!

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - Allowed for LaTeX formatting. NOTE: this changes (hence possibly
    breaking old code) the formatting behavior when specifying
    `text="html"`.
    ([\#123](https://github.com/mayoverse/arsenal/issues/123))

  - Added functionality to in-formula functions to allow the
    specification of `digits=` (etc.), `numeric.simplify=`, and
    `cat.simplify=` for a single variable.
    ([\#107](https://github.com/mayoverse/arsenal/issues/107),
    [\#134](https://github.com/mayoverse/arsenal/issues/134),
    [\#139](https://github.com/mayoverse/arsenal/issues/139)) NB: this
    has the following breaking changes:

    - There is no longer a “name” element in the “tableby” object’s
      x-specifications; instead it’s now called “term”

    - An element for “variable”, containing the variable name, was added
      to the “tableby” object’s x-specifications.

    - An element for “control.list”, recording format specifications,
      was added to the “tableby” object’s x-specifications.

    - The output of
      [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
      now reports only the variable name in the “variable” column when
      using internal statistical functions (like
      [`anova()`](https://rdrr.io/r/stats/anova.html) and `chisq()`–it
      used to include the function call as well).

    - The output of
      [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
      no longer includes category levels in the “term” column; instead,
      it contains the statistical function used (like
      [`countpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
      and
      [`count()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)).

Smaller changes:

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Added support for *calls* to the family functions, in case a
    different link function (for example) is required.

  - Properly propogated “term.name” to the
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
    method. ([\#128](https://github.com/mayoverse/arsenal/issues/128))

  - Fixed formatting of error about unsupported families.

  - Removed “concordance” from the list of supported statistics for
    Poisson regression. This shouldn’t break much code, as specifying
    “concordance” wouldn’t have shown anything anyway.

  - Fixed a bug with formatting one-per-model p-values.
    ([\#140](https://github.com/mayoverse/arsenal/issues/140))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - Added a warning for when by-variable contains empty string.
    ([\#121](https://github.com/mayoverse/arsenal/issues/121))

  - Properly propogated “term.name” to the
    [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
    method. ([\#127](https://github.com/mayoverse/arsenal/issues/127))

  - Fixed an error that sometimes occured when using categorical
    statistics on numeric variables.
    ([\#137](https://github.com/mayoverse/arsenal/issues/137))

  - Added an argument to
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
    to simplify one-line numeric output.
    ([\#139](https://github.com/mayoverse/arsenal/issues/139))

- [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md):

  - Added a warning for when by-variable contains empty string.
    ([\#121](https://github.com/mayoverse/arsenal/issues/121))

  - Added error to `na.paired("in.both")` when there are more than two
    time points.

- `compare.data.frame()`:

  - Implemented
    [`n.diff.obs()`](https://mayoverse.github.io/arsenal/reference/diffs.md).
    ([\#124](https://github.com/mayoverse/arsenal/issues/124))

  - Added an argument to
    [`summary()`](https://rdrr.io/r/base/summary.html) to allow for the
    display of attributes.
    ([\#125](https://github.com/mayoverse/arsenal/issues/125))

  - Fixed [`summary()`](https://rdrr.io/r/base/summary.html) to now
    return the object passed it.
    ([\#141](https://github.com/mayoverse/arsenal/issues/141))

- Updated documentation where appropriate.

## arsenal v1.3.0

CRAN release: 2018-07-20

**This is a mostly backwards-compatible update.**

Major changes:

- Implemented the function
  [`paired()`](https://mayoverse.github.io/arsenal/reference/paired.md)
  for paired data, based on
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
  This comes with a very light vignette.

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):
  Change default for chi-square tests to `correct=FALSE`. Note that this
  only affects the 2 x 2 case.

Smaller changes:

- Added the a default method for label assignment (`labels<-`).
  ([\#118](https://github.com/mayoverse/arsenal/issues/118))

- Update
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md)
  to handle non-syntactic names in the `data=` argument.
  ([\#105](https://github.com/mayoverse/arsenal/issues/105))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - Implemented
    [`is.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    and
    [`is.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md).
    ([\#112](https://github.com/mayoverse/arsenal/issues/112))

  - Changed how arguments are passed to stat tests.

  - Issue a warning if statistical tests are requested when there are
    fewer than two by-levels.
    ([\#108](https://github.com/mayoverse/arsenal/issues/108))

  - Fixed `trend()` and [`anova()`](https://rdrr.io/r/stats/anova.html)
    to return an object instead of the object being invisible.

  - Implemented the stat functions
    [`binomCI()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`rowbinomCI()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    for binomial confidence intervals.
    ([\#117](https://github.com/mayoverse/arsenal/issues/117))

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    ignore row.names when printing summary objects.

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    and
    [`as.data.frame.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    added a `term.name=` argument.
    ([\#110](https://github.com/mayoverse/arsenal/issues/110))

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md):
    pass `text="html"` to get better formatting in R shiny.
    ([\#114](https://github.com/mayoverse/arsenal/issues/114))

  - Implemented
    [`Ops.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    to compare tableby objects to a number (p-value).
    ([\#96](https://github.com/mayoverse/arsenal/issues/96))

  - Implemented
    [`xtfrm.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md),
    so that tableby objects can be sorted by p-value.
    ([\#96](https://github.com/mayoverse/arsenal/issues/96))

  - Implemented `length.tableby()`, so that
    [`head()`](https://rdrr.io/r/utils/head.html) and
    [`tail()`](https://rdrr.io/r/utils/head.html) also work.
    ([\#97](https://github.com/mayoverse/arsenal/issues/97))

  - Implemented
    [`countcellpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    for counts and cell percentages.
    ([\#106](https://github.com/mayoverse/arsenal/issues/106))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Implemented
    [`is.modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.internal.md)
    and
    [`is.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.internal.md).
    ([\#111](https://github.com/mayoverse/arsenal/issues/111))

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md):
    ignore row.names when printing summary objects.

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    and
    [`as.data.frame.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md):
    added a `term.name=` argument.
    ([\#109](https://github.com/mayoverse/arsenal/issues/109))

  - Allow for `weights=` and `na.action=`.
    ([\#99](https://github.com/mayoverse/arsenal/issues/99))

  - Fixed problem with column names which are prefixes of other column
    names. ([\#98](https://github.com/mayoverse/arsenal/issues/98))

  - Fixed problem with column labels overwriting categorical levels
    which also match.
    ([\#100](https://github.com/mayoverse/arsenal/issues/100))

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md):
    pass `text="html"` to get better formatting in R shiny.
    ([\#115](https://github.com/mayoverse/arsenal/issues/115))

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - Implemented
    [`is.freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md)
    and
    [`is.summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.internal.md).
    ([\#113](https://github.com/mayoverse/arsenal/issues/113))

  - Fixed a problem with a column named “method”.
    ([\#95](https://github.com/mayoverse/arsenal/issues/95))

  - [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md):
    ignore row.names when printing summary objects.

- Update documentation.

## arsenal 1.2.0

CRAN release: 2018-04-16

- Implemented
  [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  methods for `"summary.tableby"`, `"summary.modelsum"`, and
  `"summary.freqlist"` objects.
  ([\#89](https://github.com/mayoverse/arsenal/issues/89),
  [\#90](https://github.com/mayoverse/arsenal/issues/90),
  [\#91](https://github.com/mayoverse/arsenal/issues/91))

- Center-aligned
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  grouping columns in the summary output.
  ([\#93](https://github.com/mayoverse/arsenal/issues/93))

## arsenal 1.1.0

CRAN release: 2018-03-13

**This is a mostly backwards-compatible update.**

Major changes:

- [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md)
  now returns an object. `print.summary.freqlist()` prints the resulting
  object. ([\#76](https://github.com/mayoverse/arsenal/issues/76))

Smaller changes:

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - Fixed a bug in `print.summary.tableby()` involving the lack of
    wrapping for long labels.
    ([\#59](https://github.com/mayoverse/arsenal/issues/59))

  - [`as.data.frame.summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    has been implemented, and `print.summary.tableby()` updated
    accordingly.
    ([\#60](https://github.com/mayoverse/arsenal/issues/60))

  - Fixed a bug with assigning labels for tableby objects when some
    value names are unmatched.
    ([\#64](https://github.com/mayoverse/arsenal/issues/64))

  - Fixed a bug in `print.summary.tableby()` with regards to knitting in
    R Markdown with plots immediately following.
    ([\#65](https://github.com/mayoverse/arsenal/issues/65))

  - Fixed a bug in `print.summary.tableby()` with regards to PDF output
    in bookdown.
    ([\#69](https://github.com/mayoverse/arsenal/issues/69))

  - Changed
    [`tests.tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.internal.md)
    to return a data.frame without factors.

  - Fixed a bug in
    [`meansd()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    when all inputs are NA.
    ([\#80](https://github.com/mayoverse/arsenal/issues/80))

  - Fixed a bug with `kwt()`,
    [`anova()`](https://rdrr.io/r/stats/anova.html), and
    [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    formatting when all inputs are NA.
    ([\#81](https://github.com/mayoverse/arsenal/issues/81))

  - Fixed a bug with survival statistics when all inputs are NA.
    ([\#82](https://github.com/mayoverse/arsenal/issues/82))

  - Fixed a bug with `logrank()` when all inputs are NA.
    ([\#83](https://github.com/mayoverse/arsenal/issues/83))

  - Fixed how arguments get passed to stats functions in
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
    In particular, this affected the `times=` option.
    ([\#84](https://github.com/mayoverse/arsenal/issues/84))

  - Added
    [`iqr()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    as a tableby stat option.
    ([\#86](https://github.com/mayoverse/arsenal/issues/86))

  - Fixed quantile functions
    [`q1q3()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`medianq1q3()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    for dates. ([\#87](https://github.com/mayoverse/arsenal/issues/87))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - Fixed a bug in `print.summary.modelsum()` involving the lack of
    wrapping for long labels.
    ([\#59](https://github.com/mayoverse/arsenal/issues/59))

  - Fixed a bug in `print.summary.modelsum()` with regards to knitting
    in R Markdown with plots immediately following.
    ([\#66](https://github.com/mayoverse/arsenal/issues/66))

  - Fixed a bug in `print.summary.modelsum()` with regards to PDF output
    in bookdown.
    ([\#70](https://github.com/mayoverse/arsenal/issues/70))

  - [`as.data.frame.summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    has been implemented, and `print.summary.modelsum()` updated
    accordingly.
    ([\#74](https://github.com/mayoverse/arsenal/issues/74))

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - Fixed a bug in
    [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md)
    with regards to knitting in R Markdown with plots immediately
    following. ([\#67](https://github.com/mayoverse/arsenal/issues/67))

  - Fixed a bug in
    [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md)
    with regards to PDF output in bookdown.
    ([\#71](https://github.com/mayoverse/arsenal/issues/71))

- Other:

  - [`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
    now has dots, and the factor method gained a `first=` argument.
    ([\#62](https://github.com/mayoverse/arsenal/issues/62))

  - [`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md)
    also gained a numeric method, especially for use in
    [`freqlist.formula()`](https://mayoverse.github.io/arsenal/reference/freqlist.md).
    ([\#78](https://github.com/mayoverse/arsenal/issues/78))

  - Fixed a bug in `print.summary.compare.data.frame()` with regards to
    PDF output in bookdown.
    ([\#72](https://github.com/mayoverse/arsenal/issues/72))

## arsenal 1.0.0

CRAN release: 2018-02-02

**This is a non-backwards-compatible update.**

Major changes:

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    is now an S3 generic.
    ([\#35](https://github.com/mayoverse/arsenal/issues/35))

  - The first argument to
    [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    has changed from `tab=` to `object=`, for S3 consistency.
    ([\#35](https://github.com/mayoverse/arsenal/issues/35))

  - [`freqlist.formula()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    was implemented, piggybacking off of
    [`stats::xtabs()`](https://rdrr.io/r/stats/xtabs.html).
    ([\#35](https://github.com/mayoverse/arsenal/issues/35))

  - The `title=` argument was added to
    [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md).
    Passing `caption=` through the dots to
    [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) will
    now throw an error.
    ([\#34](https://github.com/mayoverse/arsenal/issues/34))

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
    has been totally overhauled. It now uses list-columns to give exact
    values.

  - [`summary.tableby()`](https://mayoverse.github.io/arsenal/reference/summary.tableby.md)
    has been totally overhauled.

    - Most arguments are no longer named, but passed through the dots.

    - It now returns an object, abusing in the process
      [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md).
      `print.summary.tableby()` prints the resulting object.
      ([\#8](https://github.com/mayoverse/arsenal/issues/8))

    - `print.summary.tableby()` now uses
      [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to
      print results, instead of internal functions. As such,
      non-exported helper functions have all been removed.

  - The arguments to
    [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
    have changed. Warnings will be issued for using old arguments.

    - `nsmall=` has been removed. `digits=` takes its place.

    - `nsmall.pct=` and `digits.test=` have been renamed to
      `digits.pct=` and `digits.p=`, respectively.

    - There’s now an option for count digits (`digits.count=`).

    - `format.p=` has been added, to turn on formatting of p-values.

    - `q1q3` is no longer a default continuous statistic.

  - NAs can be included in percents using
    [`includeNA()`](https://mayoverse.github.io/arsenal/reference/NA.operations.md).
    ([\#57](https://github.com/mayoverse/arsenal/issues/57))

  - Some additional survival summary functions are now available.
    ([\#32](https://github.com/mayoverse/arsenal/issues/32))

  - It is now possible to report row-percents using
    [`countrowpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
    ([\#9](https://github.com/mayoverse/arsenal/issues/9))

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    has been totally overhauled:

    - It now uses
      [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
      and unevaluated calls instead of custom-creating data.frames.

    - It now allows for non-syntactic names
      ([\#44](https://github.com/mayoverse/arsenal/issues/44),
      [\#45](https://github.com/mayoverse/arsenal/issues/45)).

  - [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)
    has been totally overhauled. It now gives exact values instead of
    formatted values.

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    has been totally overhauled.

    - Most arguments are no longer named, but passed through the dots.

    - It now returns an object, abusing in the process
      [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md).
      `print.summary.modelsum()` prints the resulting object.
      ([\#37](https://github.com/mayoverse/arsenal/issues/37))

    - `print.summary.modelsum()` now uses
      [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to
      print results, instead of internal functions. As such,
      non-exported helper functions have all been removed.

    - `print.summary.modelsum()` now strips leading and trailing
      whitespace from labels to fix formatting with `text=FALSE`.
      ([\#48](https://github.com/mayoverse/arsenal/issues/48))

    - `labelTranslations=` no longer accepts labels for the statistics
      columns. Use `modelsum.control(stat.labels=)` for this instead.

  - The arguments to
    [`modelsum.control()`](https://mayoverse.github.io/arsenal/reference/modelsum.control.md)
    have changed. Warnings will be issued for using old arguments.

    - `nsmall=` has been removed. `digits=` takes its place.

    - `nsmall.ratio=` and `digits.test=` have been renamed to
      `digits.ratio=` and `digits.p=`, respectively.

    - `format.p=` has been added, to turn off formatting of p-values.

    - `stat.labels=` has been added, to label the statistics columns.

  - `"[.modelsum"()` now has a named argument, and accepts character,
    numeric, and logical subscripts.

Smaller changes:

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md):

  - [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
    will no longer issue a warning about using the deprecated
    `varnames=` argument.

  - `print.freqlist()` has been made slightly more concise. The only
    change to the printed output is making “variables” singular
    (“variable”) when only one variable is present.

- [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md):

  - [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
    has also been made slightly more concise and easier to read.

  - A bug was fixed when trying to specify “stats” attributes for
    categorical variables.
    ([\#39](https://github.com/mayoverse/arsenal/issues/39))

  - A bug was fixed relating to unnamed passing of arguments for
    [`medianrange()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
    ([\#49](https://github.com/mayoverse/arsenal/issues/49))

  - [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
    no longer breaks with date ranges.
    ([\#10](https://github.com/mayoverse/arsenal/issues/10))

  - [`as.data.frame.tableby()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.tableby.md)
    no longer breaks with both
    [`count()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    and
    [`countpct()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md).
    ([\#51](https://github.com/mayoverse/arsenal/issues/51))

  - `labels<-.tableby()` no longer breaks for unmatched variables.
    ([\#53](https://github.com/mayoverse/arsenal/issues/53))

  - `labels<-.tableby()` now accepts `NULL` to set all labels to NULL.
    ([\#52](https://github.com/mayoverse/arsenal/issues/52))

  - The function
    [`Nmiss2()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
    is now exported for
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
    Note that it is exactly the same as
    [`Nmiss()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md),
    but is interpreted differently in
    [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).

- [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md):

  - [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    has been made slightly more concise.

  - “Nmiss2” has been added to the
    [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
    object and no longer replaces “Nmiss”.

  - [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)
    no longer turns “\<0.001” into `NA`.
    ([\#31](https://github.com/mayoverse/arsenal/issues/31))

  - [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)
    no longer breaks if there are too many adjustment variables.
    ([\#12](https://github.com/mayoverse/arsenal/issues/12))

  - [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md)
    now has working labels for factors.
    ([\#13](https://github.com/mayoverse/arsenal/issues/13))

  - `"labels<-.modelsum"()` has been tweaked slightly. The results
    shouldn’t change.

  - `print.modelsum()` has been fixed to show its y-variable.
    ([\#33](https://github.com/mayoverse/arsenal/issues/33))

- Documentation and vignettes have been re-reviewed and updated where
  appropriate.

- Tests have been updated to reflect major changes.

## arsenal 0.6.1

CRAN release: 2017-12-08

This is a patch to fix an error appearing with R-devel. We anticipate
releasing v1.0.0 soon, which will not be backwards-compatible.

- Re-fix trailing whitespace problem in tableby.
  ([\#3](https://github.com/mayoverse/arsenal/issues/3))

## arsenal 0.6.0

CRAN release: 2017-09-20

- Updated
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
  to be more efficient.
  ([\#20](https://github.com/mayoverse/arsenal/issues/20))

- [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
  now allows named labels.

- Fixed one-sided formula detection in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  when used with
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md).
  ([\#21](https://github.com/mayoverse/arsenal/issues/21))

- Changed environment of formula returned by
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md).

- Added variable-name subsetting to `[.tableby()`. One can now subset by
  logicals, numerics, or character vectors.

- Fixed a bug in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  related to zero-length factor levels.
  ([\#22](https://github.com/mayoverse/arsenal/issues/22))

- Fixed a bug in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  and
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
  when calling them without loading the package.
  ([\#25](https://github.com/mayoverse/arsenal/issues/25))

- Allowed `nsmall =` and `digits =` to be 0, for rounding to integers.
  ([\#23](https://github.com/mayoverse/arsenal/issues/23))

- Added
  [`yaml()`](https://mayoverse.github.io/arsenal/reference/yaml.md)
  function to use with
  [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md).
  ([\#28](https://github.com/mayoverse/arsenal/issues/28))

- Added the `yaml` package as a suggested package.

- Added
  [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  methods for `compare.data.frame()` objects.
  ([\#27](https://github.com/mayoverse/arsenal/issues/27))

- Updated documentation.

## arsenal 0.5.0

CRAN release: 2017-08-14

- Code all now in GitHub; issues can now be submitted there. Checking is
  now performed automatically on Travis-CI.

- Included documentation for getting a caption with
  [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  and
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md).
  ([\#16](https://github.com/mayoverse/arsenal/issues/16))

- Fixed subsetting in
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md).
  ([\#14](https://github.com/mayoverse/arsenal/issues/14))

- Fixed multiple class errors in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).
  ([\#17](https://github.com/mayoverse/arsenal/issues/17))

- Fixed subset dropping labels in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  and
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
  with new function
  [`keep.labels()`](https://mayoverse.github.io/arsenal/reference/keep.labels.md)
  to make labels “stick”.
  ([\#1](https://github.com/mayoverse/arsenal/issues/1))

- Added a vignette discussing labels.

- Add `compare.data.frame()`, with an accompanying vignette.

## arsenal 0.4.2

- Updated `labelTranslation` documentation

- Changed `format.translations` list to `defaultLabelTranslations()`
  function, and removed labels for sex and age.

- Tweaked `labels<-.freqlist` to allow for list input.

## arsenal 0.4.1

- Updated
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  vignette.

## arsenal 0.4.0

- Tweaked
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md)
  to behave better with data.frame subsetting and the infamous
  `drop=TRUE`.

- Added `dupLabels=` argument to
  [`summary.freqlist()`](https://mayoverse.github.io/arsenal/reference/summary.freqlist.md).
  ([\#6](https://github.com/mayoverse/arsenal/issues/6))

- Added a label for
  [`medianq1q3()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
  in
  [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md).
  ([\#4](https://github.com/mayoverse/arsenal/issues/4))

- Changed the `...` vs. `control=` action in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  and
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
  to fix which arguments are used over which other arguments.
  ([\#5](https://github.com/mayoverse/arsenal/issues/5))

- Moved import `broom` and `stringr` to “Suggests”, adding `magrittr`
  for piping.

- Added piping to `write2*()` vignette.

- Several
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  fixes for counts and percents.

- New options in
  [`tableby.control()`](https://mayoverse.github.io/arsenal/reference/tableby.control.md)
  to modify the statistical tests performed.
  ([\#2](https://github.com/mayoverse/arsenal/issues/2))

- Fixed trailing whitespace issue in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  ([\#3](https://github.com/mayoverse/arsenal/issues/3))

## arsenal 0.3.0

CRAN release: 2017-03-09

- The CRAN release of the locally stable 0.2.3. For NEWS on this
  version, see below.

- Tweaked the DESCRIPTION to include
  [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md).

## arsenal 0.2.3

- Fixed ordered stats in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md).

- Fixed a problem with
  [`as.data.frame.modelsum()`](https://mayoverse.github.io/arsenal/reference/as.data.frame.modelsum.md)
  introduced in v0.2.1.

## arsenal 0.2.2

- Added
  [`count()`](https://mayoverse.github.io/arsenal/reference/tableby.stats.md)
  function for tableby stats.

- Two problems with survival models in
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md)
  have been resolved.

## arsenal 0.2.1

- [`write2.list()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  has been implemented, allowing multiple tables output into a single
  document.
  [`write2.verbatim()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  has been implemented, allowing monospaced output. The vignette has
  been updated along with all documentation.

- The [`summary()`](https://rdrr.io/r/base/summary.html) output for
  `tableby` and `modelsum` objects now prints an extra blank header
  line, for better use inside R Markdown code chunks.

- Two bugs in
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md)
  were corrected.

## arsenal 0.2.0

CRAN release: 2017-01-31

- Vignettes have been updated.

- [`write2()`](https://mayoverse.github.io/arsenal/reference/write2.md)
  is now exported and supports all output formats supported by
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).
  There is now a vignette for it and the S3 methods have been expanded
  to handle more inputs, including
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html),
  [`xtable::xtable()`](https://rdrr.io/pkg/xtable/man/xtable.html), and
  [`pander::pander_return()`](https://rdrr.io/pkg/pander/man/pander_return.html).

- Fixed a bug in
  [`summary.modelsum()`](https://mayoverse.github.io/arsenal/reference/summary.modelsum.md).

## arsenal 0.1.2

CRAN release: 2016-12-30

- `broom` and `stringr` have been moved to `imports` instead of
  `depends`.

- Several minor tweaks to `freqlist` for better readability and
  performance.

## arsenal 0.1.1

- The description and the title are more descriptive now, per request.

## arsenal 0.1.0

- First release contains major functions
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md),
  [`formulize()`](https://mayoverse.github.io/arsenal/reference/formulize.md),
  and `write2...`.

- Vignettes are included for
  [`tableby()`](https://mayoverse.github.io/arsenal/reference/tableby.md),
  [`modelsum()`](https://mayoverse.github.io/arsenal/reference/modelsum.md),
  and
  [`freqlist()`](https://mayoverse.github.io/arsenal/reference/freqlist.md).
