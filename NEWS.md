# arsenal v3.3.0

* `tableby()` / `paired()`:

    - Redid how weights are handled. The only user-visible changes should be that standard deviations
      on length-1 groups are now reported as `NA` instead of `NaN`.
  
    - Fixed a bug with `modpval.tableby()` when factors are involved. (#239)
  
    - Added `meanCI()` and `medianmad()`. (#230, #232)

    - Added the units for `difftime` statistics when using dates (e.g., `meansd`, `medianmad`, `iqr`).

    - Fixed Chi-square and Fisher's Exact test for one-level categorical variables. (#227, #228)

    - Fixed the n's in the header when using weights. (#229)

    - Fixed a bug with confidence levels supplied through the control argument. (#234)

    - `paired()`: fixed a bug when using `count()` with factors. (#235)

    - `tableby.control()`: added explicit `times=` argument for survival summaries.

    - Added option to run statistical tests even if one by-group has 0 observations. (#233, #250, #251)

    - Stopped the formatting of p-values when they're not numeric (if, say, they're pre-formatted by the user). (#249)

* `modelsum()`:

    - Added functionality for multiple adjustor sets. (#240)

    - Fixed "Nmiss" and "N" when used with strata, which now both report the missings for the entire fit. (#241, #242, #243)

    - Suppressed messages from `pROC::auc()` when calculating AUC. (#244)

    - Fixed confidence level for survival models. (#245)

    - Added an option for the likelihood ratio test for the main effect (but not the adjustors): `p.value.lrt` (#238)

    - Blanked out p-values that are NA. (#246)

* `code.chunk()`:

    - Fixed logic checking the length of `chunk.opts=`.

    - Allowed for empty code chunks. (#236)

* `verbatim()`: removed named argument in favor of the dots; reworked the object structure to fix edge case printing oddities. (#248)

* Removed defunct functions.

# arsenal v3.2.0

* `comparedf()`:

    - Fixed a bug when "row.names" was used in combination with other by-variables. (#212)

    - Allowed for comparison of variables which have any class in common. (#216)

    - `summary.comparedf()`:
    
        * Removed the "comparedf.frame.summary" class from the first element to allow it to print. (#211)

        * Fixed a bug with reporting blank by-variables. (#213)

        * Fixed a bug with reporting by-variables as variables in common. (#214)

* `modelsum()`: added Wald confidence intervals to `binom.stats=`. (#219)

* `tableby()` / `paired()`:

    - Fixed a bug with `merge.arsenal_table()` losing control parameters for `tableby()` objects. (#221)

    - Allowed for variable-name-only `labelTranslations=` assignment for terms with inline statistical test specification.
      Backward compatibility should be maintained here. (#220)

    - Fixed a bug with assigning `NULL` labels with inline statistical test specifications. (#222)

    - `summary.tableby()`: fixed a bug with formatting when strata aren't in alphabetical order and have different number of elements
      (e.g., if only one includes missing values). (#215)

# arsenal v3.1.0

* `tableby()` / `paired()`: 

    - Added "Nmiss" to default `surv.stats=` in `tableby.control()`.

    - Fixed a bug when some `Surv()` elements are NA. (#208)
    
    - `tableby.control()`: fixed a bug with simplifying categorical and numeric output. (#199, #203)
      This fix also allows for simplification of custom statistics. (#200)
  
    - `tableby.control()`: added `date.simplify=` and `ordered.simplify=` arguments. (#202)
      The order of arguments has changed slightly for consistency.
  
    - `paired.control()`: took away the arguments that should be the same as `tableby.control()`, and only included arguments with
      new defaults or which don't appear in `tableby.control()`.

    - Added the functions `countN()` and `Nrisk()` (#201). `Nrisk()` now outputs what `NriskSurv()` used to; `NriskSurv()`
      now outputs what its name suggests: the number at risk, and the survival. Additionally, `as.countpct()` gains the `which.pct=` argument,
      whose default of `0` may break the formatting of percents (`digits.pct=`).

* `comparedf()`:

    - Added additional summary table to the `summary()` output.

    - Moved the `max.print...=` arguments to `comparedf.control()`. `max.print.diff=` is now deprecated and is replaced
      by `max.print.diffs.per.var=`. `max.print.diffs=` was also added to control overall number of differences printed.

    - Fixed a bug with numeric percent tolerances when both values being compared are 0. (#206)

    - Fixed a bug in `diffs()` (and hence `summary()`) when no variables are compared (#207). Note that this change
      also included a change to the by-variables reported in the `comparedf()` object when merging over row.names.

# arsenal v3.0.0

**There are a few non-backwards-compatible updates.**

Major changes:

* Renamed `compare()` -> `comparedf()` and `comparison.control()` -> `comparedf.control()`. (#179)

* `modelsum()`: Fixed bug(s) with interaction terms. (#173, #177)

* Added a new function `loosen.labels()` which removes the classes added by `keep.labels()` and thereby speeds up subsetting when
  labels are no longer needed. This is now used in `tableby()`, `modelsum()`, `freqlist()`, and `paired()`.

Smaller changes:

* `tableby()` / `paired()`:

    - Fixed two bugs relating to `modpval.tableby()`: one which didn't properly assign the p-value name (#174),
      and one which broke `as.data.frame()` when assigning custom p-values for only one strata (#175).
      
    - These now issue informative error when class isn't recognized. (#180)

    - Fixed two bugs in the `tableby()` vignette: `modpval.tableby()` wasn't working properly (#170), and
      `pfootnote=TRUE` was commented out (#169).

    - Fixed a bug with per-variable stats and digit specifications being lost when using the `subset=` argument. (#182, #183)

    - Made all-NA summaries prettier. (#190)

    - This now issues a warning when `coin` isn't available for the trend test. (#193)

* `comparedf()`:

    - This now allows for zero-row data.frames. (#166)
    
    - `comparedf.control()` now allows for named `tol.vars=` argument to manually match column names. (#165)

* `freqlist()`:

    - Fixed a bug where labels would get dropped when using the `subset=` argument. (#184)

    - Fixed a bug where labels were lost when subsetting the table and using strata terms. (#196)

    - `freqlist()`: implemented a `sort()` method to sort tables on frequency. (#187)

    - `summary.freqlist()`: Implemented `head()` and `tail()`. (#188)

    - `summary.freqlist()`: fixed a bug when all table counts are 0 and `sparse=FALSE`. (#186, #194)

* `keep.labels()`:

    - This no longer sticks another class on data.frames.
    
    - Fixed a bug with replacement for objects of class `"keep_labels"`.
    
* `formulize()`: added the `collapse=` and `collapse.y=` arguments. (#197)

# arsenal v2.0.0

There is a new class system (`"arsenal_table"`) which unifies `tableby()`, `modelsum()`, and `freqlist()`.

* `arsenal` now imports and re-exports `utils::head()` and `utils::tail()`.

* `arsenal` now has a sticker!

* `arsenal_table`:

    - Implemented a new class (without a constructor).
    
    - `labels<-.arsenal_table()` doesn't support unnamed labels, as it's unclear how to assign them to multiple by-variables and strata.
      It also doesn't give warnings if your labels are not used.
      
    - `[.arsenal_table()` has an argument `j=` to select the by-variables.
    
    - `merge.arsenal_table()` has arguments to select which by-variables to keep if not all are in common. It also checks to make sure
      that strata, weights, and by-variables are all identical.
      
    - `print.arsenal_table()` shows y- and x-variables, plus any strata.

* `tableby()` and `paired()`:

    - Added functionality for multiple by-variables and strata terms. This required completely reworking the innards of the `tableby` object.

    - Removed `length.tableby()` (because it was messing up `str()` and R Studio) and replaced with `head.tableby()` and `tail.tableby()` (the original
      purpose to having `length()` defined).
    
    - Implemented `sort.tableby()`, which errors out if the object has strata or multiple by-variables, and then runs the default method.
    
    - `modpval.tableby()` now requires the first column to be the by-variable, and if the object has a strata, the second column is required
      to be the corresponding strata value.
      
    - `tests.tableby()` now returns a data.frame with a by-variable column and (if applicable) a strata column.
    
    - `na.tableby()` now generates functions. The "lhs=" argument determines whether to remove NAs from the first column of the data.
      If `tableby()` detects a one-sided formula, it sets this to FALSE. Both versions now remove rows with NAs in the strata column (when applicable).
      
    - `na.paired()` now removes rows with NAs in the strata column (when applicable).
      
    - `padjust.tableby()` and `padjust.summary.tableby()` will error if fed an object with strata or multiple by-variables.
    
    - `as.data.frame.tableby()` and `as.data.frame.summary.tableby()` gain the `list.ok=` argument, for when there are multiple left-hand-sides.
    
    - Added logic to statistical tests to detect missing levels of the by-variable.
    
    - Fixed a bug with LaTeX formatting involving the `align=` argument to `knitr::kable()`.
    
    - Passing `term.name=TRUE` to `summary.tableby()` or `as.data.frame.summary.tableby()` will now put the term name in the top left corner
      of each table.

* `modelsum()`:

    - Added functionality for multiple by-variables and strata terms. This required completely reworking the innards of the `modelsum` object.
    
    - `na.modelsum()` now removes rows with NAs in the strata column (when applicable).
    
    - `as.data.frame.modelsum()` and `as.data.frame.summary.modelsum()` gain the `list.ok=` argument, for when there are multiple left-hand-sides.
    
    - Passing `term.name=TRUE` to `summary.modelsum()` or `as.data.frame.summary.modelsum()` will now put the term name in the top left corner
      of each table.
      
* `freqlist()`:

    - Added functionality for multiple by-variables. This required completely reworking the innards of the `freqlist` object.
    
    - Changed the argument `groupBy=` to `strata=` to match `tableby()` and `modelsum()`.
    
    - Added `merge.freqlist()` and `as.data.frame.summary.freqlist()`. Note that `[.arsenal_table()` now allows you to
      remove the cumulative and percent columns.
    
    - Note that `labels<-.arsenal_table()` no longer supports unnamed labels, but now accepts labels for the frequency,
      cumulative, and percent columns for `freqlist` objects.
    
    - Removed the `digits=`, `sparse=`, `single=`, and `dupLabels=` arguments from `freqlist()` and `summary.freqlist()`. These are now
      arguments to the new `freq.control()`, and are passed through the dots (for backwards compatibility). `freqlist()` also gained
      the `control=` argument for objects from `freq.control()`.
      
    - `as.data.frame.freqlist()` no longer rounds its digits, nor does it label its columns. Use `as.data.frame.summary.freqlist()` for that instead.
      It also gained the `list.ok=` argument, for when there are multiple left-hand-sides.
      
* `includeNA()`: removed the "character" and "numeric" methods, replacing them with a default. In particular, this changes the default label of
  what used to be `includeNA.numeric()`.
  
* `write2()`:

    - Changed the output to an `.Rmd` file instead of a `.md`. This shouldn't break anything, unless you're relying on the intermediate file.
    
    - Replaced the `keep.md=` argument with `keep.rmd=` (since we're not using `.md` files directly anymore).
    
    - Added the function `code.chunk()` to write executable code chunks to the `.Rmd`.

# arsenal v1.5.0

* `tableby()` and `paired()`:

    - fixed a bug with specifying individual statistics for character and logical vectors. (#142)

    - `tableby()` and `paired()`: added a function (`notest()`) to prevent performing a test on an individual variable. (#144)

    - `summary.tableby()`: changed NA p-values to blanks. (#145)

    - `summary.tableby()`: added documentation on `bookdown`. (#147)
    
    - Wrote `padjust()`, an S3 wrapper for `p.adjust()`, which can also adjust `tableby()` (and hence `paired()`) objects. (#146)
    
    - `print.summary.tableby()`, `as.data.frame.summary.tableby()`: added `width=` and `min.split=` as formal arguments.
    
    - Fixed `medSurv()` which was calculating the median survival incorrectly, and removed `rangeTime()`, an ambiguous survival statistic. (#32)

* `modelsum()`:

    - `summary.modelsum()`: added documentation on `bookdown`. (#147)
    
    - `print.summary.modelsum()`, `as.data.frame.summary.modelsum()`: added `width=` and `min.split=` as formal arguments.
    
* `summary.freqlist()`: added documentation on `bookdown`. (#147)

* `formulize()`: added support for names and calls. (#152, #153)

# arsenal v1.4.0

**There are a few non-backwards-compatible updates.**

Major changes:

* `modelsum()`:

    - Added `family="ordinal"` to do ordinal logistic regression using `MASS::polr()`. (#130)

    - Added `family="negbin"` to do negative binomial regression using `MASS::glm.nb()`. (#15)
  
    - Added support for ordinal regressors and adjustment terms (by adding support for their associated contrasts). (#133)
    
    - Allowed for LaTeX formatting. NOTE: this changes (hence possibly breaking old code) the formatting behavior when specifying `text="html"`. (#123)

* `summary.compare.data.frame()`: Added a small summary of the input data.frames as the first table. (#126)
  NOTE: this changes the structure and printed output of `summary()`!
  
* `tableby()`:

    - Allowed for LaTeX formatting. NOTE: this changes (hence possibly breaking old code) the formatting behavior when specifying `text="html"`. (#123)

    - Added functionality to in-formula functions to allow the specification of `digits=` (etc.), `numeric.simplify=`, and `cat.simplify=`
      for a single variable. (#107, #134, #139)  NB: this has the following breaking changes:
    
        * There is no longer a "name" element in the "tableby" object's x-specifications; instead it's now called "term"
    
        * An element for "variable", containing the variable name, was added to the "tableby" object's x-specifications.
    
        * An element for "control.list", recording format specifications, was added to the "tableby" object's x-specifications.
    
        * The output of `as.data.frame.tableby()` now reports only the variable name in the "variable" column when using
          internal statistical functions (like `anova()` and `chisq()`--it used to include the function call as well).
      
        * The output of `as.data.frame.tableby()` no longer includes category levels in the "term" column;
          instead, it contains the statistical function used (like `countpct()` and `count()`).
      
Smaller changes:

* `modelsum()`:

    - Added support for *calls* to the family functions, in case a different link function (for example) is required.
    
    - Properly propogated "term.name" to the `as.data.frame()` method. (#128)
    
    - Fixed formatting of error about unsupported families.
    
    - Removed "concordance" from the list of supported statistics for Poisson regression. This shouldn't break much code, as specifying "concordance"
      wouldn't have shown anything anyway.
      
    - Fixed a bug with formatting one-per-model p-values. (#140)
    
* `tableby()`:

    - Added a warning for when by-variable contains empty string. (#121)
    
    - Properly propogated "term.name" to the `as.data.frame()` method. (#127)
    
    - Fixed an error that sometimes occured when using categorical statistics on numeric variables. (#137)
    
    - Added an argument to `tableby.control()` to simplify one-line numeric output. (#139)
    
* `paired()`:

    - Added a warning for when by-variable contains empty string. (#121)
    
    - Added error to `na.paired("in.both")` when there are more than two time points.

* `compare.data.frame()`:

    - Implemented `n.diff.obs()`. (#124)
    
    - Added an argument to `summary()` to allow for the display of attributes. (#125)

    - Fixed `summary()` to now return the object passed it. (#141)
      
* Updated documentation where appropriate.

# arsenal v1.3.0

**This is a mostly backwards-compatible update.**

Major changes:

* Implemented the function `paired()` for paired data, based on `tableby()`. This comes with a very light vignette.

* `tableby()`: Change default for chi-square tests to `correct=FALSE`. Note that this only affects the 2 x 2 case.

Smaller changes:

* Added the a default method for label assignment (`labels<-`). (#118)

* Update `formulize()` to handle non-syntactic names in the `data=` argument. (#105)

* `tableby()`:

    - Implemented `is.tableby()` and `is.summary.tableby()`. (#112)
    
    - Changed how arguments are passed to stat tests.
    
    - Issue a warning if statistical tests are requested when there are fewer than two by-levels. (#108)
    
    - Fixed `trend()` and `anova()` to return an object instead of the object being invisible.
    
    - Implemented the stat functions `binomCI()` and `rowbinomCI()` for binomial confidence intervals. (#117)
    
    - `summary.tableby()`: ignore row.names when printing summary objects.
    
    - `summary.tableby()` and `as.data.frame.summary.tableby()`: added a `term.name=` argument. (#110)
    
    - `summary.tableby()`: pass `text="html"` to get better formatting in R shiny. (#114)
    
    - Implemented `Ops.tableby()` to compare tableby objects to a number (p-value). (#96)
    
    - Implemented `xtfrm.tableby()`, so that tableby objects can be sorted by p-value. (#96)
    
    - Implemented `length.tableby()`, so that `head()` and `tail()` also work. (#97)
    
    - Implemented `countcellpct()` for counts and cell percentages. (#106)

* `modelsum()`:

    - Implemented `is.modelsum()` and `is.summary.modelsum()`. (#111)
    
    - `summary.modelsum()`: ignore row.names when printing summary objects.
    
    - `summary.modelsum()` and `as.data.frame.summary.modelsum()`: added a `term.name=` argument. (#109)
    
    - Allow for `weights=` and `na.action=`. (#99)
    
    - Fixed problem with column names which are prefixes of other column names. (#98)
    
    - Fixed problem with column labels overwriting categorical levels which also match. (#100)
    
    - `summary.modelsum()`: pass `text="html"` to get better formatting in R shiny. (#115)
    
* `freqlist()`:

    - Implemented `is.freqlist()` and `is.summary.freqlist()`. (#113)
    
    - Fixed a problem with a column named "method". (#95)
    
    - `summary.freqlist()`: ignore row.names when printing summary objects.

* Update documentation.

# arsenal 1.2.0

* Implemented `write2()` methods for `"summary.tableby"`, `"summary.modelsum"`, and `"summary.freqlist"` objects. (#89, #90, #91)

* Center-aligned `tableby()` grouping columns in the summary output. (#93)

# arsenal 1.1.0

**This is a mostly backwards-compatible update.**

Major changes:

* `summary.freqlist()` now returns an object. `print.summary.freqlist()` prints the resulting object. (#76)

Smaller changes:

* `tableby()`:

    - Fixed a bug in `print.summary.tableby()` involving the lack of wrapping for long labels. (#59)
    
    - `as.data.frame.summary.tableby()` has been implemented, and `print.summary.tableby()` updated accordingly. (#60)
    
    - Fixed a bug with assigning labels for tableby objects when some value names are unmatched. (#64)
    
    - Fixed a bug in `print.summary.tableby()` with regards to knitting in R Markdown with plots immediately following. (#65)
    
    - Fixed a bug in `print.summary.tableby()` with regards to PDF output in bookdown. (#69)
    
    - Changed `tests.tableby()` to return a data.frame without factors.
    
    - Fixed a bug in `meansd()` when all inputs are NA. (#80)
    
    - Fixed a bug with `kwt()`, `anova()`, and `summary.tableby()` formatting when all inputs are NA. (#81)
    
    - Fixed a bug with survival statistics when all inputs are NA. (#82)
    
    - Fixed a bug with `logrank()` when all inputs are NA. (#83)
    
    - Fixed how arguments get passed to stats functions in `tableby()`. In particular, this affected the `times=` option. (#84)
    
    - Added `iqr()` as a tableby stat option. (#86)
    
    - Fixed quantile functions `q1q3()` and `medianq1q3()` for dates. (#87)
    
* `modelsum()`:

    - Fixed a bug in `print.summary.modelsum()` involving the lack of wrapping for long labels. (#59)

    - Fixed a bug in `print.summary.modelsum()` with regards to knitting in R Markdown with plots immediately following. (#66)
    
    - Fixed a bug in `print.summary.modelsum()` with regards to PDF output in bookdown. (#70)

    - `as.data.frame.summary.modelsum()` has been implemented, and `print.summary.modelsum()` updated accordingly. (#74)
    
* `freqlist()`:

    - Fixed a bug in `summary.freqlist()` with regards to knitting in R Markdown with plots immediately following. (#67)
    
    - Fixed a bug in `summary.freqlist()` with regards to PDF output in bookdown. (#71)
    
* Other:

    - `includeNA()` now has dots, and the factor method gained a `first=` argument. (#62)

    - `includeNA()` also gained a numeric method, especially for use in `freqlist.formula()`. (#78)

    - Fixed a bug in `print.summary.compare.data.frame()` with regards to PDF output in bookdown. (#72)

# arsenal 1.0.0

**This is a non-backwards-compatible update.**

Major changes:

* `freqlist()`:

    - `freqlist()` is now an S3 generic. (#35)
      
    - The first argument to `freqlist()` has changed from `tab=` to `object=`, for S3 consistency. (#35)

    - `freqlist.formula()` was implemented, piggybacking off of `stats::xtabs()`. (#35)
    
    - The `title=` argument was added to `summary.freqlist()`. Passing `caption=` through the dots to `knitr::kable()`
      will now throw an error. (#34)
      
* `tableby()`:

    - `as.data.frame.tableby()` has been totally overhauled. It now uses list-columns to give exact values.
    
    - `summary.tableby()` has been totally overhauled.

        * Most arguments are no longer named, but passed through the dots.
    
        * It now returns an object, abusing in the process `as.data.frame.tableby()`.
          `print.summary.tableby()` prints the resulting object. (#8)
        
        * `print.summary.tableby()` now uses `knitr::kable()` to print results, instead of internal functions.
          As such, non-exported helper functions have all been removed.

    - The arguments to `tableby.control()` have changed. Warnings will be issued for using old arguments.
    
        * `nsmall=` has been removed. `digits=` takes its place.
        
        * `nsmall.pct=` and `digits.test=` have been renamed to `digits.pct=` and `digits.p=`, respectively.
        
        * There's now an option for count digits (`digits.count=`).
        
        * `format.p=` has been added, to turn on formatting of p-values.
        
        * `q1q3` is no longer a default continuous statistic.
        
    - NAs can be included in percents using `includeNA()`. (#57)
    
    - Some additional survival summary functions are now available. (#32)
    
    - It is now possible to report row-percents using `countrowpct()`. (#9)

* `modelsum()`:

    - `modelsum()` has been totally overhauled:
    
        * It now uses `stats::model.frame()` and unevaluated calls instead of custom-creating
          data.frames.
          
        * It now allows for non-syntactic names (#44, #45).

    - `as.data.frame.modelsum()` has been totally overhauled. It now gives exact
      values instead of formatted values.

    - `summary.modelsum()` has been totally overhauled.

        * Most arguments are no longer named, but passed through the dots.
    
        * It now returns an object, abusing in the process `as.data.frame.modelsum()`.
          `print.summary.modelsum()` prints the resulting object. (#37)
        
        * `print.summary.modelsum()` now uses `knitr::kable()` to print results, instead of internal functions.
          As such, non-exported helper functions have all been removed.
          
        * `print.summary.modelsum()` now strips leading and trailing whitespace from labels to fix formatting with `text=FALSE`. (#48)
    
        * `labelTranslations=` no longer accepts labels for the statistics columns.
          Use `modelsum.control(stat.labels=)` for this instead.

    - The arguments to `modelsum.control()` have changed. Warnings will be issued for using old arguments.
    
        * `nsmall=` has been removed. `digits=` takes its place.
        
        * `nsmall.ratio=` and `digits.test=` have been renamed to `digits.ratio=` and `digits.p=`, respectively.
        
        * `format.p=` has been added, to turn off formatting of p-values.
        
        * `stat.labels=` has been added, to label the statistics columns.
    
    - `"[.modelsum"()` now has a named argument, and accepts character, numeric, and logical subscripts.

Smaller changes:

* `freqlist()`:

    - `freqlist()` will no longer issue a warning about using the deprecated `varnames=` argument.

    - `print.freqlist()` has been made slightly more concise. The only change to the printed output
      is making "variables" singular ("variable") when only one variable is present.
    
* `tableby()`:

    - `tableby()` has also been made slightly more concise and easier to read.

    - A bug was fixed when trying to specify "stats" attributes for categorical variables. (#39)
    
    - A bug was fixed relating to unnamed passing of arguments for `medianrange()`. (#49)
    
    - `as.data.frame.tableby()` no longer breaks with date ranges. (#10)

    - `as.data.frame.tableby()` no longer breaks with both `count()` and `countpct()`. (#51)

    - `labels<-.tableby()` no longer breaks for unmatched variables. (#53)
    
    - `labels<-.tableby()` now accepts `NULL` to set all labels to NULL. (#52)

    - The function `Nmiss2()` is now exported for `tableby()`. Note that it is exactly the same as `Nmiss()`, but
      is interpreted differently in `tableby()`.
      
* `modelsum()`:

    - `modelsum()` has been made slightly more concise.
    
    - "Nmiss2" has been added to the `modelsum()` object and no longer replaces "Nmiss".
    
    - `as.data.frame.modelsum()` no longer turns "<0.001" into `NA`. (#31)
    
    - `as.data.frame.modelsum()` no longer breaks if there are too many adjustment variables. (#12)
    
    - `summary.modelsum()` now has working labels for factors. (#13)
    
    - `"labels<-.modelsum"()` has been tweaked slightly. The results shouldn't change.
    
    - `print.modelsum()` has been fixed to show its y-variable. (#33)
    
* Documentation and vignettes have been re-reviewed and updated where appropriate.

* Tests have been updated to reflect major changes.

# arsenal 0.6.1

This is a patch to fix an error appearing with R-devel. We anticipate releasing v1.0.0 soon, which
will not be backwards-compatible.

* Re-fix trailing whitespace problem in tableby. (#3)

# arsenal 0.6.0

* Updated `freqlist()` to be more efficient. (#20)

* `freqlist()` now allows named labels.

* Fixed one-sided formula detection in `tableby()` when used with `formulize()`. (#21)

* Changed environment of formula returned by `formulize()`.

* Added variable-name subsetting to `[.tableby()`. One can now subset by logicals, numerics, or character vectors.

* Fixed a bug in `tableby()` related to zero-length factor levels. (#22)

* Fixed a bug in `tableby()` and `modelsum()` when calling them without loading the package. (#25)

* Allowed `nsmall = ` and `digits = ` to be 0, for rounding to integers. (#23)

* Added `yaml()` function to use with `write2()`. (#28)

* Added the `yaml` package as a suggested package.

* Added `write2()` methods for `compare.data.frame()` objects. (#27)

* Updated documentation.

# arsenal 0.5.0

* Code all now in GitHub; issues can now be submitted there. Checking is now performed automatically on Travis-CI.

* Included documentation for getting a caption with `write2()` and `freqlist()`. (#16)

* Fixed subsetting in `modelsum()`. (#14)

* Fixed multiple class errors in `tableby()`. (#17)

* Fixed subset dropping labels in `tableby()` and `modelsum()` with new function `keep.labels()` to make labels "stick". (#1)

* Added a vignette discussing labels.

* Add `compare.data.frame()`, with an accompanying vignette.

# arsenal 0.4.2

* Updated `labelTranslation` documentation

* Changed `format.translations` list to `defaultLabelTranslations()` function, and removed labels for sex and age.

* Tweaked `labels<-.freqlist` to allow for list input.

# arsenal 0.4.1

* Updated `tableby()` vignette.

# arsenal 0.4.0

* Tweaked `freqlist()` to behave better with data.frame subsetting and the infamous `drop=TRUE`.

* Added `dupLabels=` argument to `summary.freqlist()`. (#6)

* Added a label for `medianq1q3()` in `tableby.control()`. (#4)

* Changed the `...` vs. `control=` action in `tableby()` and `modelsum()` to fix which
  arguments are used over which other arguments. (#5)
  
* Moved import `broom` and `stringr` to "Suggests",
  adding `magrittr` for piping.
  
* Added piping to `write2*()` vignette.
  
* Several `tableby()` fixes for counts and percents.

* New options in `tableby.control()` to modify the statistical tests performed. (#2)

* Fixed trailing whitespace issue in `tableby()` (#3)

# arsenal 0.3.0

* The CRAN release of the locally stable 0.2.3. For NEWS on this version, see below.

* Tweaked the DESCRIPTION to include `write2()`.

# arsenal 0.2.3

* Fixed ordered stats in `tableby()`.

* Fixed a problem with `as.data.frame.modelsum()` introduced in v0.2.1.

# arsenal 0.2.2

* Added `count()` function for tableby stats.

* Two problems with survival models in `modelsum()` have been resolved.

# arsenal 0.2.1

* `write2.list()` has been implemented, allowing multiple tables output into a single document.
  `write2.verbatim()` has been implemented, allowing monospaced output.
  The vignette has been updated along with all documentation.
  
* The `summary()` output for `tableby` and `modelsum` objects now prints an extra blank header line,
  for better use inside R Markdown code chunks.
  
* Two bugs in `tableby()` were corrected.

# arsenal 0.2.0

* Vignettes have been updated.

* `write2()` is now exported and supports all output formats supported by `rmarkdown::render()`. There is now a vignette for it
  and the S3 methods have been expanded to handle more inputs, including `knitr::kable()`, `xtable::xtable()`, and `pander::pander_return()`.
  
* Fixed a bug in `summary.modelsum()`.

# arsenal 0.1.2

* `broom` and `stringr` have been moved to `imports` instead of `depends`.

* Several minor tweaks to `freqlist` for better readability and performance.

# arsenal 0.1.1

* The description and the title are more descriptive now, per request.

# arsenal 0.1.0

* First release contains major functions `tableby()`, `modelsum()`, `freqlist()`, `formulize()`, and `write2...`.

* Vignettes are included for `tableby()`, `modelsum()`, and `freqlist()`.

