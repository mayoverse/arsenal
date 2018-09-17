# arsenal v1.3.0.9000

* Added error to `na.paired("in.both")` when there are more than two time points.

* `tableby()`, `paired()`: Add warning for when by-variable contains empty string. (#121)

* `compare()`: implement `n.diff.obs()`. (#124)

* `summary.compare()`: allow for the display of attributes. (#125)

* `tableby()`, `modelsum()`: properly propogated "term.name" to the `as.data.frame()` method. (#127, #128)

* `tableby()`, `modelsum()`: Allow for LaTeX formatting. NOTE: this changes the formatting behavior when specifying
  `text="html"`! (#123)
  
* `modelsum()`: Fix formatting of error about unsupported families.

* `modelsum()`: Add `family="ordinal"` to do ordinal logistic regression using `MASS::polr()`. (#130)

* `modelsum()` now supports *calls* to the family functions, in case a different link function (for example) is required.

* `modelsum()`: removed "concordance" from the list of supported statistics for Poisson regression.

* `modelsum()`: added support for ordinal regressors and adjustment terms (by adding support for their associated contrasts). (#133)

* `modelsum()`: fixed a bug with formatting one-per-model p-values. (#140)

* `tableby()`: fixed an error that sometimes occured when using categorical statistics on numeric variables. (#137)

* `tableby.control()`: gained an argument to simplify one-line numeric output. (#139)

* `tableby()`: In-formula functions now allow the specification of `digits=` (etc.), `numeric.simplify=`, and `cat.simplify=`
  for a single variable. (#107, #134, #139)  NB: this has the following breaking changes:
    
    - There is no longer a "name" element in the "tableby" object's x-specifications; instead it's now called "term"
    
    - An element for "variable", containing the variable name, was added to the "tableby" object's x-specifications.
    
    - An element for "control.list", recording format specifications, was added to the "tableby" object's x-specifications.
    
    - The output of `as.data.frame.tableby()` now reports only the variable name in the "variable" column when using
      internal statistical functions (like `anova()` and `chisq()`). It used to include the function call as well.
      
    - The output of `as.data.frame.tableby()` no longer includes category levels in the "term" column;
      instead, it contains the statistical function used (like `countpct()` and `count()`).
      
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

* `modelsum()`:

    - Implemented `is.modelsum()` and `is.summary.modelsum()`. (#111)
    
    - `summary.modelsum()`: ignore row.names when printing summary objects.
    
    - `summary.modelsum()` and `as.data.frame.summary.modelsum()`: added a `term.name=` argument. (#109)
    
    - Implemented `Ops.tableby()` to compare tableby objects to a number (p-value). (#96)
    
    - Implemented `xtfrm.tableby()`, so that tableby objects can be sorted by p-value. (#96)
    
    - Implemented `length.tableby()`, so that `head()` and `tail()` also work. (#97)
    
    - Implemented `countcellpct()` for counts and cell percentages. (#106)
    
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

