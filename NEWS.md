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

