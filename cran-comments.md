## Submission for arsenal 0.2.0

This is a new version. In this version we have:

* Updated vignettes.

* Exported `write2()`, which now supports all output formats supported by `rmarkdown::render()`. There is now a vignette for it
  and the S3 methods have been expanded to handle more inputs, including `knitr::kable()`, `xtable::xtable()`, and `pander::pander_return()`.
  
* Fixed a bug in `summary.modelsum()`.


## Test environments
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.


