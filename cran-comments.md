## Submission for arsenal 0.2.?

This is a new version. In this version:

* `write2.list()` has been implemented, allowing multiple tables output into a single document.
  `write2.verbatim()` has been implemented, allowing monospaced output.
  The vignette has been updated along with all documentation.
  
* The `summary()` output for `tableby` and `modelsum` objects now prints an extra blank header line,
  for better use inside R Markdown code chunks.
    
* Two bugs in `tableby()` were corrected.

* Two problems with survival models in `modelsum()` have been resolved.

* Added `count()` function for tableby stats.


## Test environments
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.


