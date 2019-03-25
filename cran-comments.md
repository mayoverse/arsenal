## Submission for arsenal 3.0.0

This is a new version. In this version:

* Renamed `compare()` to `comparedf()`, to avoid conflicting with the objects in the `testthat` package.

* Fixed a major bug in `modelsum()` related to interaction terms.

* Saved out a test dataset instead of building on-the-fly (to solve problems with `sample()` changing in R devel).

* Closed several bugs/feature requests.

* We updated and clarified documentation where appropriate.

## Test environments

* local Linux install: R 3.4.2
* ubuntu 14.04 (on travis-ci): R devel, R 3.5.2 (release), R 3.4.4 (oldrel), R 3.4.2, R 3.3.1, R 3.2.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies.


