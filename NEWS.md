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

