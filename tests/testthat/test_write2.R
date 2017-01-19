## Tests for write2


context("Testing the write2 output")

data(mockstudy)

expect_write2_worked <- function(FUN, object, reference, ...)
{
  FUN <- match.fun(FUN)
  filename <- tempfile(fileext = ".html")
  on.exit(expect_true(file.remove(filename)))
  FUN(object, file = filename, ...)
  generated <- readLines(filename)
  expect_output_file(cat(generated, sep = "\n"), reference)
}

###########################################################################################################
#### HTML output
###########################################################################################################

test_that("write2.tableby -> HTML", {
  expect_write2_worked(write2html, tableby(arm ~ sex + age, data=mockstudy), reference = "write2.tableby.html", quiet = TRUE,
                       title = "My test table", labelTranslations = list(sex = "SEX", age ="Age, yrs"), total = FALSE)
})

test_that("write2.modelsum -> HTML", {
  expect_write2_worked(write2html, modelsum(alk.phos ~ arm + ps + hgb, adjust= ~ age + sex, family = "gaussian", data = mockstudy),
                       reference = "write2.modelsum.html", quiet = TRUE,
                       title = "My test table", show.intercept = FALSE, digits = 5)
})

test_that("write2.freqlist -> HTML", {
  expect_write2_worked(write2html, freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
                       reference = "write2.freqlist.html", quiet = TRUE, single = TRUE)
})

test_that("write2.knitr_kable -> HTML", {
  expect_write2_worked(write2html, knitr::kable(head(mockstudy)), reference = "write2.kable.html", quiet = TRUE)
})

test_that("write2.xtable -> HTML", {
  expect_write2_worked(write2html, xtable::xtable(head(mockstudy), caption = "My xtable"), reference = "write2.xtable.html", quiet = TRUE,
                       type = "html", comment = FALSE, include.rownames = FALSE, caption.placement = 'top')
})

test_that("write2.character (pander) -> HTML", {
  expect_write2_worked(write2html, pander::pander_return(head(mockstudy)), reference = "write2.pander.html", quiet = TRUE)
})

###########################################################################################################
#### Code used to generate the files
###########################################################################################################
# 
# write2html(tableby(arm ~ sex + age, data=mockstudy), "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.tableby.html",
#            title = "My test table", labelTranslations = list(sex = "SEX", age ="Age, yrs"), total = FALSE)
# 
# write2html(modelsum(alk.phos ~ arm + ps + hgb, adjust= ~ age + sex, family = "gaussian", data = mockstudy),
#            "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.modelsum.html",
#            title = "My test table", show.intercept = FALSE, digits = 5)
# 
# write2html(freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
#            "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.freqlist.html", single = TRUE)
# 
# write2html(knitr::kable(head(mockstudy)),
#            "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.kable.html")
# 
# write2html(xtable::xtable(head(mockstudy), caption = "My xtable"),
#            "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.xtable.html",
#            type = "html", comment = FALSE, include.rownames = FALSE, caption.placement = "top")
# 
# write2html(pander::pander_return(head(mockstudy)),
#            "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.pander.html")


