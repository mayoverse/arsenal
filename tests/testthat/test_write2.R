## Tests for write2


context("Testing the write2 output")

data(mockstudy)

expect_write2_worked <- function(FUN, object, reference, ...)
{
  FUN <- match.fun(FUN)
  filename <- tempfile(fileext = ".html")
  on.exit(expect_true(file.remove(paste0(filename, ".md"))))
  if(!file.exists(reference)) skip("Couldn't find the reference file.")
  if(!file.create(paste0(filename, ".md"))) skip("Couldn't create the temporary file.")
  if(!grepl("/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/", getwd(), fixed = TRUE)) skip("These tests only run in Ethan's space.")
  FUN(object, file = filename, ..., quiet = TRUE, render = FALSE, keep.md = TRUE, append = FALSE)
  generated <- readLines(paste0(filename, ".md"))
  expect_output_file(cat(generated, sep = "\n"), reference)
}

###########################################################################################################
#### HTML output
###########################################################################################################

test_that("write2.tableby -> HTML", {
  expect_write2_worked(write2html, tableby(arm ~ sex + age, data=mockstudy), reference = "write2.tableby.html.md",
                       title = "My test table", labelTranslations = list(sex = "SEX", age ="Age, yrs"), total = FALSE)
})

test_that("write2.modelsum -> HTML", {
  expect_write2_worked(write2html, modelsum(alk.phos ~ arm + ps + hgb, adjust= ~ age + sex, family = "gaussian", data = mockstudy),
                       reference = "write2.modelsum.html.md",
                       title = "My test table", show.intercept = FALSE, digits = 5)
})

test_that("write2.freqlist -> HTML", {
  expect_write2_worked(write2html, freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
                       reference = "write2.freqlist.html.md", single = TRUE)
})

test_that("write2.knitr_kable -> HTML", {
  if(require(knitr))
  {
    expect_write2_worked(write2html, knitr::kable(head(mockstudy)), reference = "write2.kable.html.md")
  } else skip("library(knitr) not available.")
})

test_that("write2.xtable -> HTML", {
  if(require(xtable))
  {
    expect_write2_worked(write2html, xtable::xtable(head(mockstudy), caption = "My xtable"), reference = "write2.xtable.html.md",
                         type = "html", comment = FALSE, include.rownames = FALSE, caption.placement = 'top')
  } else skip("library(xtable) not available.")
})


test_that("write2.character (pander) -> HTML", {
  if(require(pander))
  {
    expect_write2_worked(write2html, pander::pander_return(head(mockstudy)), reference = "write2.pander.html.md")
  } else skip("library(pander) not available.")
})

###########################################################################################################
#### Code used to generate the files
###########################################################################################################
# 
#  write2html(tableby(arm ~ sex + age, data=mockstudy), "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.tableby.html",
#             title = "My test table", labelTranslations = list(sex = "SEX", age ="Age, yrs"), total = FALSE, keep.md = TRUE)
# 
#  write2html(modelsum(alk.phos ~ arm + ps + hgb, adjust= ~ age + sex, family = "gaussian", data = mockstudy),
#             "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.modelsum.html",
#             title = "My test table", show.intercept = FALSE, digits = 5, keep.md = TRUE)
# 
#  write2html(freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
#             "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.freqlist.html", single = TRUE, keep.md = TRUE)
# 
#  write2html(knitr::kable(head(mockstudy)),
#             "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.kable.html", keep.md = TRUE)
# 
#  write2html(xtable::xtable(head(mockstudy), caption = "My xtable"),
#             "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.xtable.html",
#             type = "html", comment = FALSE, include.rownames = FALSE, caption.placement = "top", keep.md = TRUE)
# 
#  write2html(pander::pander_return(head(mockstudy)),
#             "/data5/bsi/adhoc/s200555.R-infrastructure/devel/eph/arsenal-eph/tests/testthat/write2.pander.html", keep.md = TRUE)
# 
###########################################################################################################
#### Reported bugs for write2
###########################################################################################################

