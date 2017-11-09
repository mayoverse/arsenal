## Tests for write2


context("Testing the write2 output")

data(mockstudy)

expect_write2_worked <- function(FUN, object, reference, ...)
{
  FUN <- match.fun(FUN)
  filename <- tempfile()
  on.exit(expect_true(file.remove(paste0(filename, ".md"))))
  if(!file.exists(reference)) skip("Couldn't find the reference file.")
  if(!file.create(paste0(filename, ".md"))) skip("Couldn't create the temporary file.")
  if(!grepl("rpkg-arsenal/", getwd(), fixed = TRUE)) skip("These tests only run in Ethan's space.")
  FUN(object, file = filename, ..., render. = FALSE, keep.md = TRUE, append. = FALSE)
  generated <- readLines(paste0(filename, ".md"))
  expect_output_file(cat(generated, sep = "\n"), reference)
}

###########################################################################################################
#### Internal output
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

test_that("write2.freqlist -> doc", {
  expect_write2_worked(write2word, freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
                       reference = "write2.freqlist.doc.md", single = TRUE, title = "My cool title")
})

###########################################################################################################
#### External output
###########################################################################################################

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
#### List output
###########################################################################################################


mylist <- list(tableby(sex ~ age, data = mockstudy),
               freqlist(table(mockstudy[, c("sex", "arm")])),
               knitr::kable(head(mockstudy)))
mylist2 <- list("# Header 1",
                "This is a small paragraph.",
                tableby(sex ~ age, data = mockstudy))

test_that("write2.list -> PDF", {
  expect_write2_worked(write2pdf, mylist, reference = "write2.mylist.pdf.md")
})

test_that("write2.list -> Word", {
  expect_write2_worked(write2word, mylist2, reference = "write2.mylist2.doc.md")
})

test_that("write2.list recursion -> PDF", {
  expect_write2_worked(write2word, list(mylist2, mylist),
                       reference = "write2.mylists.pdf.md")
})


###########################################################################################################
#### verbatim output
###########################################################################################################

my.lm <- summary(lm(age ~ sex, data = mockstudy))
test_that("write2.default -> PDF", {
  expect_write2_worked(write2pdf, my.lm,
                       reference = "write2.lm.pdf.md")
})

test_that("write2.verbatim -> html", {
  expect_write2_worked(write2pdf, verbatim(paste0("Hi.", 1:5)),
                       reference = "write2.char.html.md")
})


###########################################################################################################
#### YAML output
###########################################################################################################

mylist3 <- list(
  "# Header 1",
  "This is a small paragraph.",
  tableby(sex ~ age, data = mockstudy),
  yaml(title = "My title"),
  my.lm,
  yaml(author = "Ethan P Heinzen"),
  yaml("header-includes" = list("\\usepackage[labelformat=empty]{caption}"))
)

mylist4 <- list(
  yaml(title = "My title", author = "Ethan P Heinzen", "header-includes" = list("\\usepackage[labelformat=empty]{caption}")),
  "# Header 1",
  "This is a small paragraph.",
  tableby(sex ~ age, data = mockstudy),
  my.lm
)

test_that("write2.yaml -> PDF", {
  expect_write2_worked(write2pdf, mylist3, reference = "write2.yaml.pdf.md")
  expect_write2_worked(write2pdf, mylist4, reference = "write2.yaml.pdf.md")
})


###########################################################################################################
#### Code used to generate the files
###########################################################################################################
#
#  write2html(tableby(arm ~ sex + age, data=mockstudy), "tests/testthat/write2.tableby.html",
#             title = "My test table", labelTranslations = list(sex = "SEX", age ="Age, yrs"), total = FALSE, render. = FALSE)
#
#  write2html(modelsum(alk.phos ~ arm + ps + hgb, adjust= ~ age + sex, family = "gaussian", data = mockstudy),
#             "tests/testthat/write2.modelsum.html",
#             title = "My test table", show.intercept = FALSE, digits = 5, render. = FALSE)
#
#  write2html(freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
#             "tests/testthat/write2.freqlist.html", single = TRUE, render. = FALSE)
#
# write2word(freqlist(table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany"), groupBy = c("arm", "sex")),
#            "tests/testthat/write2.freqlist.doc", single = TRUE, title = "My cool caption", render. = FALSE)
#
#  write2html(knitr::kable(head(mockstudy)),
#             "tests/testthat/write2.kable.html", render. = FALSE)
#
#  write2html(xtable::xtable(head(mockstudy), caption = "My xtable"),
#             "tests/testthat/write2.xtable.html",
#             type = "html", comment = FALSE, include.rownames = FALSE, caption.placement = "top", render. = FALSE)
#
#  write2html(pander::pander_return(head(mockstudy)),
#             "tests/testthat/write2.pander.html", render. = FALSE)
#
#
# write2pdf(mylist, "tests/testthat/write2.mylist.pdf", render. = FALSE)
# write2word(mylist2, "tests/testthat/write2.mylist2.doc", render. = FALSE)
# write2pdf(list(mylist2, mylist), "tests/testthat/write2.mylists.pdf", render. = FALSE)
#
# write2pdf(my.lm, "tests/testthat/write2.lm.pdf", render. = FALSE)
# write2html(verbatim(paste0("Hi.", 1:5)),
#            "tests/testthat/write2.char.html", render. = FALSE)
# write2pdf(mylist3, "tests/testthat/write2.yaml.pdf", render. = FALSE)

###########################################################################################################
#### Reported bugs for write2
###########################################################################################################


