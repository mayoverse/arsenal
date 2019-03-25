## Tests for formulize


context("Testing the formulize output")

data(mockstudy)

cap <- function(...) capture.output(print(...))
check_form <- function(whatis, shouldbe)
{
  tmp <- cap(whatis, showEnv = TRUE)
  expect_identical(tmp[1], shouldbe)
  expect_true(grepl("<environment: ", tmp[2], fixed = TRUE))
}

###########################################################################################################
#### Text input
###########################################################################################################

test_that("Two-sided formula, text input", {
  check_form(formulize("y", c("x1", "x2", "x3")), "y ~ x1 + x2 + x3")
})

test_that("One-sided formula, text input", {
  check_form(formulize(x = c("x1", "x2", "x3")), "~x1 + x2 + x3")
})

test_that("Multi-sided formula, text input", {
  check_form(formulize("y", c("x1", "x2", "x3"), "w1", c("z1", "z2")), "y ~ x1 + x2 + x3 ~ w1 ~ z1 + z2")
})

test_that("Two-sided formula, invalid input", {
  expect_error(formulize("y", FALSE))
})

###########################################################################################################
#### Numeric input
###########################################################################################################

test_that("Two-sided formula, numeric input", {
  check_form(formulize(1, 2:4, data = mockstudy), "case ~ age + arm + sex")
})

test_that("One-sided formula, numeric input", {
  check_form(formulize(x = 2:4, data = mockstudy), "~age + arm + sex")
})

test_that("Multi-sided formula, numeric input", {
  check_form(formulize(1, 2:4, 5, 6:7, data = mockstudy), "case ~ age + arm + sex ~ race ~ fu.time + fu.stat")
})

test_that("Two-sided formula, numeric input, no data", {
  expect_error(formulize(1, 2:4))
})

###########################################################################################################
#### Mixed input
###########################################################################################################

test_that("Two-sided formula, mixed input", {
  check_form(formulize("y", 2:4, data = mockstudy), "y ~ age + arm + sex")
})

test_that("Multi-sided formula, mixed input", {
  check_form(formulize(1, "x1", 5, "w", data = mockstudy), "case ~ x1 ~ race ~ w")
})

test_that("Two-sided formula, interaction", {
  check_form(formulize("y", c("x1*x2", "x3")), "y ~ x1 * x2 + x3")
})

###########################################################################################################
#### Actually using it
###########################################################################################################

test_that("Two-sided formula, mixed input", {
  expect_identical(
    cap(lm(formulize(2, 3:4, data = mockstudy), data = mockstudy)),
    c(""                                                                   ,
      "Call:"                                                              ,
      "lm(formula = formulize(2, 3:4, data = mockstudy), data = mockstudy)",
      ""                                                                   ,
      "Coefficients:"                                                      ,
      " (Intercept)  armF: FOLFOX    armG: IROX     sexFemale  "           ,
      "     60.1075        0.6927        0.1484       -1.2319  "           ,
      ""
    )
  )
})


###########################################################################################################
#### Reported bugs for formulize
###########################################################################################################

test_that("08/26/2017: changing environment of resulting formula", {
  expect_identical(
    cap(environment()),
    cap(environment(formulize("y", "x")))
  )
})

test_that("06/04/2018: non-syntactic names", {
  dat <- data.frame(`:)` = 1:10, "log(hi)" = log(1:10), check.names = FALSE)
  check_form(formulize(1, 2, data = dat), "`:)` ~ `log(hi)`")
  check_form(formulize("", 1:2, data = dat), "~`:)` + `log(hi)`")
  check_form(formulize("log(hi)", 1, data = dat), "log(hi) ~ `:)`")
  check_form(formulize(1:2, ".", data = dat), "`:)` + `log(hi)` ~ .")
})

test_that("11/06/2018: passing names or calls (#152, #153)", {
  expect_identical(stats::reformulate(c("`P/E`", "`% Growth`"), response = as.name("+-")), formulize(c("`P/E`", "`% Growth`"), y = as.name("+-")))
  f <- Surv(ft, case) ~ `hi there`
  expect_identical(stats::reformulate("`hi there`", f[[2]]), formulize(f[[2]], f[[3]])) # can't pass call as first arg of reformulate
  expect_identical(f, formulize(f[[2]], f[[3]]))
  f <- Surv(ft, case) ~ `hi there` + b
  expect_identical(f, formulize(f[[2]], f[[3]]))
})
