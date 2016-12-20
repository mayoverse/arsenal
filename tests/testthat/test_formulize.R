## Tests for formulize


context("Testing the formulize output")

data(mockstudy)

###########################################################################################################
#### Text input
###########################################################################################################

test_that("Two-sided formula, text input", {
  tmp <- capture.output(print(formulize("y", c("x1", "x2", "x3"))))
  expect_true(
    identical(tmp[1], "y ~ x1 + x2 + x3") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("One-sided formula, text input", {
  tmp <- capture.output(print(formulize(x = c("x1", "x2", "x3"))))
  expect_true(
    identical(tmp[1], "~x1 + x2 + x3") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Multi-sided formula, text input", {
  tmp <- capture.output(print(formulize("y", c("x1", "x2", "x3"), "w1", c("z1", "z2"))))
  expect_true(
    identical(tmp[1], "y ~ x1 + x2 + x3 ~ w1 ~ z1 + z2") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Two-sided formula, invalid input", {
  expect_error(
    formulize("y", FALSE)
  )
})

###########################################################################################################
#### Numeric input
###########################################################################################################

test_that("Two-sided formula, numeric input", {
  tmp <- capture.output(print(formulize(1, 2:4, data = mockstudy)))
  expect_true(
    identical(tmp[1], "case ~ age + arm + sex") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("One-sided formula, numeric input", {
  tmp <- capture.output(print(formulize(x = 2:4, data = mockstudy)))
  expect_true(
    identical(tmp[1], "~age + arm + sex") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Multi-sided formula, numeric input", {
  tmp <- capture.output(print(formulize(1, 2:4, 5, 6:7, data = mockstudy)))
  expect_true(
    identical(tmp[1], "case ~ age + arm + sex ~ race ~ fu.time + fu.stat") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Two-sided formula, numeric input, no data", {
  expect_error(
    formulize(1, 2:4)
  )
})

###########################################################################################################
#### Mixed input
###########################################################################################################

test_that("Two-sided formula, mixed input", {
  tmp <- capture.output(print(formulize("y", 2:4, data = mockstudy)))
  expect_true(
    identical(tmp[1], "y ~ age + arm + sex") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Multi-sided formula, mixed input", {
  tmp <- capture.output(print(formulize(1, "x1", 5, "w", data = mockstudy)))
  expect_true(
    identical(tmp[1], "case ~ x1 ~ race ~ w") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

test_that("Two-sided formula, interaction", {
  tmp <- capture.output(print(formulize("y", c("x1*x2", "x3"))))
  expect_true(
    identical(tmp[1], "y ~ x1 * x2 + x3") && grepl("<environment: ", tmp[2], fixed = TRUE)
  )
})

###########################################################################################################
#### Actually using it
###########################################################################################################

test_that("Two-sided formula, mixed input", {
  expect_true(
    identical(capture.output(print(lm(formulize(2, 3:4, data = mockstudy), data = mockstudy))),
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
  )
})

