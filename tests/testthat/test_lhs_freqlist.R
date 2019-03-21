## Tests for freqlist


context("Testing the freqlist strata and multiple LHS output")

tab <- table(mockstudy[c("arm", "sex", "mdquality.s")], useNA = "ifany")
dat <- data.frame(
  wts1 = rep(1:2, each = 6),
  wts2 = rep(1:2, times = 6),
  wts3 = rep(1:3, times = 4),
  a = rep(c("A", "B", "C"), each = 4),
  b = rep(c("D", "E", "F", "G"), each = 3),
  d = rep(c("H", "I"), each = 6),
  stringsAsFactors = FALSE
)
tab.wts <- freqlist(list(wts1, wts2) ~ a + b + d, strata = "d", data = dat, sparse = FALSE)


#####################################

test_that("Multiple strata work", {
  expect_identical(
    capture.kable(summary(freqlist(tab, strata = c("arm", "sex")))),
    capture.kable(summary(freqlist(~ arm + sex + addNA(mdquality.s), data = mockstudy, strata = c("arm", "sex")),
                          labelTranslations = c(arm = "arm", "addNA(mdquality.s)" = "mdquality.s")))
  )
})

test_that("Multiple endpoints work", {
  expect_identical(
    capture.kable(summary(freqlist(list(wts1, wts2) ~ a + b, data = dat))),
    c(
      capture.kable(summary(freqlist(wts1 ~ a + b, data = dat))), "", "",
      capture.kable(summary(freqlist(wts2 ~ a + b, data = dat)))
    )
  )
})

test_that("Multiple endpoints and strata work", {
  expect_identical(
    capture.kable(summary(tab.wts)),
    c(
      capture.kable(summary(freqlist(wts1 ~ a + b + d, strata = "d", data = dat))), "", "",
      capture.kable(summary(freqlist(wts2 ~ a + b + d, strata = "d", data = dat)))
    )
  )

  expect_identical(
    capture.kable(summary(tab.wts)),
    c(
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|H  |A  |D  |    3|               3|   50.00|              50.00|",
      "|   |   |E  |    1|               4|   16.67|              66.67|",
      "|   |B  |E  |    2|               6|   33.33|             100.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|I  |B  |F  |    4|               4|   33.33|              33.33|",
      "|   |C  |F  |    2|               6|   16.67|              50.00|",
      "|   |   |G  |    6|              12|   50.00|             100.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|H  |A  |D  |    4|               4|   44.44|              44.44|",
      "|   |   |E  |    2|               6|   22.22|              66.67|",
      "|   |B  |E  |    3|               9|   33.33|             100.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|I  |B  |F  |    3|               3|   33.33|              33.33|",
      "|   |C  |F  |    1|               4|   11.11|              44.44|",
      "|   |   |G  |    5|               9|   55.56|             100.00|"
    )
  )
})
#####################################

test_that("Reordering variables and subsetting", {
  expect_identical(
    capture.kable(summary(tab.wts[c("b", "d", "a", "Freq")], text = TRUE)),
    c(
      "|b  |d  |a  | Freq|",
      "|:--|:--|:--|----:|",
      "|D  |H  |A  |    3|",
      "|E  |H  |A  |    1|",
      "|   |   |B  |    2|",
      ""                   ,
      ""                   ,
      "|b  |d  |a  | Freq|",
      "|:--|:--|:--|----:|",
      "|F  |I  |B  |    4|",
      "|   |   |C  |    2|",
      "|G  |I  |C  |    6|",
      ""                   ,
      ""                   ,
      "|b  |d  |a  | Freq|",
      "|:--|:--|:--|----:|",
      "|D  |H  |A  |    4|",
      "|E  |H  |A  |    2|",
      "|   |   |B  |    3|",
      ""                   ,
      ""                   ,
      "|b  |d  |a  | Freq|",
      "|:--|:--|:--|----:|",
      "|F  |I  |B  |    3|",
      "|   |   |C  |    1|",
      "|G  |I  |C  |    5|"
    )
  )
  expect_identical(
    capture.kable(summary(tab.wts[c(3,1,2,4)], text = TRUE)),
    capture.kable(summary(tab.wts[c("b", "d", "a", "Freq")], text = TRUE))
  )
  expect_identical(
    capture.kable(summary(tab.wts[, 2:1], text = TRUE)),
    capture.kable(summary(tab.wts[, c("wts2", "wts1")], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tab.wts[1:4], text = TRUE)),
    capture.kable(summary(tab.wts[c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)], text = TRUE))
  )
  expect_identical(
    capture.kable(summary(tab.wts[, 2], text = TRUE)),
    capture.kable(summary(tab.wts[, c(FALSE, TRUE)], text = TRUE))
  )

  expect_warning(tab.wts[1:8], "Some indices not found")
  expect_warning(tab.wts[, 1:3], "Some indices not found")
  expect_error(tab.wts[TRUE], "Logical vector")
  expect_error(tab.wts[, TRUE], "Logical vector")
})

test_that("Merging freqlist objects", {
  tb1 <- tab.wts
  tb2 <- freqlist(wts3 ~ b + d, strata = "d", data = dat)
  tb3 <- freqlist(~ a + d, strata = "d", data = dat)
  tb4 <- freqlist(~ b, data = dat)
  expect_error(merge(tb1, freqlist(wts1 ~ a + d, strata = "d", data = dat)), "Can only merge freqlist objects")
  expect_identical(
    capture.kable(summary(merge(tb1, tb2))),
    c(capture.kable(summary(tb1)), "", "", capture.kable(summary(tb2)))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, merge(tb3, tb4)))),
    c(capture.kable(summary(tb1)), "", "", capture.kable(summary(tb3)), "", "", capture.kable(summary(tb4)))
  )
})

test_that("head() and tail() work with freqlist (#188)", {
  expect_identical(
    capture.kable(head(summary(tab.wts), 2)),
    capture.kable(summary(tab.wts))[2-7*(1:4)]  # we already tested this above
  )
  expect_identical(
    capture.kable(head(summary(tab.wts, sparse = TRUE))),
    c("|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|H  |A  |D  |    3|               3|   50.00|              50.00|",
      "|   |   |E  |    1|               4|   16.67|              66.67|",
      "|   |   |F  |    0|               4|    0.00|              66.67|",
      "|   |   |G  |    0|               4|    0.00|              66.67|",
      "|   |B  |D  |    0|               4|    0.00|              66.67|",
      "|   |   |E  |    2|               6|   33.33|             100.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|I  |A  |D  |    0|               0|    0.00|               0.00|",
      "|   |   |E  |    0|               0|    0.00|               0.00|",
      "|   |   |F  |    0|               0|    0.00|               0.00|",
      "|   |   |G  |    0|               0|    0.00|               0.00|",
      "|   |B  |D  |    0|               0|    0.00|               0.00|",
      "|   |   |E  |    0|               0|    0.00|               0.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|H  |A  |D  |    4|               4|   44.44|              44.44|",
      "|   |   |E  |    2|               6|   22.22|              66.67|",
      "|   |   |F  |    0|               6|    0.00|              66.67|",
      "|   |   |G  |    0|               6|    0.00|              66.67|",
      "|   |B  |D  |    0|               6|    0.00|              66.67|",
      "|   |   |E  |    3|               9|   33.33|             100.00|",
      ""                                                                 ,
      ""                                                                 ,
      "|d  |a  |b  | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:--|:--|:--|----:|---------------:|-------:|------------------:|",
      "|I  |A  |D  |    0|               0|    0.00|               0.00|",
      "|   |   |E  |    0|               0|    0.00|               0.00|",
      "|   |   |F  |    0|               0|    0.00|               0.00|",
      "|   |   |G  |    0|               0|    0.00|               0.00|",
      "|   |B  |D  |    0|               0|    0.00|               0.00|",
      "|   |   |E  |    0|               0|    0.00|               0.00|"
    )
  )
})
