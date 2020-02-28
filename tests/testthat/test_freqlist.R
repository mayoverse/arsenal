## Tests for freqlist


context("Testing the freqlist output")

# "mdat" now defined in helper-data.R

TAB <- table(mdat[, c("Group", "Sex", "Phase")])
TAB.subset <- table(mdat[!(mdat$Group == "Low" & mdat$Sex == "Male"), c("Group", "Sex", "Phase")])
TAB.na <- table(mdat[, c("trt", "ethan")], useNA = 'a')
old.labs <- c(cumFreq = "cumFreq", freqPercent = "freqPercent", cumPercent = "cumPercent")

###########################################################################################################
#### Basic freqlist
###########################################################################################################

test_that("A basic freqlist call", {
  expect_identical(
    capture.kable(summary(freqlist(TAB), labelTranslations = old.labs)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|        4.44|       4.44|",
      "|      |       |II    |    8|      12|        8.89|      13.33|",
      "|      |       |III   |    3|      15|        3.33|      16.67|",
      "|      |Male   |I     |    7|      22|        7.78|      24.44|",
      "|      |       |II    |    2|      24|        2.22|      26.67|",
      "|      |       |III   |    6|      30|        6.67|      33.33|",
      "|Low   |Female |I     |    7|      37|        7.78|      41.11|",
      "|      |       |II    |    8|      45|        8.89|      50.00|",
      "|      |       |III   |    2|      47|        2.22|      52.22|",
      "|      |Male   |I     |    5|      52|        5.56|      57.78|",
      "|      |       |II    |    4|      56|        4.44|      62.22|",
      "|      |       |III   |    4|      60|        4.44|      66.67|",
      "|Med   |Female |II    |   11|      71|       12.22|      78.89|",
      "|      |       |III   |    3|      74|        3.33|      82.22|",
      "|      |Male   |II    |    8|      82|        8.89|      91.11|",
      "|      |       |III   |    8|      90|        8.89|     100.00|"
    )
  )
})

test_that("strata option in freqlist call", {
  expect_identical(
    capture.kable(summary(freqlist(TAB, strata = "Group"), single = FALSE, labelTranslations = old.labs)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|       13.33|      13.33|",
      "|      |       |II    |    8|      12|       26.67|      40.00|",
      "|      |       |III   |    3|      15|       10.00|      50.00|",
      "|      |Male   |I     |    7|      22|       23.33|      73.33|",
      "|      |       |II    |    2|      24|        6.67|      80.00|",
      "|      |       |III   |    6|      30|       20.00|     100.00|",
      ""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|Low   |Female |I     |    7|       7|       23.33|      23.33|",
      "|      |       |II    |    8|      15|       26.67|      50.00|",
      "|      |       |III   |    2|      17|        6.67|      56.67|",
      "|      |Male   |I     |    5|      22|       16.67|      73.33|",
      "|      |       |II    |    4|      26|       13.33|      86.67|",
      "|      |       |III   |    4|      30|       13.33|     100.00|",
      ""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|Med   |Female |II    |   11|      11|       36.67|      36.67|",
      "|      |       |III   |    3|      14|       10.00|      46.67|",
      "|      |Male   |II    |    8|      22|       26.67|      73.33|",
      "|      |       |III   |    8|      30|       26.67|     100.00|"
    )
  )

  expect_error(freqlist(TAB, strata = "group"), "strata variable not found")
})

test_that("sparse option in freqlist call", {
  expect_identical(
    capture.kable(summary(freqlist(TAB, sparse = TRUE), labelTranslations = old.labs)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|        4.44|       4.44|",
      "|      |       |II    |    8|      12|        8.89|      13.33|",
      "|      |       |III   |    3|      15|        3.33|      16.67|",
      "|      |Male   |I     |    7|      22|        7.78|      24.44|",
      "|      |       |II    |    2|      24|        2.22|      26.67|",
      "|      |       |III   |    6|      30|        6.67|      33.33|",
      "|Low   |Female |I     |    7|      37|        7.78|      41.11|",
      "|      |       |II    |    8|      45|        8.89|      50.00|",
      "|      |       |III   |    2|      47|        2.22|      52.22|",
      "|      |Male   |I     |    5|      52|        5.56|      57.78|",
      "|      |       |II    |    4|      56|        4.44|      62.22|",
      "|      |       |III   |    4|      60|        4.44|      66.67|",
      "|Med   |Female |I     |    0|      60|        0.00|      66.67|",
      "|      |       |II    |   11|      71|       12.22|      78.89|",
      "|      |       |III   |    3|      74|        3.33|      82.22|",
      "|      |Male   |I     |    0|      74|        0.00|      82.22|",
      "|      |       |II    |    8|      82|        8.89|      91.11|",
      "|      |       |III   |    8|      90|        8.89|     100.00|"
    )
  )
})

test_that("NA options in freqlist call", {
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "include"))),
    c("|trt |ethan   | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:---|:-------|----:|---------------:|-------:|------------------:|",
      "|A   |Ethan   |   17|              17|   18.89|              18.89|",
      "|    |Heinzen |   16|              33|   17.78|              36.67|",
      "|    |NA      |    3|              36|    3.33|              40.00|",
      "|B   |Ethan   |   25|              61|   27.78|              67.78|",
      "|    |Heinzen |   29|              90|   32.22|             100.00|"
    )
  )
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "showexclude"))),
    c("|trt |ethan   | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:---|:-------|----:|---------------:|-------:|------------------:|",
      "|A   |Ethan   |   17|              17|   19.54|              19.54|",
      "|    |Heinzen |   16|              33|   18.39|              37.93|",
      "|    |NA      |    3|              NA|      NA|                 NA|",
      "|B   |Ethan   |   25|              58|   28.74|              66.67|",
      "|    |Heinzen |   29|              87|   33.33|             100.00|"
    )
  )
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "remove"))),
    c("|trt |ethan   | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:---|:-------|----:|---------------:|-------:|------------------:|",
      "|A   |Ethan   |   17|              17|   19.54|              19.54|",
      "|    |Heinzen |   16|              33|   18.39|              37.93|",
      "|B   |Ethan   |   25|              58|   28.74|              66.67|",
      "|    |Heinzen |   29|              87|   33.33|             100.00|"
    )
  )
})

test_that("Changing the labels on non-grouped freqlists", {

  ref <- c(
    "|Treatment |Ethan Rocks | Freq| Cumulative Freq| Percent| Cumulative Percent|",
    "|:---------|:-----------|----:|---------------:|-------:|------------------:|",
    "|A         |Ethan       |   17|              17|   18.89|              18.89|",
    "|          |Heinzen     |   16|              33|   17.78|              36.67|",
    "|          |NA          |    3|              36|    3.33|              40.00|",
    "|B         |Ethan       |   25|              61|   27.78|              67.78|",
    "|          |Heinzen     |   29|              90|   32.22|             100.00|"
  )

  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "include"), labelTranslations = c(trt = "Treatment", ethan = "Ethan Rocks"))),
    ref
  )

  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "include", labelTranslations = list(trt = "Treatment", ethan = "Ethan Rocks")))),
    ref
  )

  expect_identical(
    capture.kable(summary(freqlist(TAB.na, na.options = "include", labelTranslations = c(trt = "Treatment", ethan ="Ethan Rocks")))),
    ref
  )

  expect_error(freqlist(TAB.na, labelTranslations = c("Treatment", "Ethan Rocks")), "Unnamed label")
  expect_error(freqlist(TAB.na, labelTranslations = c(hi = "Treatment", ethan = "Ethan Rocks")), NA)

  tmp <- freqlist(TAB.na, na.options = "include")
  labels(tmp) <- c(trt = "Treatment", ethan = "Ethan Rocks")
  expect_identical(
    capture.kable(summary(tmp)),
    ref
  )
  labels(tmp) <- NULL
  expect_identical(
    capture.kable(summary(tmp)),
    c("|trt |ethan   | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---|:-------|----:|-------:|-----------:|----------:|",
      "|A   |Ethan   |   17|      17|       18.89|      18.89|",
      "|    |Heinzen |   16|      33|       17.78|      36.67|",
      "|    |NA      |    3|      36|        3.33|      40.00|",
      "|B   |Ethan   |   25|      61|       27.78|      67.78|",
      "|    |Heinzen |   29|      90|       32.22|     100.00|"
    )
  )

  labels(tmp) <- c(ethan = "Ethan Rocks", trt = "Treatment", dummy = "Dummy")
  expect_identical(
    capture.kable(summary(tmp)),
    c(
      "|Treatment |Ethan Rocks | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---------|:-----------|----:|-------:|-----------:|----------:|",
      "|A         |Ethan       |   17|      17|       18.89|      18.89|",
      "|          |Heinzen     |   16|      33|       17.78|      36.67|",
      "|          |NA          |    3|      36|        3.33|      40.00|",
      "|B         |Ethan       |   25|      61|       27.78|      67.78|",
      "|          |Heinzen     |   29|      90|       32.22|     100.00|"
    )
  )
})


test_that("Changing the labels on grouped freqlists", {
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, options = "include", strata = "ethan",
                                   labelTranslations = c(trt = "Treatment", ethan = "Ethan", old.labs)))),
    c("|Ethan |Treatment | Freq| cumFreq| freqPercent| cumPercent|"  ,
      "|:-----|:---------|----:|-------:|-----------:|----------:|"  ,
      "|Ethan |A         |   17|      17|       40.48|      40.48|"  ,
      "|      |B         |   25|      42|       59.52|     100.00|"  ,
      ""                                                             ,
      ""                                                             ,
      "|Ethan   |Treatment | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-------|:---------|----:|-------:|-----------:|----------:|",
      "|Heinzen |A         |   16|      16|       35.56|      35.56|",
      "|        |B         |   29|      45|       64.44|     100.00|",
      ""                                                             ,
      ""                                                             ,
      "|Ethan |Treatment | Freq| cumFreq| freqPercent| cumPercent|"  ,
      "|:-----|:---------|----:|-------:|-----------:|----------:|"  ,
      "|NA    |A         |    3|       3|      100.00|     100.00|"
    )
  )
})


test_that("dupLabels works", {
  expect_identical(
    capture.kable(summary(freqlist(TAB, labelTranslations = old.labs), dupLabels = TRUE)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|        4.44|       4.44|",
      "|High  |Female |II    |    8|      12|        8.89|      13.33|",
      "|High  |Female |III   |    3|      15|        3.33|      16.67|",
      "|High  |Male   |I     |    7|      22|        7.78|      24.44|",
      "|High  |Male   |II    |    2|      24|        2.22|      26.67|",
      "|High  |Male   |III   |    6|      30|        6.67|      33.33|",
      "|Low   |Female |I     |    7|      37|        7.78|      41.11|",
      "|Low   |Female |II    |    8|      45|        8.89|      50.00|",
      "|Low   |Female |III   |    2|      47|        2.22|      52.22|",
      "|Low   |Male   |I     |    5|      52|        5.56|      57.78|",
      "|Low   |Male   |II    |    4|      56|        4.44|      62.22|",
      "|Low   |Male   |III   |    4|      60|        4.44|      66.67|",
      "|Med   |Female |II    |   11|      71|       12.22|      78.89|",
      "|Med   |Female |III   |    3|      74|        3.33|      82.22|",
      "|Med   |Male   |II    |    8|      82|        8.89|      91.11|",
      "|Med   |Male   |III   |    8|      90|        8.89|     100.00|"
    )
  )
  expect_identical(
    capture.kable(summary(freqlist(TAB, strata = "Group", labelTranslations = old.labs), single = FALSE, dupLabels = TRUE)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|       13.33|      13.33|",
      "|High  |Female |II    |    8|      12|       26.67|      40.00|",
      "|High  |Female |III   |    3|      15|       10.00|      50.00|",
      "|High  |Male   |I     |    7|      22|       23.33|      73.33|",
      "|High  |Male   |II    |    2|      24|        6.67|      80.00|",
      "|High  |Male   |III   |    6|      30|       20.00|     100.00|",
      ""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|Low   |Female |I     |    7|       7|       23.33|      23.33|",
      "|Low   |Female |II    |    8|      15|       26.67|      50.00|",
      "|Low   |Female |III   |    2|      17|        6.67|      56.67|",
      "|Low   |Male   |I     |    5|      22|       16.67|      73.33|",
      "|Low   |Male   |II    |    4|      26|       13.33|      86.67|",
      "|Low   |Male   |III   |    4|      30|       13.33|     100.00|",
      ""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|Med   |Female |II    |   11|      11|       36.67|      36.67|",
      "|Med   |Female |III   |    3|      14|       10.00|      46.67|",
      "|Med   |Male   |II    |    8|      22|       26.67|      73.33|",
      "|Med   |Male   |III   |    8|      30|       26.67|     100.00|"
    )
  )
})


test_that("Adding a title", {
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, options = "include", labelTranslations = c(trt = "Treatment", ethan = "Ethan", old.labs)),
                          dupLabels = TRUE, title = "Ethan Rocks")),
    c("Table: Ethan Rocks"                                           ,
      ""                                                             ,
      "|Treatment |Ethan   | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---------|:-------|----:|-------:|-----------:|----------:|",
      "|A         |Ethan   |   17|      17|       18.89|      18.89|",
      "|A         |Heinzen |   16|      33|       17.78|      36.67|",
      "|A         |NA      |    3|      36|        3.33|      40.00|",
      "|B         |Ethan   |   25|      61|       27.78|      67.78|",
      "|B         |Heinzen |   29|      90|       32.22|     100.00|"
    )
  )

  expect_identical(
    capture.kable(summary(freqlist(TAB.na, options = "include", strata = "ethan",
                                    labelTranslations = c(trt = "Treatment", ethan = "Ethan", old.labs)),
                          dupLabels = TRUE, title = "Ethan Rocks")),
    c("Table: Ethan Rocks"                                           ,
      ""                                                             ,
      "|Ethan |Treatment | Freq| cumFreq| freqPercent| cumPercent|"  ,
      "|:-----|:---------|----:|-------:|-----------:|----------:|"  ,
      "|Ethan |A         |   17|      17|       40.48|      40.48|"  ,
      "|Ethan |B         |   25|      42|       59.52|     100.00|"  ,
      ""                                                             ,
      ""                                                             ,
      "|Ethan   |Treatment | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-------|:---------|----:|-------:|-----------:|----------:|",
      "|Heinzen |A         |   16|      16|       35.56|      35.56|",
      "|Heinzen |B         |   29|      45|       64.44|     100.00|",
      ""                                                             ,
      ""                                                             ,
      "|Ethan |Treatment | Freq| cumFreq| freqPercent| cumPercent|"  ,
      "|:-----|:---------|----:|-------:|-----------:|----------:|"  ,
      "|NA    |A         |    3|       3|      100.00|     100.00|"
    )
  )
})


test_that("Formula method works", {
  expect_identical(
    capture.kable(summary(freqlist(TAB.na, options = "include"), labelTranslations = c(trt = "Trt", ethan = "Ethan"))),
    capture.kable(summary(freqlist(~ trt + addNA(ethan), data = mdat), labelTranslations = c("addNA(ethan)" = "Ethan", trt = "Trt")))
  )
  if(getRversion() >= "3.4.0")
  {
    expect_identical(
      capture.kable(summary(freqlist(~ trt + ethan, data = mdat, addNA = TRUE), labelTranslations = c(trt = "Trt", ethan = "Ethan"))),
      capture.kable(summary(freqlist(~ trt + addNA(ethan), data = mdat), labelTranslations = c("addNA(ethan)" = "Ethan", trt = "Trt")))
    )
  } else skip("R version isn't right to use 'addNA=TRUE'")
})


test_that("digits specification", {
  expect_identical(
    capture.kable(summary(freqlist(~ trt + addNA(ethan), data = mdat), digits.pct = 1, digits.count = 1)),
    capture.kable(summary(freqlist(~ trt + addNA(ethan), data = mdat, digits.pct = 1, digits.count = 1)))
  )
  expect_identical(
    capture.kable(summary(freqlist(~ trt + addNA(ethan), data = mdat), digits.pct = 1, digits.count = 1)),
    c(
      "|Treatment Arm |addNA(ethan) | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:-------------|:------------|----:|---------------:|-------:|------------------:|",
      "|A             |Ethan        | 17.0|            17.0|    18.9|               18.9|",
      "|              |Heinzen      | 16.0|            33.0|    17.8|               36.7|",
      "|              |NA           |  3.0|            36.0|     3.3|               40.0|",
      "|B             |Ethan        | 25.0|            61.0|    27.8|               67.8|",
      "|              |Heinzen      | 29.0|            90.0|    32.2|              100.0|"
    )
  )
})

###########################################################################################################
#### Reported bugs for freqlist
###########################################################################################################


test_that("11/18/16: Emily Lundt's subsetted table and duplicate label problem", {
  expect_identical(
    capture.kable(summary(freqlist(TAB.subset), labelTranslations = old.labs)),
    c("|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
      "|:-----|:------|:-----|----:|-------:|-----------:|----------:|",
      "|High  |Female |I     |    4|       4|        5.19|       5.19|",
      "|      |       |II    |    8|      12|       10.39|      15.58|",
      "|      |       |III   |    3|      15|        3.90|      19.48|",
      "|      |Male   |I     |    7|      22|        9.09|      28.57|",
      "|      |       |II    |    2|      24|        2.60|      31.17|",
      "|      |       |III   |    6|      30|        7.79|      38.96|",
      "|Low   |Female |I     |    7|      37|        9.09|      48.05|",
      "|      |       |II    |    8|      45|       10.39|      58.44|",
      "|      |       |III   |    2|      47|        2.60|      61.04|",
      "|Med   |Female |II    |   11|      58|       14.29|      75.32|",
      "|      |       |III   |    3|      61|        3.90|      79.22|",
      "|      |Male   |II    |    8|      69|       10.39|      89.61|",
      "|      |       |III   |    8|      77|       10.39|     100.00|"
    )
  )
})

test_that("04/17/18: using 'method' in freqlist (#95)", {
  dat <- data.frame(method = c(1, 1, 2, 2, 3, 3, 4, 4))

  expect_identical(
    capture.kable(summary(freqlist(~method, data = dat))),
    c("|method | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:------|----:|---------------:|-------:|------------------:|",
      "|1      |    2|               2|   25.00|              25.00|",
      "|2      |    2|               4|   25.00|              50.00|",
      "|3      |    2|               6|   25.00|              75.00|",
      "|4      |    2|               8|   25.00|             100.00|"
    )
  )
})

test_that("02/26/19: don't drop labels with subset= argument (#184)", {
  expect_identical(
    capture.kable(summary(freqlist(~ age, data = mockstudy, subset = age > 80))),
    c("|Age in Years | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:------------|----:|---------------:|-------:|------------------:|",
      "|81           |   12|              12|   41.38|              41.38|",
      "|82           |    6|              18|   20.69|              62.07|",
      "|83           |    6|              24|   20.69|              82.76|",
      "|84           |    1|              25|    3.45|              86.21|",
      "|85           |    2|              27|    6.90|              93.10|",
      "|88           |    2|              29|    6.90|             100.00|"
    )
  )
})

test_that("03/20/2019: freqlist still works with all zero counts (#194, #186).", {
  tab0 <- table(factor(c(), levels = c("m", "f")))
  expect_error(print(summary(freqlist(tab0))), "There wasn't anything")
  expect_identical(
    capture.kable(summary(freqlist(tab0), sparse = TRUE)),
    c("|Var1 | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:----|----:|---------------:|-------:|------------------:|",
      "|m    |    0|               0|      NA|                 NA|",
      "|f    |    0|               0|      NA|                 NA|"
    )
  )
})

test_that("03/21/2019: freqlist doesn't lose labels when subsetting (#196)", {
  expect_identical(
    capture.kable(summary(freqlist(~ sex + ps + arm, data = mockstudy, strata = "arm",
                                   subset = arm == "F: FOLFOX" & !is.na(ps))[c(1:2, 4)])),
    c("|Treatment Arm |sex    | Freq|",
      "|:-------------|:------|----:|",
      "|F: FOLFOX     |Male   |  168|",
      "|              |       |  148|",
      "|              |       |   16|",
      "|              |Female |  110|",
      "|              |       |   95|",
      "|              |       |   13|"
    )
  )
})

test_that("02/28/2020: freqlist.formula works without needing addNA AND na.option (#265)", {
  expect_identical(
    capture.kable(summary(freqlist(~ sex + ps, data = mockstudy))),
    c("|sex    |ps | Freq| Cumulative Freq| Percent| Cumulative Percent|",
      "|:------|:--|----:|---------------:|-------:|------------------:|",
      "|Male   |0  |  391|             391|   26.08|              26.08|",
      "|       |1  |  329|             720|   21.95|              48.03|",
      "|       |2  |   34|             754|    2.27|              50.30|",
      "|       |NA |  162|             916|   10.81|              61.11|",
      "|Female |0  |  244|            1160|   16.28|              77.38|",
      "|       |1  |  202|            1362|   13.48|              90.86|",
      "|       |2  |   33|            1395|    2.20|              93.06|",
      "|       |NA |  104|            1499|    6.94|             100.00|"
    )
  )
  expect_identical(
    capture.kable(summary(freqlist(~ sex + ps, data = mockstudy, addNA = FALSE))),
    capture.kable(summary(freqlist(~ sex + ps, data = mockstudy, na.options = "remove")))
  )
})
