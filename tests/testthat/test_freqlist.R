## Tests for freqlist


context("Testing the freqlist output")

options(stringsAsFactors=FALSE)
set.seed(100)
nsubj <- 90 # need multiple of 3
mdat <- data.frame(Group = c(rep("High", nsubj/3), rep("Med", nsubj/3), rep("Low", nsubj/3)),
                   Sex = sample(c("Male", "Female"), nsubj, replace=TRUE),
                   Age = round(rnorm(nsubj, mean=40, sd=5)),
                   Phase = ordered(sample(c("I", "II", "III"), nsubj, replace=TRUE), levels=c("I", "II", "III")),
                   ht_in = round(rnorm(nsubj, mean=65, sd=5)),
                   time = round(runif(nsubj,0,7)),
                   status = rbinom(nsubj, 1, prob=0.4),
                   dt = as.Date(round(rnorm(90, mean=100, sd=2000)), origin="1950/01/01"),
                   missing = as.character(NA),
                   trt = factor(sample(c("A", "B"), nsubj, replace=TRUE)),
                   ethan = factor(c(NA, NA, NA, sample(c("Ethan", "Heinzen"), nsubj - 3, replace=TRUE))),
                   weights = c(20, 1.5, rep(1, nsubj - 2)))
mdat$Group.fac <- factor(mdat$Group)
attr(mdat$ht_in, "label") <- "Height in Inches"
attr(mdat$trt, "label") <- "Treatment Arm"
attr(mdat$Age, "label") <- "Age in Years"

TAB <- table(mdat[, c("Group", "Sex", "Phase")])
TAB.subset <- table(mdat[!(mdat$Group == "Low" & mdat$Sex == "Male"), c("Group", "Sex", "Phase")])
TAB.na <- table(mdat[, c("trt", "ethan")], useNA = 'a')

###########################################################################################################
#### Basic freqlist
###########################################################################################################

test_that("A basic freqlist call", {
  expect_identical(
    capture.output(summary(freqlist(TAB))),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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

test_that("groupBy option in freqlist call", {
  expect_identical(
    capture.output(summary(freqlist(TAB, groupBy = "Group"), single = FALSE)),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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

  expect_error(freqlist(TAB, groupBy = "group"))
})

test_that("sparse option in freqlist call", {
  expect_identical(
    capture.output(summary(freqlist(TAB, sparse = TRUE))),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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
    capture.output(summary(freqlist(TAB.na, na.options = "include"))),
    c(""                                                       ,
      ""                                                       ,
      "|trt |ethan   | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---|:-------|----:|-------:|-----------:|----------:|",
      "|A   |Ethan   |   17|      17|       18.89|      18.89|",
      "|    |Heinzen |   16|      33|       17.78|      36.67|",
      "|    |NA      |    3|      36|        3.33|      40.00|",
      "|B   |Ethan   |   25|      61|       27.78|      67.78|",
      "|    |Heinzen |   29|      90|       32.22|     100.00|"
    )
  )
  expect_identical(
    capture.output(summary(freqlist(TAB.na, na.options = "showexclude"))),
    c(""                                                       ,
      ""                                                       ,
      "|trt |ethan   | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---|:-------|----:|-------:|-----------:|----------:|",
      "|A   |Ethan   |   17|      17|       19.54|      19.54|",
      "|    |Heinzen |   16|      33|       18.39|      37.93|",
      "|    |NA      |    3|      NA|          NA|         NA|",
      "|B   |Ethan   |   25|      58|       28.74|      66.67|",
      "|    |Heinzen |   29|      87|       33.33|     100.00|"
    )
  )
  expect_identical(
    capture.output(summary(freqlist(TAB.na, na.options = "remove"))),
    c(""                                                       ,
      ""                                                       ,
      "|trt |ethan   | Freq| cumFreq| freqPercent| cumPercent|",
      "|:---|:-------|----:|-------:|-----------:|----------:|",
      "|A   |Ethan   |   17|      17|       19.54|      19.54|",
      "|    |Heinzen |   16|      33|       18.39|      37.93|",
      "|B   |Ethan   |   25|      58|       28.74|      66.67|",
      "|    |Heinzen |   29|      87|       33.33|     100.00|"
    )
  )
})

test_that("Changing the labels on non-grouped freqlists", {

  ref <- c(
    ""                                                                 ,
    ""                                                                 ,
    "|Treatment |Ethan Rocks | Freq| cumFreq| freqPercent| cumPercent|",
    "|:---------|:-----------|----:|-------:|-----------:|----------:|",
    "|A         |Ethan       |   17|      17|       18.89|      18.89|",
    "|          |Heinzen     |   16|      33|       17.78|      36.67|",
    "|          |NA          |    3|      36|        3.33|      40.00|",
    "|B         |Ethan       |   25|      61|       27.78|      67.78|",
    "|          |Heinzen     |   29|      90|       32.22|     100.00|"
  )

  expect_identical(
    capture.output(summary(freqlist(TAB.na, na.options = "include"), labelTranslations = c("Treatment", "Ethan Rocks"))),
    ref
  )

  expect_identical(
    capture.output(summary(freqlist(TAB.na, na.options = "include", labelTranslations = list("Treatment", "Ethan Rocks")))),
    ref
  )

  expect_identical(
    capture.output(summary(freqlist(TAB.na, na.options = "include", labelTranslations = c("Treatment", "Ethan Rocks")))),
    ref
  )

  expect_error(freqlist(TAB.na, labelTranslations = c("Treatment", "Ethan Rocks", "Oops!")))
  expect_error(freqlist(TAB.na, labelTranslations = c(hi = "Treatment", ethan = "Ethan Rocks")))

  tmp <- freqlist(TAB.na, na.options = "include")
  labels(tmp) <- c("Treatment", "Ethan Rocks")
  expect_identical(
    capture.output(summary(tmp)),
    ref
  )
  labels(tmp) <- NULL
  expect_identical(
    capture.output(summary(tmp)),
    c(""                                                       ,
      ""                                                       ,
      "|trt |ethan   | Freq| cumFreq| freqPercent| cumPercent|",
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
    capture.output(summary(tmp)),
    ref
  )
})


test_that("Changing the labels on grouped freqlists", {
  expect_identical(
    capture.output(summary(freqlist(TAB.na, options = "include", groupBy = "ethan", labelTranslations = c("Treatment", "Ethan")))),
    c(""                                                             ,
      ""                                                             ,
      "|Ethan |Treatment | Freq| cumFreq| freqPercent| cumPercent|"  ,
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
      "|NA    |A         |    3|       3|         100|        100|"
    )
  )
})


test_that("dupLabels works", {
  expect_identical(
    capture.output(summary(freqlist(TAB), dupLabels = TRUE)),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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
    capture.output(summary(freqlist(TAB, groupBy = "Group"), single = FALSE, dupLabels = TRUE)),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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
    capture.output(summary(freqlist(TAB.na, options = "include", labelTranslations = c("Treatment", "Ethan")), dupLabels = TRUE, title = "Ethan Rocks")),
    c(""                                                             ,
      "Table: Ethan Rocks"                                           ,
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
    capture.output(summary(freqlist(TAB.na, options = "include", groupBy = "ethan",
                                    labelTranslations = c("Treatment", "Ethan")), dupLabels = TRUE, title = "Ethan Rocks")),
    c(""                                                             ,
      "Table: Ethan Rocks"                                           ,
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
      "|NA    |A         |    3|       3|         100|        100|"
    )
  )
})


test_that("Formula method works", {
  expect_identical(
    capture.output(summary(freqlist(TAB.na, options = "include"), labelTranslations = c("Trt", "Ethan"))),
    capture.output(summary(freqlist(~ trt + addNA(ethan), data = mdat), labelTranslations = c("Trt", "Ethan")))
  )
})

###########################################################################################################
#### Reported bugs for freqlist
###########################################################################################################


test_that("11/18/16: Emily Lundt's subsetted table and duplicate label problem", {
  expect_identical(
    capture.output(summary(freqlist(TAB.subset))),
    c(""                                                               ,
      ""                                                               ,
      "|Group |Sex    |Phase | Freq| cumFreq| freqPercent| cumPercent|",
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


