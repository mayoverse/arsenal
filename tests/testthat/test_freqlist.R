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

###########################################################################################################
#### Basic freqlist
###########################################################################################################

test_that("A basic freqlist call", {
  expect_true(
    identical(capture.output(summary(freqlist(TAB))),
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
  )
})

test_that("A basic freqlist call--subsetted", {
  expect_true(
    identical(capture.output(summary(freqlist(TAB.subset))),
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
  )
})

test_that("groupBy option in freqlist call", {
  expect_true(
    identical(capture.output(summary(freqlist(TAB, groupBy = "Group"), single = FALSE)),
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
  )
})