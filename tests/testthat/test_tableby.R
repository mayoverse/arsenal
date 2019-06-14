## Tests for tableby


context("Testing the tableby output")

# "mdat" now defined in helper-data.R

###########################################################################################################
#### Basic two-sided tableby
###########################################################################################################

test_that("A basic two-sided tableby call--no labels, no missings", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + time + dt, data = mdat, numeric.stats = c("meansd", "q1q3", "range")), text = TRUE)),
    c("|             |       High (N=30)       |       Low (N=30)        |       Med (N=30)        |      Total (N=90)       | p value|",
      "|:------------|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|-------:|",
      "|Sex          |                         |                         |                         |                         |   0.733|",
      "|-  Female    |       15 (50.0%)        |       17 (56.7%)        |       14 (46.7%)        |       46 (51.1%)        |        |",
      "|-  Male      |       15 (50.0%)        |       13 (43.3%)        |       16 (53.3%)        |       44 (48.9%)        |        |",
      "|time         |                         |                         |                         |                         |   0.025|",
      "|-  Mean (SD) |      4.567 (1.813)      |      3.167 (2.036)      |      3.833 (2.001)      |      3.856 (2.014)      |        |",
      "|-  Q1, Q3    |      3.250, 6.000       |      1.250, 5.000       |      2.000, 5.000       |      2.000, 6.000       |        |",
      "|-  Range     |      0.000 - 7.000      |      0.000 - 6.000      |      1.000 - 7.000      |      0.000 - 7.000      |        |",
      "|dt           |                         |                         |                         |                         |   0.391|",
      "|-  Median    |       1950-01-07        |       1951-06-13        |       1948-09-13        |       1949-10-07        |        |",
      "|-  Range     | 1935-08-15 - 1968-05-14 | 1937-02-08 - 1959-09-06 | 1939-04-01 - 1958-07-30 | 1935-08-15 - 1968-05-14 |        |"
    )
  )
})

test_that("A basic two-sided tableby call--labels, no missings", {
  skip_if_not_installed("coin")
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + trt + Phase, data = mdat, numeric.stats = c("meansd", "q1q3", "range")), text = TRUE)),
    c("|              |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value|",
      "|:-------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|",
      "|Age in Years  |                 |                 |                 |                 |   0.906|",
      "|-  Mean (SD)  | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  | 39.700 (5.258)  |        |",
      "|-  Q1, Q3     | 36.000, 44.500  | 37.250, 41.750  | 35.250, 44.000  | 36.000, 43.000  |        |",
      "|-  Range      | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 | 29.000 - 53.000 |        |",
      "|Treatment Arm |                 |                 |                 |                 |   0.659|",
      "|-  A          |   14 (46.7%)    |   11 (36.7%)    |   11 (36.7%)    |   36 (40.0%)    |        |",
      "|-  B          |   16 (53.3%)    |   19 (63.3%)    |   19 (63.3%)    |   54 (60.0%)    |        |",
      "|Phase         |                 |                 |                 |                 |   0.008|",
      "|-  I          |   11 (36.7%)    |   12 (40.0%)    |    0 (0.0%)     |   23 (25.6%)    |        |",
      "|-  II         |   10 (33.3%)    |   12 (40.0%)    |   19 (63.3%)    |   41 (45.6%)    |        |",
      "|-  III        |    9 (30.0%)    |    6 (20.0%)    |   11 (36.7%)    |   26 (28.9%)    |        |"
    )
  )
})

test_that("A basic two-sided tableby call--no labels, some missings", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ ethan, data = mdat), text = TRUE)),
    c("|           | High (N=30) | Low (N=30) | Med (N=30) | Total (N=90) | p value|",
      "|:----------|:-----------:|:----------:|:----------:|:------------:|-------:|",
      "|ethan      |             |            |            |              |   0.178|",
      "|-  N-Miss  |      3      |     0      |     0      |      3       |        |",
      "|-  Ethan   | 17 (63.0%)  | 13 (43.3%) | 12 (40.0%) |  42 (48.3%)  |        |",
      "|-  Heinzen | 10 (37.0%)  | 17 (56.7%) | 18 (60.0%) |  45 (51.7%)  |        |"
    )
  )
})

###########################################################################################################
#### Basic one-sided tableby
###########################################################################################################

test_that("A basic one-sided tableby call--no labels, no missings", {
  expect_identical(
    capture.kable(summary(tableby(~ Sex + time + dt, data = mdat), text = TRUE)),
    c("|             |     Overall (N=90)      |",
      "|:------------|:-----------------------:|",
      "|Sex          |                         |",
      "|-  Female    |       46 (51.1%)        |",
      "|-  Male      |       44 (48.9%)        |",
      "|time         |                         |",
      "|-  Mean (SD) |      3.856 (2.014)      |",
      "|-  Range     |      0.000 - 7.000      |",
      "|dt           |                         |",
      "|-  Median    |       1949-10-07        |",
      "|-  Range     | 1935-08-15 - 1968-05-14 |"
    )
  )
})

test_that("A basic one-sided tableby call--labels, no missings", {
  expect_identical(
    capture.kable(summary(tableby(~ Age + trt, data = mdat, numeric.stats = c("meansd", "q1q3", "range")), text = TRUE)),
    c("|              | Overall (N=90)  |",
      "|:-------------|:---------------:|",
      "|Age in Years  |                 |",
      "|-  Mean (SD)  | 39.700 (5.258)  |",
      "|-  Q1, Q3     | 36.000, 43.000  |",
      "|-  Range      | 29.000 - 53.000 |",
      "|Treatment Arm |                 |",
      "|-  A          |   36 (40.0%)    |",
      "|-  B          |   54 (60.0%)    |"
    )
  )
})

test_that("A basic one-sided tableby call--no labels, some missings (Sarah Jenkins's Error)", {
  expect_identical(
    capture.kable(summary(tableby(~ ethan, data = mdat), text = TRUE)),
    c("|           | Overall (N=90) |",
      "|:----------|:--------------:|",
      "|ethan      |                |",
      "|-  N-Miss  |       3        |",
      "|-  Ethan   |   42 (48.3%)   |",
      "|-  Heinzen |   45 (51.7%)   |"
    )
  )
})

###########################################################################################################
#### Change totals/p-values
###########################################################################################################

test_that("A basic two-sided tableby call--no p-value, no total", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + Sex, data = mdat, test = FALSE, total = FALSE), text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |",
      "|:------------|:---------------:|:---------------:|:---------------:|",
      "|Age in Years |                 |                 |                 |",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 |",
      "|Sex          |                 |                 |                 |",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + Sex, data = mdat), test = FALSE, total = FALSE, text = TRUE)),
    capture.kable(summary(tableby(Group ~ Age + Sex, data = mdat, test = FALSE, total = FALSE), text = TRUE))
  )
})

test_that("A basic two-sided tableby call--p-value, no total", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + Sex, data = mdat, total = FALSE), text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|-------:|",
      "|Age in Years |                 |                 |                 |   0.906|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 |        |",
      "|Sex          |                 |                 |                 |   0.733|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + Sex, data = mdat), total = FALSE, text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|-------:|",
      "|Age in Years |                 |                 |                 |   0.906|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 |        |",
      "|Sex          |                 |                 |                 |   0.733|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |        |"
    )
  )
})

###########################################################################################################
#### markdown output
###########################################################################################################

test_that("A basic two-sided tableby markdown output", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + Sex + notest(ethan) + dt, data = mdat,
                                  numeric.stats = c("meansd", "q1q3", "range"), total = FALSE), pfootnote = TRUE)),
    c("|                            |       High (N=30)       |       Low (N=30)        |       Med (N=30)        |  p value|",
      "|:---------------------------|:-----------------------:|:-----------------------:|:-----------------------:|--------:|",
      "|**Age in Years**            |                         |                         |                         | 0.906^1^|",
      "|&nbsp;&nbsp;&nbsp;Mean (SD) |     40.033 (6.217)      |     39.633 (3.873)      |     39.433 (5.569)      |         |",
      "|&nbsp;&nbsp;&nbsp;Q1, Q3    |     36.000, 44.500      |     37.250, 41.750      |     35.250, 44.000      |         |",
      "|&nbsp;&nbsp;&nbsp;Range     |     29.000 - 53.000     |     32.000 - 48.000     |     30.000 - 52.000     |         |",
      "|**Sex**                     |                         |                         |                         | 0.733^2^|",
      "|&nbsp;&nbsp;&nbsp;Female    |       15 (50.0%)        |       17 (56.7%)        |       14 (46.7%)        |         |",
      "|&nbsp;&nbsp;&nbsp;Male      |       15 (50.0%)        |       13 (43.3%)        |       16 (53.3%)        |         |",
      "|**ethan**                   |                         |                         |                         |         |",
      "|&nbsp;&nbsp;&nbsp;N-Miss    |            3            |            0            |            0            |         |",
      "|&nbsp;&nbsp;&nbsp;Ethan     |       17 (63.0%)        |       13 (43.3%)        |       12 (40.0%)        |         |",
      "|&nbsp;&nbsp;&nbsp;Heinzen   |       10 (37.0%)        |       17 (56.7%)        |       18 (60.0%)        |         |",
      "|**dt**                      |                         |                         |                         | 0.391^3^|",
      "|&nbsp;&nbsp;&nbsp;Median    |       1950-01-07        |       1951-06-13        |       1948-09-13        |         |",
      "|&nbsp;&nbsp;&nbsp;Range     | 1935-08-15 - 1968-05-14 | 1937-02-08 - 1959-09-06 | 1939-04-01 - 1958-07-30 |         |",
      "1. Linear Model ANOVA"                                                                                                 ,
      "2. Pearson's Chi-squared test"                                                                                         ,
      "3. Kruskal-Wallis rank sum test"
    )
  )
})


###########################################################################################################
#### Other warnings and tests and things...
###########################################################################################################

test_that("A warning occurs using one-sided formula and na.tableby", {
  expect_error(tableby(~ ethan, data = mdat, na.action = na.tableby), "na.tableby now generates functions")
  expect_warning(tableby(~ ethan, data = mdat, na.action = na.tableby(TRUE)))
})

test_that("The by-variable droplevels is working correctly", {
  expect_identical(
    capture.kable(summary(tableby(Group.fac ~ Sex + time + dt, data = mdat[mdat$Group.fac %in% c("High", "Low"), ]), text = TRUE)),
    c("|             |       High (N=30)       |       Low (N=30)        |      Total (N=60)       | p value|",
      "|:------------|:-----------------------:|:-----------------------:|:-----------------------:|-------:|",
      "|Sex          |                         |                         |                         |   0.605|",
      "|-  Female    |       15 (50.0%)        |       17 (56.7%)        |       32 (53.3%)        |        |",
      "|-  Male      |       15 (50.0%)        |       13 (43.3%)        |       28 (46.7%)        |        |",
      "|time         |                         |                         |                         |   0.007|",
      "|-  Mean (SD) |      4.567 (1.813)      |      3.167 (2.036)      |      3.867 (2.038)      |        |",
      "|-  Range     |      0.000 - 7.000      |      0.000 - 6.000      |      0.000 - 7.000      |        |",
      "|dt           |                         |                         |                         |   0.574|",
      "|-  Median    |       1950-01-07        |       1951-06-13        |       1950-07-02        |        |",
      "|-  Range     | 1935-08-15 - 1968-05-14 | 1937-02-08 - 1959-09-06 | 1935-08-15 - 1968-05-14 |        |"
    )
  )
})

test_that("Using cat.simplify", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + trt, data = mdat, cat.simplify = TRUE), text = TRUE)),
    c("|              | High (N=30) | Low (N=30) | Med (N=30) | Total (N=90) | p value|",
      "|:-------------|:-----------:|:----------:|:----------:|:------------:|-------:|",
      "|Sex           | 15 (50.0%)  | 13 (43.3%) | 16 (53.3%) |  44 (48.9%)  |   0.733|",
      "|Treatment Arm | 16 (53.3%)  | 19 (63.3%) | 19 (63.3%) |  54 (60.0%)  |   0.659|"
    )
  )
})


test_that("Reordering variables", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + dt + Age, data = mdat)[c(3,1,2)], text = TRUE)),
    capture.kable(summary(tableby(Group ~ Age + Sex + dt, data = mdat), text = TRUE))
  )

  expect_identical(
    capture.kable(summary(sort(tableby(Group ~ Sex + dt + Age, data = mdat)))),
    capture.kable(summary(tableby(Group ~ dt + Sex + Age, data = mdat)))
  )

  expect_identical(
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[c(3,1,2)], text = TRUE)),
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[c("Age", "Sex", "dt")], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[1:2], text = TRUE)),
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[c(TRUE, TRUE, FALSE)], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat), text = TRUE)),
    capture.kable(summary(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[], text = TRUE))
  )

  expect_warning(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[1:4], "Some indices not found")
  expect_error(tableby(Group ~ fe(Sex) + dt + Age, data = mdat)[TRUE], "Logical vector")

})


test_that("Merging tableby objects", {
  tb1 <- tableby(Group ~ Sex + Phase, data = mdat)
  tb2 <- tableby(Group.fac ~ Age, data = mdat)
  tb3 <- tableby(Group ~ Age + dt, data = mdat)
  tb4 <- tableby(Group ~ chisq(Sex, "count"), data = mdat)
  expect_error(merge(tb1, tb2), "No terms in common")
  expect_error(merge(tb1, tableby(Group ~ Age, data = set_labels(mdat, list(Group = "Eek")))), "By-variables not identical")
  expect_identical(
    capture.kable(summary(merge(tb1, tb2, all = TRUE))),
    c(capture.kable(summary(tb1)), "", "", capture.kable(summary(tb2)))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, tb3), text = TRUE)),
    capture.kable(summary(tableby(Group ~ Sex + Phase + Age + dt, data = mdat), text = TRUE))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, tb4), text = TRUE)),
    capture.kable(summary(tableby(Group ~ chisq(Sex, "count") + Phase, data = mdat), text = TRUE))
  )
})


test_that("Changing tests", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ fe(Sex) + kwt(Age) + notest(Phase), data = mdat, numeric.stats = c("meansd", "q1q3", "range")), text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|",
      "|Sex          |                 |                 |                 |                 |   0.806|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |   46 (51.1%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |   44 (48.9%)    |        |",
      "|Age in Years |                 |                 |                 |                 |   0.869|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  | 39.700 (5.258)  |        |",
      "|-  Q1, Q3    | 36.000, 44.500  | 37.250, 41.750  | 35.250, 44.000  | 36.000, 43.000  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 | 29.000 - 53.000 |        |",
      "|Phase        |                 |                 |                 |                 |        |",
      "|-  I         |   11 (36.7%)    |   12 (40.0%)    |    0 (0.0%)     |   23 (25.6%)    |        |",
      "|-  II        |   10 (33.3%)    |   12 (40.0%)    |   19 (63.3%)    |   41 (45.6%)    |        |",
      "|-  III       |    9 (30.0%)    |    6 (20.0%)    |   11 (36.7%)    |   26 (28.9%)    |        |"
    )
  )

  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + Age + Phase, data = mdat, numeric.test = "kwt", cat.test = "fe", ordered.test = "notest"), text = TRUE)),
    capture.kable(summary(tableby(Group ~ fe(Sex) + kwt(Age) + notest(Phase), data = mdat), text = TRUE))
  )
})


test_that("Changing labels", {
  tb <- tableby(Group ~ Sex + Age, data = mdat)
  expect_error(labels(tb) <- c("Group", "Sex", "Age"))
  expect_warning(labels(tb) <- c(hi = "hi", Sex = "Sex", Age = "Age"), NA)
  expect_identical(
    capture.kable(summary(tb, text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|",
      "|Sex          |                 |                 |                 |                 |   0.733|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |   46 (51.1%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |   44 (48.9%)    |        |",
      "|Age          |                 |                 |                 |                 |   0.906|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  | 39.700 (5.258)  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 | 29.000 - 53.000 |        |"
    )
  )
  labels(tb) <- NULL
  expect_identical(
    capture.kable(summary(tb, text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|",
      "|Sex          |                 |                 |                 |                 |   0.733|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |   46 (51.1%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |   44 (48.9%)    |        |",
      "|Age          |                 |                 |                 |                 |   0.906|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  | 39.700 (5.258)  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 | 29.000 - 53.000 |        |"
    )
  )
  labels(tb) <- list(Age = "Age (yrs)", Sex = "Gender")
  expect_identical(labels(tb), c(Group = "Group", Sex = "Gender", Age = "Age (yrs)"))
  expect_identical(
    capture.kable(summary(tb, text = TRUE)),
    c("|             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value|",
      "|:------------|:---------------:|:---------------:|:---------------:|:---------------:|-------:|",
      "|Gender       |                 |                 |                 |                 |   0.733|",
      "|-  Female    |   15 (50.0%)    |   17 (56.7%)    |   14 (46.7%)    |   46 (51.1%)    |        |",
      "|-  Male      |   15 (50.0%)    |   13 (43.3%)    |   16 (53.3%)    |   44 (48.9%)    |        |",
      "|Age (yrs)    |                 |                 |                 |                 |   0.906|",
      "|-  Mean (SD) | 40.033 (6.217)  | 39.633 (3.873)  | 39.433 (5.569)  | 39.700 (5.258)  |        |",
      "|-  Range     | 29.000 - 53.000 | 32.000 - 48.000 | 30.000 - 52.000 | 29.000 - 53.000 |        |"
    )
  )
})


round.p <- function(x)
{
  x$p.value <- round(x$p.value, 5)
  row.names(x) <- NULL
  x
}

set.seed(1000)
test_that("05/25/2017: simulate.p.value option for chisq.test", {
  expect_true(identical(
    round.p(tests(tableby(Group ~ Sex + time + dt, data = mdat,  subset=Group != "High",simulate.p.value=TRUE))),
    data.frame(Group = "Group", Variable = c("Sex", "time", "dt"), p.value = c(0.61169, 0.20595, 0.17144),
               Method = c("Pearson's Chi-squared test with simulated p-value\n\t (based on 2000 replicates)",
                          "Linear Model ANOVA", "Kruskal-Wallis rank sum test"), stringsAsFactors = FALSE)
  ))
})

test_that("05/25/2017: chisq.correct=FALSE option for chisq.test", {
  expect_true(identical(
    round.p(tests(tableby(Group ~ Sex + time + dt, data = mdat, subset=Group != "High", chisq.correct=FALSE))),
    data.frame(Group = "Group", Variable = c("Sex", "time", "dt"), p.value = c(0.43832, 0.20595, 0.17144),
               Method = c("Pearson's Chi-squared test", "Linear Model ANOVA", "Kruskal-Wallis rank sum test"),
               stringsAsFactors = FALSE)
  ))
})


set.seed(1000)
test_that("05/25/2017: simulate.p.value=TRUE option for fisher.test", {
  expect_true(identical(
    round.p(tests(tableby(Group ~ fe(Sex) + time + dt, data = mdat, simulate.p.value=TRUE, B = 1999))),
    data.frame(Group = "Group", Variable = c("Sex", "time", "dt"), p.value = c(0.80000, 0.02480, 0.39127),
               Method = c("Fisher's Exact Test for Count Data with simulated p-value\n\t (based on 1999 replicates)",
                          "Linear Model ANOVA", "Kruskal-Wallis rank sum test"), stringsAsFactors = FALSE)
  ))
})


###########################################################################################################
#### Reported bugs for tableby
###########################################################################################################

test_that("02/07/2017: Ryan Lennon's R Markdown spacing problem. Also 02/14/2018 (#65)", {
  expect_error(capture.kable(summary(tableby(Group ~ Sex + time + dt, data = mdat), text = TRUE)), NA)
})

dat <- data.frame(x = c("A", "A", "A", rep(c("B", "C"), each = 7)),
                  y = c("cough", "pneumonia", NA,
                        "chest pain", "chest pain", "chest pain", "cough", "cough", "pneumonia", "cough",
                        "cough", "pneumonia", "chest pain", "chest pain", "pneumonia", NA, NA))
dat$y <- factor(dat$y)

test_that("02/07/2017: Jason Sinnwell's countpct problem", {
  expect_identical(
    capture.kable(summary(tableby(x ~ fe(y), data = dat), text = TRUE)),
    c("|              |  A (N=3)  |  B (N=7)  |  C (N=7)  | Total (N=17) | p value|",
      "|:-------------|:---------:|:---------:|:---------:|:------------:|-------:|",
      "|y             |           |           |           |              |   0.750|",
      "|-  N-Miss     |     1     |     0     |     2     |      3       |        |",
      "|-  chest pain | 0 (0.0%)  | 3 (42.9%) | 2 (40.0%) |  5 (35.7%)   |        |",
      "|-  cough      | 1 (50.0%) | 3 (42.9%) | 1 (20.0%) |  5 (35.7%)   |        |",
      "|-  pneumonia  | 1 (50.0%) | 1 (14.3%) | 2 (40.0%) |  4 (28.6%)   |        |"
    )
  )
})

test_that("02/07/2017: Jason Sinnwell's chisq problem", {
  expect_identical(
    capture.kable(summary(tableby(x ~ y, data = dat[dat$y == "cough",]), text = TRUE)),
    c("|              |  A (N=1)   |  B (N=3)   |  C (N=1)   | Total (N=5) | p value|",
      "|:-------------|:----------:|:----------:|:----------:|:-----------:|-------:|",
      "|y             |            |            |            |             |   1.000|",
      "|-  chest pain |  0 (0.0%)  |  0 (0.0%)  |  0 (0.0%)  |  0 (0.0%)   |        |",
      "|-  cough      | 1 (100.0%) | 3 (100.0%) | 1 (100.0%) | 5 (100.0%)  |        |",
      "|-  pneumonia  |  0 (0.0%)  |  0 (0.0%)  |  0 (0.0%)  |  0 (0.0%)   |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(x ~ as.character(y), data = dat[dat$y == "cough",]), text = TRUE)),
    c("|                |  A (N=1)   |  B (N=3)   |  C (N=1)   | Total (N=5) | p value|",
      "|:---------------|:----------:|:----------:|:----------:|:-----------:|-------:|",
      "|as.character(y) |            |            |            |             |   1.000|",
      "|-  cough        | 1 (100.0%) | 3 (100.0%) | 1 (100.0%) | 5 (100.0%)  |        |"
    )
  )
})
rm(dat)

test_that("03/17/2017: Beth's medianq1q3 label", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ ht_in + time, data = mdat,
                                  control = tableby.control(numeric.stats = c("Nmiss2", "medianq1q3"))), text = TRUE)),
    c("|                   |       High (N=30)       |       Low (N=30)        |       Med (N=30)        |      Total (N=90)       | p value|",
      "|:------------------|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|-------:|",
      "|Height in Inches   |                         |                         |                         |                         |   0.785|",
      "|-  N-Miss          |            0            |            0            |            0            |            0            |        |",
      "|-  Median (Q1, Q3) | 64.500 (62.000, 68.000) | 64.000 (61.000, 68.750) | 64.500 (62.000, 68.000) | 64.000 (62.000, 68.000) |        |",
      "|time               |                         |                         |                         |                         |   0.025|",
      "|-  N-Miss          |            0            |            0            |            0            |            0            |        |",
      "|-  Median (Q1, Q3) |  5.000 (3.250, 6.000)   |  3.000 (1.250, 5.000)   |  4.000 (2.000, 5.000)   |  4.000 (2.000, 6.000)   |        |"
    )
  )
})


test_that("04/12/2017: Katherine King's cat.simplify vs tableby.control", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ trt + Sex, data = mdat, control = tableby.control(), cat.simplify = TRUE), text = TRUE)),
    c("|              | High (N=30) | Low (N=30) | Med (N=30) | Total (N=90) | p value|",
      "|:-------------|:-----------:|:----------:|:----------:|:------------:|-------:|",
      "|Treatment Arm | 16 (53.3%)  | 19 (63.3%) | 19 (63.3%) |  54 (60.0%)  |   0.659|",
      "|Sex           | 15 (50.0%)  | 13 (43.3%) | 16 (53.3%) |  44 (48.9%)  |   0.733|"
    )
  )
})

data(mockstudy)
temp <- mockstudy[1:5,]
test_that("05/24/2017: Katherine King's count vs countpct", {
  expect_identical(
    capture.kable(summary(tableby(arm ~ sex + age, data=temp, cat.stats="count", test = FALSE), text = TRUE)),
    c("|             |  A: IFL (N=2)   | F: FOLFOX (N=2) |  G: IROX (N=1)  |   Total (N=5)   |",
      "|:------------|:---------------:|:---------------:|:---------------:|:---------------:|",
      "|sex          |                 |                 |                 |                 |",
      "|-  Male      |        0        |        1        |        0        |        1        |",
      "|-  Female    |        2        |        1        |        1        |        4        |",
      "|age          |                 |                 |                 |                 |",
      "|-  Mean (SD) | 62.000 (16.971) | 68.000 (1.414)  |   71.000 (NA)   | 66.200 (9.418)  |",
      "|-  Range     | 50.000 - 74.000 | 67.000 - 69.000 | 71.000 - 71.000 | 50.000 - 74.000 |"
    )
  )
})


df <- data.frame(x = c("a ", "a ", "b", "b ", "c", "c"), y = c("A", "A", "A", "B", "B", "B"), stringsAsFactors = FALSE)
##table(df$x, df$y)
test_that("05/24/2017: Missy Larson and Ethan Heinzen trailing spaces on char x variable", {
  expect_identical(
    capture.kable(summary(tableby(y ~ x, data = df, test = FALSE), text = TRUE)),
    c("|     |  A (N=3)  |  B (N=3)  | Total (N=6) |",
      "|:----|:---------:|:---------:|:-----------:|",
      "|x    |           |           |             |",
      "|-  a | 2 (66.7%) | 0 (0.0%)  |  2 (33.3%)  |",
      "|-  b | 1 (33.3%) | 0 (0.0%)  |  1 (16.7%)  |",
      "|-  b | 0 (0.0%)  | 1 (33.3%) |  1 (16.7%)  |",
      "|-  c | 0 (0.0%)  | 2 (66.7%) |  2 (33.3%)  |"
    )
  )
})


test_that("08/02/2017: Chi-square warnings are suppressed", {
  expect_warning(tableby(arm ~ sex, data = mockstudy, subset = 1:5), NA)
})

test_that("08/26/2017: Richard Pendegraft and using formulize and tableby (#21)", {
  # tableby was having trouble identifying one-sided formulas when you use formulize
  expect_warning(tableby(formulize(x = 11, data = mdat), data = mdat, na.action = na.tableby(TRUE)), "It appears you're using na.tableby")

  expect_identical(
    capture.kable(summary(tableby(Group ~ fe(Sex) + kwt(Age), data = mdat), text = TRUE)),
    capture.kable(summary(tableby(formulize("Group", c("fe(Sex)", "kwt(Age)")), data = mdat), text = TRUE))
  )
})

df <- data.frame(a = c("b", "b", "b", "a", "a"), d = NA_character_, e = c(1, 2, 2, 1, 2), stringsAsFactors = FALSE)
test_that("08/30/2017: Brendan Broderick and zero-length levels (#22)", {
  expect_error(tableby(a ~ d + e, data = df), "Zero-length levels")
})


test_that("09/13/2017: Peter Martin and rounding to integers (#23)", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + time + dt, data = mdat,
                                  numeric.stats = c("meansd", "q1q3", "range"), digits = 0, digits.p = 3), text = TRUE)),
    c("|             |       High (N=30)       |       Low (N=30)        |       Med (N=30)        |      Total (N=90)       | p value|",
      "|:------------|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|-------:|",
      "|Sex          |                         |                         |                         |                         |   0.733|",
      "|-  Female    |       15 (50.0%)        |       17 (56.7%)        |       14 (46.7%)        |       46 (51.1%)        |        |",
      "|-  Male      |       15 (50.0%)        |       13 (43.3%)        |       16 (53.3%)        |       44 (48.9%)        |        |",
      "|time         |                         |                         |                         |                         |   0.025|",
      "|-  Mean (SD) |          5 (2)          |          3 (2)          |          4 (2)          |          4 (2)          |        |",
      "|-  Q1, Q3    |          3, 6           |          1, 5           |          2, 5           |          2, 6           |        |",
      "|-  Range     |          0 - 7          |          0 - 6          |          1 - 7          |          0 - 7          |        |",
      "|dt           |                         |                         |                         |                         |   0.391|",
      "|-  Median    |       1950-01-07        |       1951-06-13        |       1948-09-13        |       1949-10-07        |        |",
      "|-  Range     | 1935-08-15 - 1968-05-14 | 1937-02-08 - 1959-09-06 | 1939-04-01 - 1958-07-30 | 1935-08-15 - 1968-05-14 |        |"
    )
  )
  expect_warning(tableby(Group ~ Sex + time + dt, data = mdat, digits.p = -1))
  expect_warning(tableby(Group ~ Sex + time + dt, data = mdat, digits = -1))
})


dat <- data.frame(a = c("b", "b", "b", "a", "a", "a"), b = c("a", "b", "a", "b", "a", "b"), stringsAsFactors = FALSE)
attr(dat$a, "stats") <- c("countpct", "Nmiss")
test_that("11/10/2017: trouble with 'stats' attribute (#39)", {
  expect_error(tableby(~ a + b, data = dat), NA)
})


colnames(dat) <- c("1y", "2x")
test_that("11/15/2017: Krista Goergen and non-syntactic names (#41)", {
  expect_identical(
    capture.kable(summary(tableby(`1y` ~ `2x`, data = dat), text = TRUE)),
    c("|     |  a (N=3)  |  b (N=3)  | Total (N=6) | p value|",
      "|:----|:---------:|:---------:|:-----------:|-------:|",
      "|2x   |           |           |             |   0.414|",
      "|-  a | 1 (33.3%) | 2 (66.7%) |  3 (50.0%)  |        |",
      "|-  b | 2 (66.7%) | 1 (33.3%) |  3 (50.0%)  |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(`1y` ~ fe(`2x`), data = dat), text = TRUE)),
    c("|     |  a (N=3)  |  b (N=3)  | Total (N=6) | p value|",
      "|:----|:---------:|:---------:|:-----------:|-------:|",
      "|2x   |           |           |             |   1.000|",
      "|-  a | 1 (33.3%) | 2 (66.7%) |  3 (50.0%)  |        |",
      "|-  b | 2 (66.7%) | 1 (33.3%) |  3 (50.0%)  |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby( ~ `2x`, data = dat), text = TRUE)),
    c("|     | Overall (N=6) |",
      "|:----|:-------------:|",
      "|2x   |               |",
      "|-  a |   3 (50.0%)   |",
      "|-  b |   3 (50.0%)   |"
    )
  )
})


test_that("7/27/2017: as.data.frame.tableby and dates (#10)", {
  expect_identical(as.data.frame(tableby(~ dt, data = mdat))$Overall[[3]][2], as.Date("1968-05-14"))
})

test_that("01/24/2018: count, countN, and countpct at the same time (#51, #201)", {
  dat <- data.frame(y = rep(c("C", "D"), times = 5), x = rep(c("A", "B"), each = 5), stringsAsFactors = FALSE)
  expect_identical(
    capture.kable(summary(tableby(y ~ x, data = dat, cat.stats = c("count", "countN", "countpct")), text = TRUE)),
    c("|     |  C (N=5)  |  D (N=5)  | Total (N=10) | p value|",
      "|:----|:---------:|:---------:|:------------:|-------:|",
      "|x    |           |           |              |   0.527|",
      "|-  A |     3     |     2     |      5       |        |",
      "|-  B |     2     |     3     |      5       |        |",
      "|-  A |    3/5    |    2/5    |     5/10     |        |",
      "|-  B |    2/5    |    3/5    |     5/10     |        |",
      "|-  A | 3 (60.0%) | 2 (40.0%) |  5 (50.0%)   |        |",
      "|-  B | 2 (40.0%) | 3 (60.0%) |  5 (50.0%)   |        |"
    )
  )
})

test_that("01/30/2018: additional follow-up statistics (#32)", {
  skip_if_not(getRversion() >= "3.3.0")
  skip_if_not_installed("survival", "2.41-3")
  require(survival)
  expect_identical(
    capture.kable(summary(tableby(sex ~ Surv(fu.time/365.25, fu.stat), data=mockstudy, times=1:5,
                                  surv.stats=c("medSurv", "Nevents", "NeventsSurv", "Nrisk", "NriskSurv", "medTime")), text = TRUE)),
    c("|                              | Male (N=916) | Female (N=583) | Total (N=1499) | p value|",
      "|:-----------------------------|:------------:|:--------------:|:--------------:|-------:|",
      "|Surv(fu.time/365.25, fu.stat) |              |                |                |   0.975|",
      "|-  Median Survival            |    1.506     |     1.487      |     1.495      |        |",
      "|-  Events                     |     829      |      527       |      1356      |        |",
      "|-  time = 1                   |  286 (68.7)  |   202 (65.3)   |   488 (67.4)   |        |",
      "|-  time = 2                   |  597 (34.4)  |   391 (32.8)   |   988 (33.7)   |        |",
      "|-  time = 3                   |  748 (17.5)  |   481 (17.0)   |  1229 (17.3)   |        |",
      "|-  time = 4                   |  809 (9.4)   |   513 (10.9)   |  1322 (10.1)   |        |",
      "|-  time = 5                   |  825 (6.3)   |   525 (7.4)    |   1350 (6.8)   |        |",
      "|-  time = 1                   |     626      |      380       |      1006      |        |",
      "|-  time = 2                   |     309      |      190       |      499       |        |",
      "|-  time = 3                   |     152      |       95       |      247       |        |",
      "|-  time = 4                   |      57      |       51       |      108       |        |",
      "|-  time = 5                   |      24      |       18       |       42       |        |",
      "|-  time = 1                   |  626 (68.7)  |   380 (65.3)   |  1006 (67.4)   |        |",
      "|-  time = 2                   |  309 (34.4)  |   190 (32.8)   |   499 (33.7)   |        |",
      "|-  time = 3                   |  152 (17.5)  |   95 (17.0)    |   247 (17.3)   |        |",
      "|-  time = 4                   |   57 (9.4)   |   51 (10.9)    |   108 (10.1)   |        |",
      "|-  time = 5                   |   24 (6.3)   |    18 (7.4)    |    42 (6.8)    |        |",
      "|-  Median Follow-Up           |    4.665     |     4.413      |     4.561      |        |"
    )
  )
})


test_that("01/31/2018 and 6/4/18: row and cell percents (#9, #106)", {
  catstats <- c("Nmiss", "countpct", "countrowpct", "countcellpct", "binomCI", "rowbinomCI")
  expect_identical(
    capture.kable(summary(tableby(Group ~ Sex + ethan, data = mdat, cat.stats = catstats), text = TRUE)),
    c("|           |     High (N=30)      |      Low (N=30)      |      Med (N=30)      |     Total (N=90)     | p value|",
      "|:----------|:--------------------:|:--------------------:|:--------------------:|:--------------------:|-------:|",
      "|Sex        |                      |                      |                      |                      |   0.733|",
      "|-  Female  |      15 (50.0%)      |      17 (56.7%)      |      14 (46.7%)      |      46 (51.1%)      |        |",
      "|-  Male    |      15 (50.0%)      |      13 (43.3%)      |      16 (53.3%)      |      44 (48.9%)      |        |",
      "|-  Female  |      15 (32.6%)      |      17 (37.0%)      |      14 (30.4%)      |     46 (100.0%)      |        |",
      "|-  Male    |      15 (34.1%)      |      13 (29.5%)      |      16 (36.4%)      |     44 (100.0%)      |        |",
      "|-  Female  |      15 (16.7%)      |      17 (18.9%)      |      14 (15.6%)      |      46 (51.1%)      |        |",
      "|-  Male    |      15 (16.7%)      |      13 (14.4%)      |      16 (17.8%)      |      44 (48.9%)      |        |",
      "|-  Female  | 0.500 (0.313, 0.687) | 0.567 (0.374, 0.745) | 0.467 (0.283, 0.657) | 0.511 (0.403, 0.618) |        |",
      "|-  Male    | 0.500 (0.313, 0.687) | 0.433 (0.255, 0.626) | 0.533 (0.343, 0.717) | 0.489 (0.382, 0.597) |        |",
      "|-  Female  | 0.326 (0.195, 0.480) | 0.370 (0.232, 0.525) | 0.304 (0.177, 0.458) | 1.000 (0.923, 1.000) |        |",
      "|-  Male    | 0.341 (0.205, 0.499) | 0.295 (0.168, 0.452) | 0.364 (0.224, 0.522) | 1.000 (0.920, 1.000) |        |",
      "|ethan      |                      |                      |                      |                      |   0.178|",
      "|-  N-Miss  |          3           |          0           |          0           |          3           |        |",
      "|-  Ethan   |      17 (63.0%)      |      13 (43.3%)      |      12 (40.0%)      |      42 (48.3%)      |        |",
      "|-  Heinzen |      10 (37.0%)      |      17 (56.7%)      |      18 (60.0%)      |      45 (51.7%)      |        |",
      "|-  Ethan   |      17 (40.5%)      |      13 (31.0%)      |      12 (28.6%)      |     42 (100.0%)      |        |",
      "|-  Heinzen |      10 (22.2%)      |      17 (37.8%)      |      18 (40.0%)      |     45 (100.0%)      |        |",
      "|-  Ethan   |      17 (19.5%)      |      13 (14.9%)      |      12 (13.8%)      |      42 (48.3%)      |        |",
      "|-  Heinzen |      10 (11.5%)      |      17 (19.5%)      |      18 (20.7%)      |      45 (51.7%)      |        |",
      "|-  Ethan   | 0.630 (0.424, 0.806) | 0.433 (0.255, 0.626) | 0.400 (0.227, 0.594) | 0.483 (0.374, 0.592) |        |",
      "|-  Heinzen | 0.370 (0.194, 0.576) | 0.567 (0.374, 0.745) | 0.600 (0.406, 0.773) | 0.517 (0.408, 0.626) |        |",
      "|-  Ethan   | 0.405 (0.256, 0.567) | 0.310 (0.176, 0.471) | 0.286 (0.157, 0.446) | 1.000 (0.916, 1.000) |        |",
      "|-  Heinzen | 0.222 (0.112, 0.371) | 0.378 (0.238, 0.535) | 0.400 (0.257, 0.557) | 1.000 (0.921, 1.000) |        |"
    )
  )
})

test_that("01/31/2018: include NAs in percents (#57, #62)", {
  mdat2 <- mdat
  attr(mdat2$ethan, "label") <- "Ethan"
  expect_identical(
    capture.kable(summary(tableby(Sex ~ includeNA(ethan, label = "N-Miss") + includeNA(ethan, first = TRUE, label = "N-Miss"),
                                  data = mdat2, cat.stats = "countrowpct"), text = TRUE)),
    c("|           | Female (N=46) | Male (N=44) | Total (N=90) | p value|",
      "|:----------|:-------------:|:-----------:|:------------:|-------:|",
      "|Ethan      |               |             |              |   0.229|",
      "|-  Ethan   |  18 (42.9%)   | 24 (57.1%)  | 42 (100.0%)  |        |",
      "|-  Heinzen |  27 (60.0%)   | 18 (40.0%)  | 45 (100.0%)  |        |",
      "|-  N-Miss  |   1 (33.3%)   |  2 (66.7%)  |  3 (100.0%)  |        |",
      "|Ethan      |               |             |              |   0.229|",
      "|-  N-Miss  |   1 (33.3%)   |  2 (66.7%)  |  3 (100.0%)  |        |",
      "|-  Ethan   |  18 (42.9%)   | 24 (57.1%)  | 42 (100.0%)  |        |",
      "|-  Heinzen |  27 (60.0%)   | 18 (40.0%)  | 45 (100.0%)  |        |"
    )
  )

  expect_identical(
    capture.kable(summary(tableby(Sex ~ includeNA(ethan, label = "N-Miss"), data = mdat2, cat.stats = "countpct"), text = TRUE)),
    c("|           | Female (N=46) | Male (N=44) | Total (N=90) | p value|",
      "|:----------|:-------------:|:-----------:|:------------:|-------:|",
      "|Ethan      |               |             |              |   0.229|",
      "|-  Ethan   |  18 (39.1%)   | 24 (54.5%)  |  42 (46.7%)  |        |",
      "|-  Heinzen |  27 (58.7%)   | 18 (40.9%)  |  45 (50.0%)  |        |",
      "|-  N-Miss  |   1 (2.2%)    |  2 (4.5%)   |   3 (3.3%)   |        |"
    )
  )
})

test_that("02/23/2018: wrapping long labels (#59)", {
  labs <- list(
    Group = "This is a really long label for the Group variable",
    time = "Another really long label. Can you believe how long this is",
    dt = "ThisLabelHasNoSpacesSoLetsSeeHowItBehaves"
  )
  expect_identical(
    capture.kable(print(summary(tableby(Sex ~ Group + time + dt, data = set_labels(mdat, labs)), text = TRUE), width = 30)),
    c("|                               |      Female (N=46)      |       Male (N=44)       |      Total (N=90)       | p value|",
      "|:------------------------------|:-----------------------:|:-----------------------:|:-----------------------:|-------:|",
      "|This is a really long label    |                         |                         |                         |   0.733|",
      "|for the Group variable         |                         |                         |                         |        |",
      "|-  High                        |       15 (32.6%)        |       15 (34.1%)        |       30 (33.3%)        |        |",
      "|-  Low                         |       17 (37.0%)        |       13 (29.5%)        |       30 (33.3%)        |        |",
      "|-  Med                         |       14 (30.4%)        |       16 (36.4%)        |       30 (33.3%)        |        |",
      "|Another really long label.     |                         |                         |                         |   0.237|",
      "|Can you believe how long this  |                         |                         |                         |        |",
      "|is                             |                         |                         |                         |        |",
      "|-  Mean (SD)                   |      3.609 (1.926)      |      4.114 (2.093)      |      3.856 (2.014)      |        |",
      "|-  Range                       |      0.000 - 7.000      |      0.000 - 7.000      |      0.000 - 7.000      |        |",
      "|ThisLabelHasNoSpacesSoLetsSeeH |                         |                         |                         |   0.339|",
      "|owItBehaves                    |                         |                         |                         |        |",
      "|-  Median                      |       1948-12-07        |       1951-03-26        |       1949-10-07        |        |",
      "|-  Range                       | 1935-08-15 - 1959-09-06 | 1937-02-08 - 1968-05-14 | 1935-08-15 - 1968-05-14 |        |"
    )
  )
})


test_that("02/26/2018: all NA vars (#80, #81, #82, #83, #84)", {
  dat <- data.frame(y = factor(c("A", "A", "A", "B", "B")), x = c(1, 2, 3, NA, NA))
  expect_identical(
    capture.kable(summary(tableby(y ~ x, data = dat, numeric.test = "anova"), text = TRUE)),
    c("|             |    A (N=3)    | B (N=2) |  Total (N=5)  | p value|",
      "|:------------|:-------------:|:-------:|:-------------:|-------:|",
      "|x            |               |         |               |        |",
      "|-  N-Miss    |       0       |    2    |       2       |        |",
      "|-  Mean (SD) | 2.000 (1.000) |   NA    | 2.000 (1.000) |        |",
      "|-  Range     | 1.000 - 3.000 |   NA    | 1.000 - 3.000 |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(y ~ x, data = dat, numeric.test = "kwt"), text = TRUE)),
    c("|             |    A (N=3)    | B (N=2) |  Total (N=5)  | p value|",
      "|:------------|:-------------:|:-------:|:-------------:|-------:|",
      "|x            |               |         |               |        |",
      "|-  N-Miss    |       0       |    2    |       2       |        |",
      "|-  Mean (SD) | 2.000 (1.000) |   NA    | 2.000 (1.000) |        |",
      "|-  Range     | 1.000 - 3.000 |   NA    | 1.000 - 3.000 |        |"
    )
  )

  dat2 <- data.frame(Group = rep(1:2, each=5), A = rep(c(1, NA), each=5), B = rep(factor(c("A", NA)), each=5))
  expect_identical(
    capture.kable(summary(tableby(Group ~ A + B, data = dat2), text = TRUE)),
    c("|             |    1 (N=5)    | 2 (N=5) | Total (N=10)  | p value|",
      "|:------------|:-------------:|:-------:|:-------------:|-------:|",
      "|A            |               |         |               |        |",
      "|-  N-Miss    |       0       |    5    |       5       |        |",
      "|-  Mean (SD) | 1.000 (0.000) |   NA    | 1.000 (0.000) |        |",
      "|-  Range     | 1.000 - 1.000 |   NA    | 1.000 - 1.000 |        |",
      "|B            |               |         |               |   1.000|",
      "|-  N-Miss    |       0       |    5    |       5       |        |",
      "|-  A         |  5 (100.0%)   |    0    |  5 (100.0%)   |        |"
    )
  )

  skip_if_not(getRversion() >= "3.3.0")
  skip_if_not_installed("survival", "2.41-3")
  require(survival)
  expect_identical(
    capture.kable(summary(tableby(y ~ Surv(x), data=dat, times = 1:2,
                                  surv.stats=c("medSurv", "Nevents", "NeventsSurv", "Nrisk", "NriskSurv", "medTime")), text = TRUE)),
    c("|                    | A (N=3)  | B (N=2) | Total (N=5) | p value|",
      "|:-------------------|:--------:|:-------:|:-----------:|-------:|",
      "|Surv(x)             |          |         |             |        |",
      "|-  Median Survival  |  2.000   |   NA    |    2.000    |        |",
      "|-  Events           |    3     |   NA    |      3      |        |",
      "|-  time = 1         | 1 (66.7) |   NA    |  1 (66.7)   |        |",
      "|-  time = 2         | 2 (33.3) |   NA    |  2 (33.3)   |        |",
      "|-  time = 1         |    3     |   NA    |      3      |        |",
      "|-  time = 2         |    2     |   NA    |      2      |        |",
      "|-  time = 1         | 3 (66.7) |   NA    |  3 (66.7)   |        |",
      "|-  time = 2         | 2 (33.3) |   NA    |  2 (33.3)   |        |",
      "|-  Median Follow-Up |    NA    |   NA    |     NA      |        |"
    )
  )
})


test_that("03/07/2018: quantiles for dates and IQR (#86)", {
  expect_identical(
    capture.kable(summary(tableby(Sex ~ dt + ht_in + Age, data = mdat,
                                  numeric.stats = c("q1q3", "iqr"), date.stats = c("q1q3", "iqr")), text = TRUE)),
    c("|                 |     Female (N=46)      |      Male (N=44)       |      Total (N=90)      | p value|",
      "|:----------------|:----------------------:|:----------------------:|:----------------------:|-------:|",
      "|dt               |                        |                        |                        |   0.339|",
      "|-  Q1, Q3        | 1946-04-26, 1953-11-07 | 1946-11-27, 1954-06-13 | 1946-06-13, 1954-04-26 |        |",
      "|-  IQR           |        2751.250        |        2755.500        |        2873.250        |        |",
      "|Height in Inches |                        |                        |                        |   0.786|",
      "|-  Q1, Q3        |     61.250, 68.000     |     62.000, 68.000     |     62.000, 68.000     |        |",
      "|-  IQR           |         6.750          |         6.000          |         6.000          |        |",
      "|Age in Years     |                        |                        |                        |   0.818|",
      "|-  Q1, Q3        |     36.000, 44.000     |     37.000, 41.250     |     36.000, 43.000     |        |",
      "|-  IQR           |         8.000          |         4.250          |         7.000          |        |"
    )
  )
})

test_that("06/19/2018: term.name (#109)", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ ethan, data = mdat), text = TRUE, term.name = "Term")),
    c("|Term       | High (N=30) | Low (N=30) | Med (N=30) | Total (N=90) | p value|",
      "|:----------|:-----------:|:----------:|:----------:|:------------:|-------:|",
      "|ethan      |             |            |            |              |   0.178|",
      "|-  N-Miss  |      3      |     0      |     0      |      3       |        |",
      "|-  Ethan   | 17 (63.0%)  | 13 (43.3%) | 12 (40.0%) |  42 (48.3%)  |        |",
      "|-  Heinzen | 10 (37.0%)  | 17 (56.7%) | 18 (60.0%) |  45 (51.7%)  |        |"
    )
  )
})

mockstudy$grp <- c(rep("Group1", 749), rep("Group2",749), "")
test_that("08/23/2018: empty string in by-variable (#121)", expect_warning(summary(tableby(grp ~ race, data=mockstudy)), "Empty"))

test_that("08/24/2018: latex (#123)", {
  expect_identical(
    capture.output(summary(tableby(Group ~ ethan, data = mdat), text = "latex")),
    c(""                                                                     ,
      "\\begin{tabular}{l|c|c|c|c|r}"                                        ,
      "\\hline"                                                              ,
      " & High (N=30) & Low (N=30) & Med (N=30) & Total (N=90) & p value\\\\",
      "\\hline"                                                              ,
      "\\textbf{ethan} &  &  &  &  & 0.178\\\\"                              ,
      "\\hline"                                                              ,
      "~~~N-Miss & 3 & 0 & 0 & 3 & \\\\"                                     ,
      "\\hline"                                                              ,
      "~~~Ethan & 17 (63.0%) & 13 (43.3%) & 12 (40.0%) & 42 (48.3%) & \\\\"  ,
      "\\hline"                                                              ,
      "~~~Heinzen & 10 (37.0%) & 17 (56.7%) & 18 (60.0%) & 45 (51.7%) & \\\\",
      "\\hline"                                                              ,
      "\\end{tabular}"                                                       ,
      ""
    )
  )
})


test_that("09/07/2018: using countpct with numerics (#137)", {
  expect_identical(
    capture.kable(summary(tableby(y ~ chisq(x, "countpct"), data = data.frame(y = c("A", "B", "C"), x = c(1, 2, 3))), text = TRUE)),
    c("|     |  A (N=1)   |  B (N=1)   |  C (N=1)   | Total (N=3) | p value|",
      "|:----|:----------:|:----------:|:----------:|:-----------:|-------:|",
      "|x    |            |            |            |             |   0.199|",
      "|-  1 | 1 (100.0%) |  0 (0.0%)  |  0 (0.0%)  |  1 (33.3%)  |        |",
      "|-  2 |  0 (0.0%)  | 1 (100.0%) |  0 (0.0%)  |  1 (33.3%)  |        |",
      "|-  3 |  0 (0.0%)  |  0 (0.0%)  | 1 (100.0%) |  1 (33.3%)  |        |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(sex ~ anova(ps, "count", "meansd"), data = mockstudy), text = TRUE)),
    c("|             | Male (N=916)  | Female (N=583) | Total (N=1499) | p value|",
      "|:------------|:-------------:|:--------------:|:--------------:|-------:|",
      "|ps           |               |                |                |   0.345|",
      "|-  0         |      391      |      244       |      635       |        |",
      "|-  1         |      329      |      202       |      531       |        |",
      "|-  2         |      34       |       33       |       67       |        |",
      "|-  Mean (SD) | 0.527 (0.583) | 0.559 (0.621)  | 0.539 (0.598)  |        |"
    )
  )
})

test_that("09/07/2018: specifying different digits (#107) and cat.simplify (#134) and numeric.simplify (#139)", {
  tmp.mockstudy <- mockstudy
  tmp.mockstudy$date <- as.Date("2019-03-01") + c(rep(1, times = 750), rep(2, times = 749))
  tmp.mockstudy$date2 <- tmp.mockstudy$date
  tmp.mockstudy$ord <- ordered(c(rep("A", times = 749), rep("B", times = 750)))
  tmp.mockstudy$ord2 <- tmp.mockstudy$ord
  expect_identical(
    capture.kable(summary(tableby(arm ~ I(age/10) + chisq(sex, digits.count=1, digits.pct=0, cat.simplify=TRUE) + race +
                                    anova(ast, digits=0, digits.count=1) + kwt(fu.time, "medianq1q3", digits=0) +
                                    kwt(date, date.simplify=TRUE) + notest(ord, ordered.simplify=TRUE) + date2 + notest(ord2),
                                  numeric.simplify=TRUE, date.stats = "median", data = tmp.mockstudy), text=TRUE)),
    c("|                    | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value|",
      "|:-------------------|:--------------:|:-----------------:|:---------------:|:--------------:|-------:|",
      "|Age in Years        |                |                   |                 |                |   0.614|",
      "|-  Mean (SD)        | 5.967 (1.136)  |   6.030 (1.163)   |  5.976 (1.150)  | 5.999 (1.152)  |        |",
      "|-  Range            | 2.700 - 8.800  |   1.900 - 8.800   |  2.600 - 8.500  | 1.900 - 8.800  |        |",
      "|sex                 |  151.0 (35%)   |    280.0 (41%)    |   152.0 (40%)   |  583.0 (39%)   |   0.190|",
      "|Race                |                |                   |                 |                |   0.367|",
      "|-  N-Miss           |       0        |         6         |        1        |       7        |        |",
      "|-  African-Am       |   39 (9.1%)    |     49 (7.2%)     |    27 (7.1%)    |   115 (7.7%)   |        |",
      "|-  Asian            |    1 (0.2%)    |     14 (2.0%)     |    3 (0.8%)     |   18 (1.2%)    |        |",
      "|-  Caucasian        |  371 (86.7%)   |    586 (85.5%)    |   331 (87.3%)   |  1288 (86.3%)  |        |",
      "|-  Hawaii/Pacific   |    1 (0.2%)    |     3 (0.4%)      |    1 (0.3%)     |    5 (0.3%)    |        |",
      "|-  Hispanic         |   12 (2.8%)    |     28 (4.1%)     |    14 (3.7%)    |   54 (3.6%)    |        |",
      "|-  Native-Am/Alaska |    2 (0.5%)    |     1 (0.1%)      |    2 (0.5%)     |    5 (0.3%)    |        |",
      "|-  Other            |    2 (0.5%)    |     4 (0.6%)      |    1 (0.3%)     |    7 (0.5%)    |        |",
      "|ast                 |                |                   |                 |                |   0.507|",
      "|-  N-Miss           |      69.0      |       141.0       |      56.0       |     266.0      |        |",
      "|-  Mean (SD)        |    37 (28)     |      35 (27)      |     36 (26)     |    36 (27)     |        |",
      "|-  Range            |    10 - 205    |      7 - 174      |     5 - 176     |    5 - 205     |        |",
      "|fu.time             | 446 (256, 724) |  601 (345, 1046)  | 516 (306, 807)  | 542 (310, 878) | < 0.001|",
      "|date                |   2019-03-02   |    2019-03-03     |   2019-03-02    |   2019-03-02   | < 0.001|",
      "|ord                 |  170 (39.7%)   |    439 (63.5%)    |   141 (37.1%)   |  750 (50.0%)   |        |",
      "|date2               |                |                   |                 |                | < 0.001|",
      "|-  Median           |   2019-03-02   |    2019-03-03     |   2019-03-02    |   2019-03-02   |        |",
      "|ord2                |                |                   |                 |                |        |",
      "|-  A                |  258 (60.3%)   |    252 (36.5%)    |   239 (62.9%)   |  749 (50.0%)   |        |",
      "|-  B                |  170 (39.7%)   |    439 (63.5%)    |   141 (37.1%)   |  750 (50.0%)   |        |"
    )
  )
})


test_that("09/19/2018: specifying different stats for character and logical variables (#142)", {
  expect_identical(
    capture.kable(summary(tableby(arm ~ chisq(race, "countpct") + chisq(I(sex == "Male"), "count"), data = mockstudy), text = TRUE)),
    c('|                    | A: IFL (N=428) | F: FOLFOX (N=691) | G: IROX (N=380) | Total (N=1499) | p value|',
      '|:-------------------|:--------------:|:-----------------:|:---------------:|:--------------:|-------:|',
      '|Race                |                |                   |                 |                |   0.367|',
      '|-  African-Am       |   39 (9.1%)    |     49 (7.2%)     |    27 (7.1%)    |   115 (7.7%)   |        |',
      '|-  Asian            |    1 (0.2%)    |     14 (2.0%)     |    3 (0.8%)     |   18 (1.2%)    |        |',
      '|-  Caucasian        |  371 (86.7%)   |    586 (85.5%)    |   331 (87.3%)   |  1288 (86.3%)  |        |',
      '|-  Hawaii/Pacific   |    1 (0.2%)    |     3 (0.4%)      |    1 (0.3%)     |    5 (0.3%)    |        |',
      '|-  Hispanic         |   12 (2.8%)    |     28 (4.1%)     |    14 (3.7%)    |   54 (3.6%)    |        |',
      '|-  Native-Am/Alaska |    2 (0.5%)    |     1 (0.1%)      |    2 (0.5%)     |    5 (0.3%)    |        |',
      '|-  Other            |    2 (0.5%)    |     4 (0.6%)      |    1 (0.3%)     |    7 (0.5%)    |        |',
      '|I(sex == "Male")    |                |                   |                 |                |   0.190|',
      '|-  FALSE            |      151       |        280        |       152       |      583       |        |',
      '|-  TRUE             |      277       |        411        |       228       |      916       |        |'
    )
  )
})


test_that("10/19/2018: padjust works on tableby objects (#146)", {
  tab <- tableby(sex ~ age + arm + race + ps + alk.phos, data = mockstudy)
  expect_identical(
    capture.kable(summary(padjust(tab, method = "bonfer"), text = TRUE)),
    c("|                    |   Male (N=916)    |  Female (N=583)   |  Total (N=1499)   | p value|",
      "|:-------------------|:-----------------:|:-----------------:|:-----------------:|-------:|",
      "|Age in Years        |                   |                   |                   |   0.238|",
      "|-  Mean (SD)        |  60.455 (11.369)  |  59.247 (11.722)  |  59.985 (11.519)  |        |",
      "|-  Range            |  19.000 - 88.000  |  22.000 - 88.000  |  19.000 - 88.000  |        |",
      "|Treatment Arm       |                   |                   |                   |   0.952|",
      "|-  A: IFL           |    277 (30.2%)    |    151 (25.9%)    |    428 (28.6%)    |        |",
      "|-  F: FOLFOX        |    411 (44.9%)    |    280 (48.0%)    |    691 (46.1%)    |        |",
      "|-  G: IROX          |    228 (24.9%)    |    152 (26.1%)    |    380 (25.4%)    |        |",
      "|Race                |                   |                   |                   |   1.000|",
      "|-  N-Miss           |         6         |         1         |         7         |        |",
      "|-  African-Am       |     65 (7.1%)     |     50 (8.6%)     |    115 (7.7%)     |        |",
      "|-  Asian            |     11 (1.2%)     |     7 (1.2%)      |     18 (1.2%)     |        |",
      "|-  Caucasian        |    787 (86.5%)    |    501 (86.1%)    |   1288 (86.3%)    |        |",
      "|-  Hawaii/Pacific   |     2 (0.2%)      |     3 (0.5%)      |     5 (0.3%)      |        |",
      "|-  Hispanic         |     37 (4.1%)     |     17 (2.9%)     |     54 (3.6%)     |        |",
      "|-  Native-Am/Alaska |     3 (0.3%)      |     2 (0.3%)      |     5 (0.3%)      |        |",
      "|-  Other            |     5 (0.5%)      |     2 (0.3%)      |     7 (0.5%)      |        |",
      "|ps                  |                   |                   |                   |   1.000|",
      "|-  N-Miss           |        162        |        104        |        266        |        |",
      "|-  Mean (SD)        |   0.527 (0.583)   |   0.559 (0.621)   |   0.539 (0.598)   |        |",
      "|-  Range            |   0.000 - 2.000   |   0.000 - 2.000   |   0.000 - 2.000   |        |",
      "|alk.phos            |                   |                   |                   |   1.000|",
      "|-  N-Miss           |        162        |        104        |        266        |        |",
      "|-  Mean (SD)        | 167.893 (130.754) | 170.664 (124.965) | 168.969 (128.492) |        |",
      "|-  Range            | 10.000 - 1014.000 |  7.000 - 771.000  | 7.000 - 1014.000  |        |"
    )
  )
  expect_identical(
    capture.kable(summary(padjust(tab, "bonfer"), pfootnote = TRUE)),
    capture.kable(padjust(summary(tab, pfootnote = TRUE), "bonfer"))
  )
})

test_that("02/26/2019: digits and stats are maintained when subsetting (#182, #183)", {
  mck <- mockstudy
  attr(mck$arm, "name") <- "armm"
  tmp <- tableby(sex ~ kwt(age, digits = 1, "meansd") + chisq(arm, "count", digits.count = 1), data = mck, subset = age < 65)
  expect_identical(
    capture.kable(summary(tmp)),
    c("|                            | Male (N=552) | Female (N=375) | Total (N=927) | p value|",
      "|:---------------------------|:------------:|:--------------:|:-------------:|-------:|",
      "|**Age in Years**            |              |                |               |   0.143|",
      "|&nbsp;&nbsp;&nbsp;Mean (SD) |  53.3 (8.4)  |   52.5 (8.6)   |  53.0 (8.5)   |        |",
      "|**Treatment Arm**           |              |                |               |   0.404|",
      "|&nbsp;&nbsp;&nbsp;A: IFL    |    169.0     |     100.0      |     269.0     |        |",
      "|&nbsp;&nbsp;&nbsp;F: FOLFOX |    246.0     |     173.0      |     419.0     |        |",
      "|&nbsp;&nbsp;&nbsp;G: IROX   |    137.0     |     102.0      |     239.0     |        |"
    )
  )
})

test_that("03/27/2019: cat.simplify and numeric.simplify work right, even with custom stats (#199, #200, #203)", {
  dat <- data.frame(x = c("A", "A"))
  expect_identical(
    capture.kable(summary(tableby(~ x, data = dat, numeric.simplify = TRUE), text = TRUE)),
    c("|     | Overall (N=2) |",
      "|:----|:-------------:|",
      "|x    |               |",
      "|-  A |  2 (100.0%)   |"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(~ x, data = dat, cat.simplify = TRUE), text = TRUE)),
    c("|   | Overall (N=2) |",
      "|:--|:-------------:|",
      "|x  |  2 (100.0%)   |"
    )
  )
  # mystat <- countpct
  # expect_identical(
  #   capture.kable(summary(tableby(~ x, data = dat, cat.simplify = TRUE, cat.stats = "mystat"), text = TRUE)),
  #   c("|   | Overall (N=2) |",
  #     "|:--|:-------------:|",
  #     "|x  |  2 (100.0%)   |"
  #   )
  # )
})

test_that("04/12/2019: Missing Surv()[,2] (#208)", {
  skip_if_not(getRversion() >= "3.3.0")
  skip_if_not_installed("survival", "2.41-3")
  require(survival)
  dat <- data.frame(by = c(1, 1, 2), time = c(1, 2, 2), event = c(0, NA, 1))
  expect_identical(
    capture.kable(summary(tableby(by ~ Surv(time, event), data = dat), text = TRUE)),
    c("|                   | 1 (N=2) | 2 (N=1) | Total (N=3) | p value|",
      "|:------------------|:-------:|:-------:|:-----------:|-------:|",
      "|Surv(time, event)  |         |         |             |   1.000|",
      "|-  N-Miss          |    1    |    0    |      1      |        |",
      "|-  Events          |    0    |    1    |      1      |        |",
      "|-  Median Survival |   NA    |  2.000  |    2.000    |        |"
    )
  )
})


test_that("06/12/2019: labelTranslations for non-default stat tests (#220, #222)", {
  expect_identical(
    capture.kable(summary(tableby(sex ~ age + kwt(fu.time), data = mockstudy), labelTranslations = list(fu.time = "FU time"), text = TRUE)),
    c("|             |   Male (N=916)    |  Female (N=583)   |  Total (N=1499)   | p value|",
      "|:------------|:-----------------:|:-----------------:|:-----------------:|-------:|",
      "|Age in Years |                   |                   |                   |   0.048|",
      "|-  Mean (SD) |  60.455 (11.369)  |  59.247 (11.722)  |  59.985 (11.519)  |        |",
      "|-  Range     |  19.000 - 88.000  |  22.000 - 88.000  |  19.000 - 88.000  |        |",
      "|FU time      |                   |                   |                   |   0.679|",
      "|-  Mean (SD) | 649.345 (454.332) | 648.674 (475.472) | 649.084 (462.511) |        |",
      "|-  Range     | 0.000 - 2472.000  | 9.000 - 2441.000  | 0.000 - 2472.000  |        |"
    )
  )

  expect_identical(
    capture.kable(summary(tableby(sex ~ age + kwt(fu.time), data = mockstudy), labelTranslations = list(fu.time = "FU time"), text = TRUE)),
    capture.kable(summary(tableby(sex ~ age + kwt(fu.time), data = mockstudy), labelTranslations = list(`kwt(fu.time)` = "FU time"), text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tableby(sex ~ kwt(fu.time), data = set_labels(mockstudy, list(fu.time = "FU time"))), labelTranslations = list(NULL))),
    capture.kable(summary(tableby(sex ~ kwt(fu.time), data = mockstudy)))
  )

})
