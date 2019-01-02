## Tests for tableby


context("Testing the tableby strata and multiple LHS output")

# "mdat" now defined in helper-data.R

###########################################################################################################
#### Basic two-sided tableby
###########################################################################################################

test_that("A three-LHS tableby call", {
  expect_identical(
    capture.kable(summary(tableby(list(Group, trt, ethan) ~ Sex + time + dt + Phase, data = mdat), text = TRUE)),
    c(
      capture.kable(summary(tableby(Group ~ Sex + time + dt + Phase, data = mdat), text = TRUE)), "", "",
      capture.kable(summary(tableby(trt ~ Sex + time + dt + Phase, data = mdat), text = TRUE)), "", "",
      capture.kable(summary(tableby(ethan ~ Sex + time + dt + Phase, data = mdat), text = TRUE))
    )
  )
})

test_that("A tableby call with strata", {
  expect_identical(
    capture.kable(summary(tableby(Group ~ Age + time + Phase, data = mdat, strata = trt), text = TRUE)),
    c("|Treatment Arm |             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value |",
      "|:-------------|:------------|:---------------:|:---------------:|:---------------:|:---------------:|:-------:|",
      "|A             |Age in Years |                 |                 |                 |                 |  0.918  |",
      "|              |-  Mean (SD) | 41.000 (6.493)  | 40.091 (4.571)  | 40.364 (5.573)  | 40.528 (5.537)  |         |",
      "|              |-  Range     | 29.000 - 53.000 | 33.000 - 48.000 | 30.000 - 49.000 | 29.000 - 53.000 |         |",
      "|              |time         |                 |                 |                 |                 |  0.319  |",
      "|              |-  Mean (SD) |  4.357 (1.865)  |  3.273 (1.421)  |  3.727 (1.954)  |  3.833 (1.781)  |         |",
      "|              |-  Range     |  0.000 - 6.000  |  1.000 - 5.000  |  1.000 - 7.000  |  0.000 - 7.000  |         |",
      "|              |Phase        |                 |                 |                 |                 |  0.506  |",
      "|              |-  I         |    6 (42.9%)    |    2 (18.2%)    |    0 (0.0%)     |    8 (22.2%)    |         |",
      "|              |-  II        |    3 (21.4%)    |    6 (54.5%)    |    8 (72.7%)    |   17 (47.2%)    |         |",
      "|              |-  III       |    5 (35.7%)    |    3 (27.3%)    |    3 (27.3%)    |   11 (30.6%)    |         |",
      "|B             |Age in Years |                 |                 |                 |                 |  0.960  |",
      "|              |-  Mean (SD) | 39.188 (6.047)  | 39.368 (3.515)  | 38.895 (5.646)  | 39.148 (5.041)  |         |",
      "|              |-  Range     | 30.000 - 49.000 | 32.000 - 47.000 | 30.000 - 52.000 | 30.000 - 52.000 |         |",
      "|              |time         |                 |                 |                 |                 |  0.081  |",
      "|              |-  Mean (SD) |  4.750 (1.807)  |  3.105 (2.355)  |  3.895 (2.079)  |  3.870 (2.172)  |         |",
      "|              |-  Range     |  1.000 - 7.000  |  0.000 - 6.000  |  1.000 - 7.000  |  0.000 - 7.000  |         |",
      "|              |Phase        |                 |                 |                 |                 |  0.005  |",
      "|              |-  I         |    5 (31.2%)    |   10 (52.6%)    |    0 (0.0%)     |   15 (27.8%)    |         |",
      "|              |-  II        |    7 (43.8%)    |    6 (31.6%)    |   11 (57.9%)    |   24 (44.4%)    |         |",
      "|              |-  III       |    4 (25.0%)    |    3 (15.8%)    |    8 (42.1%)    |   15 (27.8%)    |         |"
    )
  )
  expect_equal(
    xtfrm(tableby(Group ~ Age + time + Phase, data = mdat, strata = trt)),
    c(
      xtfrm(tableby(Group ~ Age + time + Phase, data = mdat, subset = trt == "A")),
      xtfrm(tableby(Group ~ Age + time + Phase, data = mdat, subset = trt == "B"))
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


test_that("strata levels are maintained", {
  dat <- data.frame(a = c("A", "A", "A", "B", "A", "B"), b = c(1, 1, 1, 2, 2, 2), stringsAsFactors = FALSE)
  expect_identical(
    capture.kable(summary(tableby(~ a, strata = b, data = dat), text = TRUE)),
    c("|b  |     | Overall (N=6) |",
      "|:--|:----|:-------------:|",
      "|1  |a    |               |",
      "|   |-  A |  3 (100.0%)   |",
      "|   |-  B |   0 (0.0%)    |",
      "|2  |a    |               |",
      "|   |-  A |   1 (33.3%)   |",
      "|   |-  B |   2 (66.7%)   |"
    )
  )
})


###########################################################################################################
#### Change totals/p-values
###########################################################################################################

test_that("A two-LHS tableby call--no p-value, no total", {
  expect_identical(
    capture.kable(summary(tableby(list(Group, ethan) ~ Age + Sex, data = mdat, strata = trt), test = FALSE, total = FALSE, text = TRUE)),
    capture.kable(summary(tableby(list(Group, ethan) ~ Age + Sex, data = mdat, strata = trt, test = FALSE, total = FALSE), text = TRUE))
  )
})

###########################################################################################################
#### Other warnings and tests and things...
###########################################################################################################

test_that("Certain functions don't work with multiple LHS or strata", {
  expect_error(padjust(tableby(list(Group, ethan) ~ Age + Sex, data = mdat)), "with strata or multiple")
  expect_error(padjust(tableby(Group ~ Age + Sex, data = mdat, strata = trt)), "with strata or multiple")
  expect_error(sort(tableby(list(Group, ethan) ~ Age + Sex, data = mdat)), "with strata or multiple")
  expect_error(sort(tableby(Group ~ Age + Sex, data = mdat, strata = trt)), "with strata or multiple")
})


test_that("Using cat.simplify", {
  expect_identical(
    capture.kable(summary(tableby(list(Group, ethan) ~ Sex + Age, data = mdat, cat.simplify = TRUE,
                                  numeric.simplify = TRUE, numeric.stats = "meansd"), text = TRUE)),
    c("|             |  High (N=30)   |   Low (N=30)   |   Med (N=30)   |  Total (N=90)  | p value|",
      "|:------------|:--------------:|:--------------:|:--------------:|:--------------:|-------:|",
      "|Sex          |   15 (50.0%)   |   13 (43.3%)   |   16 (53.3%)   |   44 (48.9%)   |   0.733|",
      "|Age in Years | 40.033 (6.217) | 39.633 (3.873) | 39.433 (5.569) | 39.700 (5.258) |   0.906|",
      ""                                                                                            ,
      ""                                                                                            ,
      "|             |  Ethan (N=42)  | Heinzen (N=45) |  Total (N=87)  | p value|"                 ,
      "|:------------|:--------------:|:--------------:|:--------------:|-------:|"                 ,
      "|Sex          |   24 (57.1%)   |   18 (40.0%)   |   42 (48.3%)   |   0.110|"                 ,
      "|Age in Years | 38.857 (5.201) | 40.200 (5.225) | 39.552 (5.227) |   0.233|"
    )
  )
  expect_identical(
    capture.kable(summary(tableby(list(Group, ethan) ~ Sex + Age, strata = trt, data = mdat, cat.simplify = TRUE,
                                  numeric.simplify = TRUE, numeric.stats = "meansd"), text = TRUE)),
    c("|Treatment Arm |             |  High (N=30)   |   Low (N=30)   |   Med (N=30)   |  Total (N=90)  | p value |",
      "|:-------------|:------------|:--------------:|:--------------:|:--------------:|:--------------:|:-------:|",
      "|A             |Sex          |   7 (50.0%)    |   4 (36.4%)    |   6 (54.5%)    |   17 (47.2%)   |  0.670  |",
      "|              |Age in Years | 41.000 (6.493) | 40.091 (4.571) | 40.364 (5.573) | 40.528 (5.537) |  0.918  |",
      "|B             |Sex          |   8 (50.0%)    |   9 (47.4%)    |   10 (52.6%)   |   27 (50.0%)   |  0.949  |",
      "|              |Age in Years | 39.188 (6.047) | 39.368 (3.515) | 38.895 (5.646) | 39.148 (5.041) |  0.960  |",
      ""                                                                                                            ,
      ""                                                                                                            ,
      "|Treatment Arm |             |  Ethan (N=42)  | Heinzen (N=45) |  Total (N=87)  | p value |"                 ,
      "|:-------------|:------------|:--------------:|:--------------:|:--------------:|:-------:|"                 ,
      "|A             |Sex          |   10 (58.8%)   |   5 (31.2%)    |   15 (45.5%)   |  0.112  |"                 ,
      "|              |Age in Years | 37.647 (5.689) | 42.938 (3.924) | 40.212 (5.533) |  0.004  |"                 ,
      "|B             |Sex          |   14 (56.0%)   |   13 (44.8%)   |   27 (50.0%)   |  0.413  |"                 ,
      "|              |Age in Years | 39.680 (4.785) | 38.690 (5.292) | 39.148 (5.041) |  0.477  |"
    )
  )
})


test_that("Reordering variables and subsetting", {
  tmp.tab <- tableby(list(Group, ethan) ~ fe(Sex) + dt + Age, strata = trt, data = mdat)

  expect_identical(
    capture.kable(summary(tmp.tab[c(3,1,2), 2:1], text = TRUE)),
    capture.kable(summary(tableby(list(ethan, Group) ~ Age + fe(Sex) + dt, strata = trt, data = mdat), text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tmp.tab[c(3,1,2)], text = TRUE)),
    capture.kable(summary(tmp.tab[c("Age", "Sex", "dt")], text = TRUE))
  )
  expect_identical(
    capture.kable(summary(tmp.tab[, 2:1], text = TRUE)),
    capture.kable(summary(tmp.tab[, c("ethan", "Group")], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tmp.tab[1:2], text = TRUE)),
    capture.kable(summary(tmp.tab[c(TRUE, TRUE, FALSE)], text = TRUE))
  )
  expect_identical(
    capture.kable(summary(tmp.tab[, 2], text = TRUE)),
    capture.kable(summary(tmp.tab[, c(FALSE, TRUE)], text = TRUE))
  )

  expect_warning(tmp.tab[1:4], "Some indices not found")
  expect_warning(tmp.tab[, 1:3], "Some indices not found")
  expect_error(tmp.tab[TRUE], "Logical vector")
  expect_error(tmp.tab[, TRUE], "Logical vector")
})

test_that("Merging tableby objects", {
  tb1 <- tableby(list(Group, ethan) ~ Sex + Phase, strata = trt, data = mdat)
  tb2 <- tableby(list(Group.fac, status) ~ Age, strata = trt, data = mdat)
  tb3 <- tableby(list(Group, Group.fac, ethan) ~ Age + dt, strata = trt, data = mdat)
  tb4 <- tableby(list(Group, ethan) ~ Sex + Phase + Age + dt, strata = trt, data = mdat)

  expect_error(merge(tb1, tb2), "No terms in common")
  expect_identical(
    capture.kable(summary(merge(tb1, tb2, all = TRUE))),
    c(capture.kable(summary(tb1)), "", "", capture.kable(summary(tb2)))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, tb3), text = TRUE)),
    capture.kable(summary(tb4, text = TRUE))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, tb3, all.x = TRUE), text = TRUE)),
    capture.kable(summary(tb4, text = TRUE))
  )
  expect_identical(
    capture.kable(summary(merge(tb1, tb3, all = TRUE), text = TRUE)),
    c(
      capture.kable(summary(tb4, text = TRUE)), "", "",
      capture.kable(summary(tableby(Group.fac ~ Age + dt, data = mdat, strata = trt), text = TRUE))
    )
  )
})

test_that("Changing labels", {
  tb <- tableby(list(Group, ethan) ~ Sex + Age, strata = trt, data = mdat)
  expect_warning(labels(tb) <- c(hi = "hi", Sex = "Sex label", Age = "Age at event", trt = "Trt Arm", ethan = "EthanH"), NA)
  expect_identical(
    capture.kable(summary(tb, text = TRUE)),
    c("|Trt Arm |             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value |",
      "|:-------|:------------|:---------------:|:---------------:|:---------------:|:---------------:|:-------:|",
      "|A       |Sex label    |                 |                 |                 |                 |  0.670  |",
      "|        |-  Female    |    7 (50.0%)    |    7 (63.6%)    |    5 (45.5%)    |   19 (52.8%)    |         |",
      "|        |-  Male      |    7 (50.0%)    |    4 (36.4%)    |    6 (54.5%)    |   17 (47.2%)    |         |",
      "|        |Age at event |                 |                 |                 |                 |  0.918  |",
      "|        |-  Mean (SD) | 41.000 (6.493)  | 40.091 (4.571)  | 40.364 (5.573)  | 40.528 (5.537)  |         |",
      "|        |-  Range     | 29.000 - 53.000 | 33.000 - 48.000 | 30.000 - 49.000 | 29.000 - 53.000 |         |",
      "|B       |Sex label    |                 |                 |                 |                 |  0.949  |",
      "|        |-  Female    |    8 (50.0%)    |   10 (52.6%)    |    9 (47.4%)    |   27 (50.0%)    |         |",
      "|        |-  Male      |    8 (50.0%)    |    9 (47.4%)    |   10 (52.6%)    |   27 (50.0%)    |         |",
      "|        |Age at event |                 |                 |                 |                 |  0.960  |",
      "|        |-  Mean (SD) | 39.188 (6.047)  | 39.368 (3.515)  | 38.895 (5.646)  | 39.148 (5.041)  |         |",
      "|        |-  Range     | 30.000 - 49.000 | 32.000 - 47.000 | 30.000 - 52.000 | 30.000 - 52.000 |         |",
      ""                                                                                                          ,
      ""                                                                                                                ,
      "|Trt Arm |             |  Ethan (N=42)   | Heinzen (N=45)  |  Total (N=87)   | p value |"                  ,
      "|:-------|:------------|:---------------:|:---------------:|:---------------:|:-------:|"                  ,
      "|A       |Sex label    |                 |                 |                 |  0.112  |"                  ,
      "|        |-  Female    |    7 (41.2%)    |   11 (68.8%)    |   18 (54.5%)    |         |"                  ,
      "|        |-  Male      |   10 (58.8%)    |    5 (31.2%)    |   15 (45.5%)    |         |"                  ,
      "|        |Age at event |                 |                 |                 |  0.004  |"                  ,
      "|        |-  Mean (SD) | 37.647 (5.689)  | 42.938 (3.924)  | 40.212 (5.533)  |         |"                  ,
      "|        |-  Range     | 29.000 - 53.000 | 36.000 - 49.000 | 29.000 - 53.000 |         |"                  ,
      "|B       |Sex label    |                 |                 |                 |  0.413  |"                  ,
      "|        |-  Female    |   11 (44.0%)    |   16 (55.2%)    |   27 (50.0%)    |         |"                  ,
      "|        |-  Male      |   14 (56.0%)    |   13 (44.8%)    |   27 (50.0%)    |         |"                  ,
      "|        |Age at event |                 |                 |                 |  0.477  |"                  ,
      "|        |-  Mean (SD) | 39.680 (4.785)  | 38.690 (5.292)  | 39.148 (5.041)  |         |"                  ,
      "|        |-  Range     | 30.000 - 48.000 | 30.000 - 52.000 | 30.000 - 52.000 |         |"
    )
  )
  labels(tb) <- NULL
  expect_identical(
    capture.kable(summary(tb, text = TRUE)),
    c("|trt |             |   High (N=30)   |   Low (N=30)    |   Med (N=30)    |  Total (N=90)   | p value |",
      "|:---|:------------|:---------------:|:---------------:|:---------------:|:---------------:|:-------:|",
      "|A   |Sex          |                 |                 |                 |                 |  0.670  |",
      "|    |-  Female    |    7 (50.0%)    |    7 (63.6%)    |    5 (45.5%)    |   19 (52.8%)    |         |",
      "|    |-  Male      |    7 (50.0%)    |    4 (36.4%)    |    6 (54.5%)    |   17 (47.2%)    |         |",
      "|    |Age          |                 |                 |                 |                 |  0.918  |",
      "|    |-  Mean (SD) | 41.000 (6.493)  | 40.091 (4.571)  | 40.364 (5.573)  | 40.528 (5.537)  |         |",
      "|    |-  Range     | 29.000 - 53.000 | 33.000 - 48.000 | 30.000 - 49.000 | 29.000 - 53.000 |         |",
      "|B   |Sex          |                 |                 |                 |                 |  0.949  |",
      "|    |-  Female    |    8 (50.0%)    |   10 (52.6%)    |    9 (47.4%)    |   27 (50.0%)    |         |",
      "|    |-  Male      |    8 (50.0%)    |    9 (47.4%)    |   10 (52.6%)    |   27 (50.0%)    |         |",
      "|    |Age          |                 |                 |                 |                 |  0.960  |",
      "|    |-  Mean (SD) | 39.188 (6.047)  | 39.368 (3.515)  | 38.895 (5.646)  | 39.148 (5.041)  |         |",
      "|    |-  Range     | 30.000 - 49.000 | 32.000 - 47.000 | 30.000 - 52.000 | 30.000 - 52.000 |         |",
      ""                                                                                                      ,
      ""                                                                                                      ,
      "|trt |             |  Ethan (N=42)   | Heinzen (N=45)  |  Total (N=87)   | p value |"                  ,
      "|:---|:------------|:---------------:|:---------------:|:---------------:|:-------:|"                  ,
      "|A   |Sex          |                 |                 |                 |  0.112  |"                  ,
      "|    |-  Female    |    7 (41.2%)    |   11 (68.8%)    |   18 (54.5%)    |         |"                  ,
      "|    |-  Male      |   10 (58.8%)    |    5 (31.2%)    |   15 (45.5%)    |         |"                  ,
      "|    |Age          |                 |                 |                 |  0.004  |"                  ,
      "|    |-  Mean (SD) | 37.647 (5.689)  | 42.938 (3.924)  | 40.212 (5.533)  |         |"                  ,
      "|    |-  Range     | 29.000 - 53.000 | 36.000 - 49.000 | 29.000 - 53.000 |         |"                  ,
      "|B   |Sex          |                 |                 |                 |  0.413  |"                  ,
      "|    |-  Female    |   11 (44.0%)    |   16 (55.2%)    |   27 (50.0%)    |         |"                  ,
      "|    |-  Male      |   14 (56.0%)    |   13 (44.8%)    |   27 (50.0%)    |         |"                  ,
      "|    |Age          |                 |                 |                 |  0.477  |"                  ,
      "|    |-  Mean (SD) | 39.680 (4.785)  | 38.690 (5.292)  | 39.148 (5.041)  |         |"                  ,
      "|    |-  Range     | 30.000 - 48.000 | 30.000 - 52.000 | 30.000 - 52.000 |         |"
    )
  )
})

###########################################################################################################
#### Reported bugs for tableby
###########################################################################################################

test_that("02/23/2018: wrapping long labels (#59)", {
  labs <- list(
    Group = "This is a really long label for the Group variable",
    time = "Another really long label. Can you believe how long this is",
    dt = "ThisLabelHasNoSpacesSoLetsSeeHowItBehaves",
    trt = NULL
  )
  expect_identical(
    capture.kable(print(summary(tableby(Sex ~ Group + time + dt, strata = trt, data = set_labels(mdat, labs)), text = TRUE), width = 30)),
    c("|trt |                               |      Female (N=46)      |       Male (N=44)       |      Total (N=90)       | p value |",
      "|:---|:------------------------------|:-----------------------:|:-----------------------:|:-----------------------:|:-------:|",
      "|A   |This is a really long label    |                         |                         |                         |  0.670  |",
      "|    |for the Group variable         |                         |                         |                         |         |",
      "|    |-  High                        |        7 (36.8%)        |        7 (41.2%)        |       14 (38.9%)        |         |",
      "|    |-  Low                         |        7 (36.8%)        |        4 (23.5%)        |       11 (30.6%)        |         |",
      "|    |-  Med                         |        5 (26.3%)        |        6 (35.3%)        |       11 (30.6%)        |         |",
      "|    |Another really long label.     |                         |                         |                         |  0.831  |",
      "|    |Can you believe how long this  |                         |                         |                         |         |",
      "|    |is                             |                         |                         |                         |         |",
      "|    |-  Mean (SD)                   |      3.895 (1.560)      |      3.765 (2.047)      |      3.833 (1.781)      |         |",
      "|    |-  Range                       |      1.000 - 6.000      |      0.000 - 7.000      |      0.000 - 7.000      |         |",
      "|    |ThisLabelHasNoSpacesSoLetsSeeH |                         |                         |                         |  0.669  |",
      "|    |owItBehaves                    |                         |                         |                         |         |",
      "|    |-  Median                      |       1949-11-01        |       1950-02-06        |       1949-12-19        |         |",
      "|    |-  Range                       | 1939-04-01 - 1959-09-06 | 1939-04-03 - 1968-05-14 | 1939-04-01 - 1968-05-14 |         |",
      "|B   |This is a really long label    |                         |                         |                         |  0.949  |",
      "|    |for the Group variable         |                         |                         |                         |         |",
      "|    |-  High                        |        8 (29.6%)        |        8 (29.6%)        |       16 (29.6%)        |         |",
      "|    |-  Low                         |       10 (37.0%)        |        9 (33.3%)        |       19 (35.2%)        |         |",
      "|    |-  Med                         |        9 (33.3%)        |       10 (37.0%)        |       19 (35.2%)        |         |",
      "|    |Another really long label.     |                         |                         |                         |  0.118  |",
      "|    |Can you believe how long this  |                         |                         |                         |         |",
      "|    |is                             |                         |                         |                         |         |",
      "|    |-  Mean (SD)                   |      3.407 (2.153)      |      4.333 (2.130)      |      3.870 (2.172)      |         |",
      "|    |-  Range                       |      0.000 - 7.000      |      0.000 - 7.000      |      0.000 - 7.000      |         |",
      "|    |ThisLabelHasNoSpacesSoLetsSeeH |                         |                         |                         |  0.102  |",
      "|    |owItBehaves                    |                         |                         |                         |         |",
      "|    |-  Median                      |       1948-05-31        |       1951-03-31        |       1949-09-09        |         |",
      "|    |-  Range                       | 1935-08-15 - 1957-08-15 | 1937-02-08 - 1958-07-30 | 1935-08-15 - 1958-07-30 |         |"
    )
  )
})


