## Tests for tableby


## Jason's code for sourcing .R files
## fl <- list.files(path="../../R", pattern="*.R", full.names=TRUE, include.dirs=TRUE)
## for(file in fl) source(file)

context("Testing the tableby output")

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

###########################################################################################################
#### Basic two-sided tableby
###########################################################################################################

test_that("A basic two-sided tableby call--no labels, no missings", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Sex + time + dt, data = mdat), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Sex                                                                                    0.733",
      "   Female         15 (50%)       17 (56.7%)     14 (46.7%)     46 (51.1%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)    "               ,
      "time                                                                                   0.025",
      "   Mean (SD)      4.57 (1.81)    3.17 (2.04)    3.83 (2)       3.86 (2.01)   "               ,
      "   Q1, Q3         3.25, 6        1.25, 5        2, 5           2, 6          "               ,
      "   Range          0 - 7          0 - 6          1 - 7          0 - 7         "               ,
      "dt                                                                                     0.391",
      "   median         1950-01-07     1951-06-13     1948-09-13     1949-10-07    "               ,
      "   Range          1935-08-15 -   1937-02-08 -   1939-04-01 -   1935-08-15 -  "               ,
      "                  1968-05-14     1959-09-06     1958-07-30     1968-05-14    "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})

test_that("A basic two-sided tableby call--labels, no missings", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + trt + Phase, data = mdat), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Age in Years                                                                           0.906",
      "   Mean (SD)      40 (6.22)      39.6 (3.87)    39.4 (5.57)    39.7 (5.26)   "               ,
      "   Q1, Q3         36, 44.5       37.2, 41.8     35.2, 44       36, 43        "               ,
      "   Range          29 - 53        32 - 48        30 - 52        29 - 53       "               ,
      "Treatment Arm                                                                          0.659",
      "   A              14 (46.7%)     11 (36.7%)     11 (36.7%)     36 (40%)      "               ,
      "   B              16 (53.3%)     19 (63.3%)     19 (63.3%)     54 (60%)      "               ,
      "Phase                                                                                  0.008",
      "   I              11 (36.7%)     12 (40%)       0 (0%)         23 (25.6%)    ",
      "   II             10 (33.3%)     12 (40%)       19 (63.3%)     41 (45.6%)    ",
      "   III            9 (30%)        6 (20%)        11 (36.7%)     26 (28.9%)    ",
      "--------------------------------------------------------------------------------------------"
    )
  )
})

test_that("A basic two-sided tableby call--no labels, some missings", {
  expect_identical(
    capture.output(summary(tableby(Group ~ ethan, data = mdat), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "ethan                                                                                  0.178",
      "   N-Miss         3              0              0              3             "               ,
      "   Ethan          17 (63%)       13 (43.3%)     12 (40%)       42 (48.3%)    "               ,
      "   Heinzen        10 (37%)       17 (56.7%)     18 (60%)       45 (51.7%)    "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})

###########################################################################################################
#### Basic one-sided tableby
###########################################################################################################

test_that("A basic one-sided tableby call--no labels, no missings", {
  expect_identical(
    capture.output(summary(tableby(~ Sex + time + dt, data = mdat), text = TRUE)),
    c(""                                    ,
      "------------------------------------",
      "                     Overall (N=90) ",
      "------------------- ----------------",
      "Sex                "                 ,
      "   Female           46 (51.1%)      ",
      "   Male             44 (48.9%)      ",
      "time               ",
      "   Mean (SD)        3.86 (2.01)     ",
      "   Q1, Q3           2, 6            ",
      "   Range            0 - 7           ",
      "dt                 "                 ,
      "   median           1949-10-07      ",
      "   Range            1935-08-15 -    ",
      "                    1968-05-14      ",
      "------------------------------------"
    )
  )
})

test_that("A basic one-sided tableby call--labels, no missings", {
  expect_identical(
    capture.output(summary(tableby(~ Age + trt, data = mdat), text = TRUE)),
    c(""                                    ,
      "------------------------------------",
      "                     Overall (N=90) ",
      "------------------- ----------------",
      "Age in Years       "                 ,
      "   Mean (SD)        39.7 (5.26)     ",
      "   Q1, Q3           36, 43          ",
      "   Range            29 - 53         ",
      "Treatment Arm      "                 ,
      "   A                36 (40%)        ",
      "   B                54 (60%)        ",
      "------------------------------------"
    )
  )
})

test_that("A basic one-sided tableby call--no labels, some missings (Sarah Jenkins's Error)", {
  expect_identical(
    capture.output(summary(tableby(~ ethan, data = mdat), text = TRUE)),
    c(""                                    ,
      "------------------------------------",
      "                     Overall (N=90) ",
      "------------------- ----------------",
      "ethan              "                 ,
      "   N-Miss           3               ",
      "   Ethan            42 (48.3%)      ",
      "   Heinzen          45 (51.7%)      ",
      "------------------------------------"
    )
  )
})

###########################################################################################################
#### Change totals/p-values
###########################################################################################################

test_that("A basic two-sided tableby call--no p-value, no total", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + Sex, data = mdat, test = FALSE, total = FALSE), text = TRUE)),
    c(""                                                          ,
      "----------------------------------------------------------",
      "                  High (N=30)   Low (N=30)    Med (N=30)  ",
      "---------------- ------------- ------------- -------------",
      "Age in Years    "                                          ,
      "   Mean (SD)     40 (6.22)     39.6 (3.87)   39.4 (5.57)  ",
      "   Q1, Q3        36, 44.5      37.2, 41.8    35.2, 44     ",
      "   Range         29 - 53       32 - 48       30 - 52      ",
      "Sex             "                                          ,
      "   Female        15 (50%)      17 (56.7%)    14 (46.7%)   ",
      "   Male          15 (50%)      13 (43.3%)    16 (53.3%)   ",
      "----------------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + Sex, data = mdat), test = FALSE, total = FALSE, text = TRUE)),
    c(""                                                          ,
      "----------------------------------------------------------",
      "                  High (N=30)   Low (N=30)    Med (N=30)  ",
      "---------------- ------------- ------------- -------------",
      "Age in Years    "                                          ,
      "   Mean (SD)     40 (6.22)     39.6 (3.87)   39.4 (5.57)  ",
      "   Q1, Q3        36, 44.5      37.2, 41.8    35.2, 44     ",
      "   Range         29 - 53       32 - 48       30 - 52      ",
      "Sex             "                                          ,
      "   Female        15 (50%)      17 (56.7%)    14 (46.7%)   ",
      "   Male          15 (50%)      13 (43.3%)    16 (53.3%)   ",
      "----------------------------------------------------------"
    )
  )
})

test_that("A basic two-sided tableby call--p-value, no total", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + Sex, data = mdat, total = FALSE), text = TRUE)),
    c(""                                                                        ,
      "------------------------------------------------------------------------",
      "                  High (N=30)   Low (N=30)    Med (N=30)    p value     ",
      "---------------- ------------- ------------- ------------- -------------",
      "Age in Years                                                       0.906",
      "   Mean (SD)     40 (6.22)     39.6 (3.87)   39.4 (5.57)  "              ,
      "   Q1, Q3        36, 44.5      37.2, 41.8    35.2, 44     "              ,
      "   Range         29 - 53       32 - 48       30 - 52      "              ,
      "Sex                                                                0.733",
      "   Female        15 (50%)      17 (56.7%)    14 (46.7%)   "              ,
      "   Male          15 (50%)      13 (43.3%)    16 (53.3%)   "              ,
      "------------------------------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + Sex, data = mdat), total = FALSE, text = TRUE)),
    c(""                                                                        ,
      "------------------------------------------------------------------------",
      "                  High (N=30)   Low (N=30)    Med (N=30)    p value     ",
      "---------------- ------------- ------------- ------------- -------------",
      "Age in Years                                                       0.906",
      "   Mean (SD)     40 (6.22)     39.6 (3.87)   39.4 (5.57)  "              ,
      "   Q1, Q3        36, 44.5      37.2, 41.8    35.2, 44     "              ,
      "   Range         29 - 53       32 - 48       30 - 52      "              ,
      "Sex                                                                0.733",
      "   Female        15 (50%)      17 (56.7%)    14 (46.7%)   "              ,
      "   Male          15 (50%)      13 (43.3%)    16 (53.3%)   "              ,
      "------------------------------------------------------------------------"
    )
  )
})

###########################################################################################################
#### markdown output
###########################################################################################################

test_that("A basic two-sided tableby markdown output", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Age + Sex + ethan + dt, data = mdat, total = FALSE))),
    c(""                                                                        ,
      "------------------------------------------------------------------------",
      "                  High (N=30)   Low (N=30)    Med (N=30)    p value     ",
      "---------------- ------------- ------------- ------------- -------------",
      "**Age in Years**                                                   0.906",
      ""                                                                        ,
      "&nbsp;&nbsp;     40 (6.22)     39.6 (3.87)   39.4 (5.57)  "              ,
      "&nbsp;                                                    "              ,
      "Mean (SD)                                                 "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     36, 44.5      37.2, 41.8    35.2, 44     "              ,
      "&nbsp;                                                    "              ,
      "Q1, Q3                                                    "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     29 - 53       32 - 48       30 - 52      "              ,
      "&nbsp;                                                    "              ,
      "Range                                                     "              ,
      ""                                                                        ,
      "**Sex**                                                            0.733",
      ""                                                                        ,
      "&nbsp;&nbsp;     15 (50%)      17 (56.7%)    14 (46.7%)   "              ,
      "&nbsp;                                                    "              ,
      "Female                                                    "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     15 (50%)      13 (43.3%)    16 (53.3%)   "              ,
      "&nbsp;                                                    "              ,
      "Male                                                      "              ,
      ""                                                                        ,
      "**ethan**                                                          0.178",
      ""                                                                        ,
      "&nbsp;&nbsp;     3             0             0            "              ,
      "&nbsp;                                                    "              ,
      "N-Miss                                                    "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     17 (63%)      13 (43.3%)    12 (40%)     "              ,
      "&nbsp;                                                    "              ,
      "Ethan                                                     "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     10 (37%)      17 (56.7%)    18 (60%)     "              ,
      "&nbsp;                                                    "              ,
      "Heinzen                                                   "              ,
      ""                                                                        ,
      "**dt**                                                             0.391",
      ""                                                                        ,
      "&nbsp;&nbsp;     1950-01-07    1951-06-13    1948-09-13   "              ,
      "&nbsp;                                                    "              ,
      "median                                                    "              ,
      ""                                                                        ,
      "&nbsp;&nbsp;     1935-08-15 -  1937-02-08 -  1939-04-01 - "              ,
      "&nbsp;           1968-05-14    1959-09-06    1958-07-30   "              ,
      "Range                                                     "              ,
      ""                                                                        ,
      "------------------------------------------------------------------------"
    )
  )
})


###########################################################################################################
#### Other warnings and tests and things...
###########################################################################################################

test_that("A warning occurs using one-sided formula and na.tableby", {
  expect_warning(tableby(~ ethan, data = mdat, na.action = na.tableby))
})

test_that("The by-variable droplevels is working correctly", {
  expect_identical(
    capture.output(summary(tableby(Group.fac ~ Sex + time + dt, data = mdat[mdat$Group.fac %in% c("High", "Low"), ]), text = TRUE)),
    c(""                                                                             ,
      "-----------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Total (N=60)   p value      ",
      "----------------- -------------- -------------- -------------- --------------",
      "Sex                                                                     0.796",
      "   Female         15 (50%)       17 (56.7%)     32 (53.3%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     28 (46.7%)    "               ,
      "time                                                                    0.007",
      "   Mean (SD)      4.57 (1.81)    3.17 (2.04)    3.87 (2.04)   "               ,
      "   Q1, Q3         3.25, 6        1.25, 5        2, 6          "               ,
      "   Range          0 - 7          0 - 6          0 - 7         "               ,
      "dt                                                                      0.574",
      "   median         1950-01-07     1951-06-13     1950-07-02    "               ,
      "   Range          1935-08-15 -   1937-02-08 -   1935-08-15 -  "               ,
      "                  1968-05-14     1959-09-06     1968-05-14    "               ,
      "-----------------------------------------------------------------------------"
    )
  )
})

test_that("Using cat.simplify", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Sex + trt, data = mdat, cat.simplify = TRUE), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Sex               15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)              0.733",
      "Treatment Arm     16 (53.3%)     19 (63.3%)     19 (63.3%)     54 (60%)                0.659",
      "--------------------------------------------------------------------------------------------"
    )
  )
})


test_that("Reordering variables", {
  expect_identical(
    capture.output(summary(tableby(Group ~ Sex + dt + Age, data = mdat)[c(3,1,2)], text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Age in Years                                                                           0.906",
      "   Mean (SD)      40 (6.22)      39.6 (3.87)    39.4 (5.57)    39.7 (5.26)   "               ,
      "   Q1, Q3         36, 44.5       37.2, 41.8     35.2, 44       36, 43        "               ,
      "   Range          29 - 53        32 - 48        30 - 52        29 - 53       "               ,
      "Sex                                                                                    0.733",
      "   Female         15 (50%)       17 (56.7%)     14 (46.7%)     46 (51.1%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)    "               ,
      "dt                                                                                     0.391",
      "   median         1950-01-07     1951-06-13     1948-09-13     1949-10-07    "               ,
      "   Range          1935-08-15 -   1937-02-08 -   1939-04-01 -   1935-08-15 -  "               ,
      "                  1968-05-14     1959-09-06     1958-07-30     1968-05-14    "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})


test_that("Merging tableby objects", {
  expect_error(merge(tableby(Group ~ Sex, data = mdat), tableby(Group.fac ~ Age, data = mdat)))
  expect_identical(
    capture.output(summary(merge(tableby(Group ~ Sex, data = mdat), tableby(Group ~ Age, data = mdat)), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Sex                                                                                    0.733",
      "   Female         15 (50%)       17 (56.7%)     14 (46.7%)     46 (51.1%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)    "               ,
      "Age in Years                                                                           0.906",
      "   Mean (SD)      40 (6.22)      39.6 (3.87)    39.4 (5.57)    39.7 (5.26)   "               ,
      "   Q1, Q3         36, 44.5       37.2, 41.8     35.2, 44       36, 43        "               ,
      "   Range          29 - 53        32 - 48        30 - 52        29 - 53       "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})


test_that("Changing tests", {
  expect_identical(
    capture.output(summary(tableby(Group ~ fe(Sex) + kwt(Age), data = mdat), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Sex                                                                                    0.806",
      "   Female         15 (50%)       17 (56.7%)     14 (46.7%)     46 (51.1%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)    "               ,
      "Age in Years                                                                           0.869",
      "   Mean (SD)      40 (6.22)      39.6 (3.87)    39.4 (5.57)    39.7 (5.26)   "               ,
      "   Q1, Q3         36, 44.5       37.2, 41.8     35.2, 44       36, 43        "               ,
      "   Range          29 - 53        32 - 48        30 - 52        29 - 53       "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )

  expect_identical(
    capture.output(summary(tableby(Group ~ Sex + Age, data = mdat, numeric.test = "kwt", cat.test = "fe"), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Sex                                                                                    0.806",
      "   Female         15 (50%)       17 (56.7%)     14 (46.7%)     46 (51.1%)    "               ,
      "   Male           15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)    "               ,
      "Age in Years                                                                           0.869",
      "   Mean (SD)      40 (6.22)      39.6 (3.87)    39.4 (5.57)    39.7 (5.26)   "               ,
      "   Q1, Q3         36, 44.5       37.2, 41.8     35.2, 44       36, 43        "               ,
      "   Range          29 - 53        32 - 48        30 - 52        29 - 53       "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})


#tmp <- tableby(Group ~ Sex + time + dt, data = mdat, subset=Group != "High")
set.seed(1000)
old <- options(width = 150)
test_that("05/25/2017: simulate.p.value option for chisq.test", {
  expect_identical(
    capture.output(tests(tableby(Group ~ Sex + time + dt, data = mdat,  subset=Group != "High",simulate.p.value=TRUE))),
    c("     Variable   p.value                                                                           Method"  ,
      "Sex       Sex 0.6116942 Pearson's Chi-squared test with simulated p-value\\n\\t (based on 2000 replicates)",
      "time     time 0.2059543                                                               Linear Model ANOVA",
      "dt         dt 0.1714441                                                     Kruskal-Wallis rank sum test"
    )
  )
})

test_that("05/25/2017: chisq.correct=FALSE option for chisq.test", {
  expect_identical(
    capture.output(tests(tableby(Group ~ Sex + time + dt, data = mdat, subset=Group != "High", chisq.correct=FALSE))),
    c("     Variable   p.value                       Method",
      "Sex       Sex 0.4383235   Pearson's Chi-squared test",
      "time     time 0.2059543           Linear Model ANOVA",
      "dt         dt 0.1714441 Kruskal-Wallis rank sum test"
    )
  )
})


set.seed(1000)
test_that("05/25/2017: simulate.p.value=TRUE option for fisher.test", {
  expect_identical(
    capture.output(tests(tableby(Group ~ fe(Sex) + time + dt, data = mdat,simulate.p.value=TRUE))),
    c("     Variable    p.value                                                                                   Method",
      "Sex       Sex 0.80009995 Fisher's Exact Test for Count Data with simulated p-value\\n\\t (based on 2000 replicates)",
      "time     time 0.02480103                                                                       Linear Model ANOVA",
      "dt         dt 0.39126924                                                             Kruskal-Wallis rank sum test"
    )
  )
})
options(old)

###########################################################################################################
#### Reported bugs for tableby
###########################################################################################################

test_that("02/07/2017: Ryan Lennon's R Markdown spacing problem", {
  expect_identical(capture.output(summary(tableby(Group ~ Sex + time + dt, data = mdat), text = TRUE))[1], "")
})

dat <- data.frame(x = c("A", "A", "A", rep(c("B", "C"), each = 7)),
                  y = c("cough", "pneumonia", NA,
                        "chest pain", "chest pain", "chest pain", "cough", "cough", "pneumonia", "cough",
                        "cough", "pneumonia", "chest pain", "chest pain", "pneumonia", NA, NA))
dat$y <- factor(dat$y)

test_that("02/07/2017: Jason Sinnwell's countpct problem", {
  expect_identical(
    capture.output(summary(tableby(x ~ fe(y), data = dat), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   A (N=3)        B (N=7)        C (N=7)        Total (N=17)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "y                                                                                      0.750",
      "   N-Miss         1              0              2              3             "               ,
      "   chest pain     0 (0%)         3 (42.9%)      2 (40%)        5 (35.7%)     "               ,
      "   cough          1 (50%)        3 (42.9%)      1 (20%)        5 (35.7%)     "               ,
      "   pneumonia      1 (50%)        1 (14.3%)      2 (40%)        4 (28.6%)     "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})

test_that("02/07/2017: Jason Sinnwell's chisq problem", {
  expect_identical(
    capture.output(summary(tableby(x ~ y, data = dat[dat$y == "cough",]), text = TRUE)),
    c(""                                                                                      ,
      "--------------------------------------------------------------------------------------",
      "                  A (N=1)       B (N=3)       C (N=1)       Total (N=5)   p value     ",
      "---------------- ------------- ------------- ------------- ------------- -------------",
      "y                                                                                1.000",
      "   chest pain    0 (0%)        0 (0%)        0 (0%)        0 (0%)       "              ,
      "   cough         1 (100%)      3 (100%)      1 (100%)      5 (100%)     "              ,
      "   pneumonia     0 (0%)        0 (0%)        0 (0%)        0 (0%)       "              ,
      "--------------------------------------------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(summary(tableby(x ~ as.character(y), data = dat[dat$y == "cough",]), text = TRUE)),
    c(""                                                                                      ,
      "--------------------------------------------------------------------------------------",
      "                  A (N=1)       B (N=3)       C (N=1)       Total (N=5)   p value     ",
      "---------------- ------------- ------------- ------------- ------------- -------------",
      "as.character(y)                                                                  1.000",
      "   cough         1 (100%)      3 (100%)      1 (100%)      5 (100%)     "              ,
      "--------------------------------------------------------------------------------------"
    )
  )
})
rm(dat)

test_that("03/17/2017: Beth's medianq1q3 label", {
  expect_identical(
    capture.output(summary(tableby(Group ~ ht_in + time, data = mdat,
                                   control = tableby.control(numeric.stats = c("Nmiss2", "medianq1q3"))), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Height in Inches                                                                       0.785",
      "   N-Miss         0              0              0              0             "               ,
      "   Median (Q1,    64.5 (62, 68)  64 (61, 68.8)  64.5 (62, 68)  64 (62, 68)   "               ,
      "   Q3)                                                                       "               ,
      "time                                                                                   0.025",
      "   N-Miss         0              0              0              0             "               ,
      "   Median (Q1,    5 (3.25, 6)    3 (1.25, 5)    4 (2, 5)       4 (2, 6)      "               ,
      "   Q3)                                                                       "               ,
      "--------------------------------------------------------------------------------------------"
    )
  )
})


test_that("04/12/2017: Katherine King's cat.simplify vs tableby.control", {
  expect_identical(
    capture.output(summary(tableby(Group ~ trt + Sex, data = mdat, control = tableby.control(), cat.simplify = TRUE), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                   High (N=30)    Low (N=30)     Med (N=30)     Total (N=90)   p value      ",
      "----------------- -------------- -------------- -------------- -------------- --------------",
      "Treatment Arm     16 (53.3%)     19 (63.3%)     19 (63.3%)     54 (60%)                0.659",
      "Sex               15 (50%)       13 (43.3%)     16 (53.3%)     44 (48.9%)              0.733",
      "--------------------------------------------------------------------------------------------"
    )
  )
})

data(mockstudy)
temp <- mockstudy[1:5,]
test_that("05/24/2017: Katherine King's count vs countpct", {
  expect_identical(
    capture.output(summary(tableby(arm ~ sex + age, data=temp,cat.stats="count", test = FALSE), text = TRUE)),
    c(""                                                                                            ,
      "--------------------------------------------------------------------------------------------",
      "                      A: IFL (N=2)      F: FOLFOX (N=2)   G: IROX (N=1)     Total (N=5)     ",
      "-------------------- ----------------- ----------------- ----------------- -----------------",
      "sex                 "                                                                        ,
      "   Male              0                 1                 0                 1                ",
      "   Female            2                 1                 1                 4                ",
      "age                 "                                                                        ,
      "   Mean (SD)         62 (17)           68 (1.41)         71 (NaN)          66.2 (9.42)      ",
      "   Q1, Q3            56, 68            67.5, 68.5        71, 71            67, 71           ",
      "   Range             50 - 74           67 - 69           71 - 71           50 - 74          ",
      "--------------------------------------------------------------------------------------------"
    )
  )
})


df <- data.frame(x = c("a ", "a ", "b", "b ", "c", "c"), y = c("A", "A", "A", "B", "B", "B"), stringsAsFactors = FALSE)
##table(df$x, df$y)
test_that("05/24/2017: Missy Larson and Ethan Heinzen trailing spaces on char x variable", {
  expect_identical(
    capture.output(summary(tableby(y ~ x, data = df, test = FALSE), text = TRUE)),
    c(""                                                         ,
      "----------------------------------------------------------",
      "                  A (N=3)       B (N=3)       Total (N=6) ",
      "---------------- ------------- ------------- -------------",
      "x               "                                          ,
      "   a             2 (66.7%)     0 (0%)        2 (33.3%)    ",
      "   b             1 (33.3%)     0 (0%)        1 (16.7%)    ",
      "   b             0 (0%)        1 (33.3%)     1 (16.7%)    ",
      "   c             0 (0%)        2 (66.7%)     2 (33.3%)    ",
      "----------------------------------------------------------"
    )
  )
})


test_that("08/02/2017: Chi-square warnings are suppressed", {
  expect_warning(tableby(arm ~ sex, data = mockstudy, subset = 1:5), NA)
})
