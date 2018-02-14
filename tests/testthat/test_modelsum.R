## Tests for modelsum


context("Testing the modelsum output")

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
                   weights = c(20, 1.5, rep(1, nsubj - 2)),
                   stringsAsFactors = FALSE)
mdat$Group.fac <- factor(mdat$Group)
attr(mdat$ht_in, "label") <- "Height in Inches"
attr(mdat$trt, "label") <- "Treatment Arm"
attr(mdat$Age, "label") <- "Age in Years"

###########################################################################################################
#### Basic modelsum call
###########################################################################################################

test_that("A basic modelsum call--no labels, no missings", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, data = mdat), text = TRUE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |39.826   |0.779     |< 0.001 |-0.011        |",
      "|Sex Male    |-0.258   |1.115     |0.818   |              |",
      "|(Intercept) |41.130   |1.197     |< 0.001 |0.009         |",
      "|time        |-0.371   |0.275     |0.182   |              |"
    )
  )
})

test_that("A basic modelsum tableby call--labels, no missings", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + trt, data = mdat), text = TRUE)),
    c("|                |estimate |std.error |p.value |adj.r.squared |",
      "|:---------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)     |39.826   |0.779     |< 0.001 |-0.011        |",
      "|Sex Male        |-0.258   |1.115     |0.818   |              |",
      "|(Intercept)     |40.528   |0.874     |< 0.001 |0.006         |",
      "|Treatment Arm B |-1.380   |1.128     |0.225   |              |"
    )
  )
})

test_that("A basic modelsum call--adding adjustment", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE)),
    c("|                |estimate |std.error |p.value |adj.r.squared |",
      "|:---------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)     |40.632   |1.024     |< 0.001 |-0.005        |",
      "|Sex Male        |-0.221   |1.112     |0.843   |              |",
      "|Treatment Arm B |-1.373   |1.135     |0.229   |              |",
      "|(Intercept)     |41.938   |1.366     |< 0.001 |0.014         |",
      "|time            |-0.368   |0.275     |0.184   |              |",
      "|Treatment Arm B |-1.366   |1.123     |0.227   |              |"
    )
  )
})

test_that("A basic modelsum call--suppressing intercept and/or adjustment vars", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.intercept = FALSE)),
    c("|                |estimate |std.error |p.value |adj.r.squared |",
      "|:---------------|:--------|:---------|:-------|:-------------|",
      "|Sex Male        |-0.221   |1.112     |0.843   |-0.005        |",
      "|Treatment Arm B |-1.373   |1.135     |0.229   |              |",
      "|time            |-0.368   |0.275     |0.184   |0.014         |",
      "|Treatment Arm B |-1.366   |1.123     |0.227   |              |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.adjust = FALSE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |40.632   |1.024     |< 0.001 |-0.005        |",
      "|Sex Male    |-0.221   |1.112     |0.843   |              |",
      "|(Intercept) |41.938   |1.366     |< 0.001 |0.014         |",
      "|time        |-0.368   |0.275     |0.184   |              |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.intercept = FALSE, show.adjust = FALSE)),
    c("|         |estimate |std.error |p.value |adj.r.squared |",
      "|:--------|:--------|:---------|:-------|:-------------|",
      "|Sex Male |-0.221   |1.112     |0.843   |-0.005        |",
      "|time     |-0.368   |0.275     |0.184   |0.014         |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.intercept = FALSE, show.adjust = FALSE)),
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat, show.intercept = FALSE, show.adjust = FALSE), text = TRUE))
  )
})


test_that("Reordering variables", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[c(3,1,2)], text = TRUE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |41.130   |1.197     |< 0.001 |0.009         |",
      "|time        |-0.371   |0.275     |0.182   |              |",
      "|(Intercept) |39.826   |0.779     |< 0.001 |-0.011        |",
      "|Sex Male    |-0.258   |1.115     |0.818   |              |",
      "|(Intercept) |40.033   |0.970     |< 0.001 |-0.021        |",
      "|Group Low   |-0.400   |1.372     |0.771   |              |",
      "|Group Med   |-0.600   |1.372     |0.663   |              |"
    )
  )

  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[c(3,1,2)], text = TRUE)),
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[c("time", "Sex", "Group")], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[1:2], text = TRUE)),
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[c(TRUE, TRUE, FALSE)], text = TRUE))
  )

  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat), text = TRUE)),
    capture.kable(summary(modelsum(Age ~ Sex + Group + time, data = mdat)[], text = TRUE))
  )

  expect_warning(modelsum(Age ~ Sex + Group + time, data = mdat)[1:4], "Some indices not found")
  expect_error(modelsum(Age ~ Sex + Group + time, data = mdat)[TRUE], "Logical vector")

})

test_that("offset() works", {
  expect_error(summary(modelsum(fu.stat ~ age, adjust=~offset(log(fu.time+.01))+ sex + arm,
                                data=mockstudy, family=poisson)), NA)
})

###########################################################################################################
#### Reported bugs for modelsum
###########################################################################################################

set.seed(3248)
dat <- data.frame(short.name = rnorm(100), really.long.name = rnorm(100),
                  why.would.you.name.something = rnorm(100),
                  as.long.as.this = rnorm(100))


test_that("01/26/2017: Brendan Broderick's Bold Text Wrapping Problem", {
  expect_identical(
    capture.kable(summary(modelsum(short.name ~ really.long.name + as.long.as.this, adjust = ~ why.would.you.name.something, data = dat))),
    c("|                                 |estimate |std.error |p.value |adj.r.squared |",
      "|:--------------------------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)                      |0.035    |0.099     |0.721   |-0.001        |",
      "|**really.long.name**             |0.099    |0.099     |0.319   |              |",
      "|**why.would.you.name.something** |-0.083   |0.090     |0.361   |              |",
      "|(Intercept)                      |0.048    |0.097     |0.624   |0.023         |",
      "|**as.long.as.this**              |0.198    |0.106     |0.066   |              |",
      "|**why.would.you.name.something** |-0.090   |0.089     |0.314   |              |"
    )
  )
})
rm(dat)

#################################################################################################################################

test_that("02/07/2017: Ryan Lennon's R Markdown spacing problem. Also 02/14/2018 (#66)", {
  expect_error(capture.kable(summary(modelsum(Age ~ Sex + time, data = mdat), text = TRUE)), NA)
})

#################################################################################################################################


test_that("02/13/2017: Krista Goergen's survival subset and NA problems", {
  if(require(survival) && packageVersion("survival") >= "2.41-3")
  {
    mdat.tmp <- keep.labels(mdat)

    form <- Surv(time, status) ~ Sex + ethan
    expect_identical(capture.kable(summary(modelsum(form, data = mdat.tmp, subset = Group=="High", family="survival"), text = TRUE)),
                     capture.kable(summary(modelsum(form, data = mdat.tmp[mdat.tmp$Group=="High",], family="survival"), text = TRUE)))

    mdat.tmp[3:4,"time"] <- c(NA,NA)
    expect_identical(capture.kable(summary(modelsum(form, data = mdat.tmp, subset = Group=="High", family="survival"), text = TRUE)),
                     capture.kable(summary(modelsum(form, data = mdat.tmp[mdat.tmp$Group=="High",], family="survival"), text = TRUE)))

    expect_identical(capture.kable(summary(modelsum(form, adjust = ~Age, data = mdat.tmp, subset = Group=="High", family="survival"), text = TRUE)),
                     capture.kable(summary(modelsum(form, adjust = ~Age, data = mdat.tmp[mdat.tmp$Group=="High",], family="survival"), text = TRUE)))

    expect_identical(
      capture.kable(summary(modelsum(form, adjust = ~Age, data = mdat.tmp, subset = Group=="High", family="survival"), text = TRUE)),
      c("|              |HR    |CI.lower.HR |CI.upper.HR |p.value |concordance |Nmiss |",
        "|:-------------|:-----|:-----------|:-----------|:-------|:-----------|:-----|",
        "|Sex Male      |0.612 |0.210       |1.786       |0.369   |0.592       |0     |",
        "|Age in Years  |1.061 |0.968       |1.164       |0.205   |            |      |",
        "|ethan Heinzen |1.019 |0.297       |3.501       |0.976   |0.639       |3     |",
        "|Age in Years  |1.058 |0.960       |1.166       |0.258   |            |      |"
      )
    )

    rm(mdat.tmp)
  } else skip("survival package not available or not the right version.")
})

#################################################################################################################################

test_that("04/12/2017: ... vs modelsum.control", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat, show.adjust = FALSE, control = modelsum.control()), text = TRUE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |40.632   |1.024     |< 0.001 |-0.005        |",
      "|Sex Male    |-0.221   |1.112     |0.843   |              |",
      "|(Intercept) |41.938   |1.366     |< 0.001 |0.014         |",
      "|time        |-0.368   |0.275     |0.184   |              |"
    )
  )
})

#################################################################################################################################

data(mockstudy)
test_that("08/01/2017: Beth Atkinson's subset problem", {
  idx <- mockstudy$sex == "Male"
  form <- fu.stat - 1 ~ age + hgb
  expect_identical(capture.kable(summary(modelsum(form, data = mockstudy, subset = idx, adjust = ~arm, family="binomial"), text = TRUE)),
                   capture.kable(summary(modelsum(form, data = mockstudy, subset = sex == "Male", adjust = ~arm, family="binomial"), text = TRUE)))
})


#################################################################################################################################

set.seed(88)
df <- data.frame(
  y = rnorm(1000),
  x1 = rnorm(1000),
  x2 = rnorm(1000),
  x3 = rpois(1000, 2),
  x5 = rnorm(1000),
  x7 = sample(LETTERS[1:5], 1000, replace = TRUE),
  x8 = runif(1000)
)

data(mockstudy)
test_that("07/27/2017: Too many adjustment vars in as.data.frame.modelsum (#12)", {
  expect_equal(nrow(as.data.frame(modelsum(y ~ x1, adjust = ~ x7 + x2 + x3 + x5 + x8, data = df))), 10L)
})

#################################################################################################################################

test_that("07/27/2017: modelsum labels (#13)", {
  expect_identical(
    capture.kable(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy), labelTranslations = list(sexFemale = "Female", age = "Age, yrs"), text = TRUE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |26.793   |0.766     |< 0.001 |0.004         |",
      "|Age, yrs    |0.012    |0.012     |0.348   |              |",
      "|Female      |-0.718   |0.291     |0.014   |              |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy), labelTranslations = list(sexFemale = "Female", age = "Age, yrs"), text = TRUE)),
    capture.kable(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy), labelTranslations = c(sexFemale = "Female", age = "Age, yrs"), text = TRUE))
  )
  expect_warning(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy), labelTranslations = c(badvar = "Eek")), "badvar")
})


#################################################################################################################################

test_that("12/23/2017: non-syntactic names (#44, #45)", {
  dat <- data.frame(y = 1:10, x1x = rep(c("A", "B"), each = 5),
                    `1x` = rep(c("C", "D"), each = 5),
                    stringsAsFactors = FALSE, check.names = FALSE)
  expect_identical(
    capture.kable(summary(modelsum(y ~ x1x, data = dat))),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |3.000    |0.707     |0.003   |0.727         |",
      "|**x1x B**   |5.000    |1.000     |0.001   |              |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(y ~ `1x`, data = dat))),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |3.000    |0.707     |0.003   |0.727         |",
      "|**1x D**    |5.000    |1.000     |0.001   |              |"
    )
  )
})

#################################################################################################################################

test_that("01/05/2018: leading/trailing whitespace (#48)", {
  expect_identical(
    capture.kable(summary(modelsum(age ~ arm, data = set_labels(mockstudy, list(arm = " Arm "))))),
    c("|                   |estimate |std.error |p.value |adj.r.squared |",
      "|:------------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)        |59.673   |0.557     |< 0.001 |-0.001        |",
      "|**Arm  F: FOLFOX** |0.628    |0.709     |0.376   |              |",
      "|**Arm  G: IROX**   |0.090    |0.812     |0.912   |              |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(age ~ arm, data = set_labels(mockstudy, list(arm = " Arm "))), text = TRUE)),
    c("|               |estimate |std.error |p.value |adj.r.squared |",
      "|:--------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)    |59.673   |0.557     |< 0.001 |-0.001        |",
      "|Arm  F: FOLFOX |0.628    |0.709     |0.376   |              |",
      "|Arm  G: IROX   |0.090    |0.812     |0.912   |              |"
    )
  )
})
