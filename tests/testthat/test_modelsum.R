## Tests for modelsum


context("Testing the modelsum output")

# "mdat" now defined in helper-data.R

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

test_that("strata() works", {
  skip_if_not(getRversion() >= "3.3.0")
  skip_if_not_installed("survival", "2.41-3")
  require(survival)
  expect_identical(
    capture.kable(summary(modelsum(Surv(time, status) ~ ethan, adjust = ~strata(Sex), data = mdat, family="survival"), text = TRUE)),
    c("|              |HR    |CI.lower.HR |CI.upper.HR |p.value |concordance |Nmiss |",
      "|:-------------|:-----|:-----------|:-----------|:-------|:-----------|:-----|",
      "|ethan Heinzen |1.051 |0.549       |2.014       |0.880   |0.499       |3     |"
    )
  )
})

test_that("'weights=' works", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex, data = mdat, weights = weights))),
    c("|             |estimate |std.error |p.value |adj.r.squared |",
      "|:------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)  |39.826   |0.889     |< 0.001 |0.020         |",
      "|**Sex Male** |1.953    |1.167     |0.098   |              |"
    )
  )
})

test_that("interactions work", {
  expect_identical(
    capture.kable(summary(modelsum(age ~ bmi, adjust = ~ sex*arm, data=mockstudy))),
    c("|                                       |estimate |std.error |p.value |adj.r.squared |Nmiss |",
      "|:--------------------------------------|:--------|:---------|:-------|:-------------|:-----|",
      "|(Intercept)                            |58.401   |1.691     |< 0.001 |0.001         |33    |",
      "|**Body Mass Index (kg/m^2)**           |0.051    |0.056     |0.362   |              |      |",
      "|**sex Female**                         |-0.351   |1.177     |0.765   |              |      |",
      "|**Treatment Arm F: FOLFOX**            |0.852    |0.908     |0.348   |              |      |",
      "|**Treatment Arm G: IROX**              |0.979    |1.040     |0.347   |              |      |",
      "|**sex Female:Treatment Arm F: FOLFOX** |-0.596   |1.485     |0.688   |              |      |",
      "|**sex Female:Treatment Arm G: IROX**   |-1.975   |1.688     |0.242   |              |      |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(age ~ bmi, adjust = ~ hgb*arm, data=mockstudy))),
    c("|                                |estimate |std.error |p.value |adj.r.squared |Nmiss |",
      "|:-------------------------------|:--------|:---------|:-------|:-------------|:-----|",
      "|(Intercept)                     |54.324   |4.747     |< 0.001 |0.004         |33    |",
      "|**Body Mass Index (kg/m^2)**    |0.029    |0.062     |0.643   |              |      |",
      "|**hgb**                         |0.404    |0.366     |0.271   |              |      |",
      "|**Treatment Arm F: FOLFOX**     |-1.386   |5.748     |0.809   |              |      |",
      "|**Treatment Arm G: IROX**       |-1.228   |6.589     |0.852   |              |      |",
      "|**hgb:Treatment Arm F: FOLFOX** |0.176    |0.462     |0.703   |              |      |",
      "|**hgb:Treatment Arm G: IROX**   |0.052    |0.529     |0.922   |              |      |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(age ~ bmi:arm, adjust = ~ hgb, data=mockstudy))),
    c("|                                                     |estimate |std.error |p.value |adj.r.squared |Nmiss |",
      "|:----------------------------------------------------|:--------|:---------|:-------|:-------------|:-----|",
      "|(Intercept)                                          |53.303   |2.822     |< 0.001 |0.005         |33    |",
      "|**hgb**                                              |0.499    |0.193     |0.010   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm A: IFL**    |0.023    |0.065     |0.721   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm F: FOLFOX** |0.052    |0.063     |0.416   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm G: IROX**   |0.002    |0.065     |0.973   |              |      |"
    )
  )
  expect_identical(
    capture.kable(summary(modelsum(age ~ bmi:arm, adjust = ~ hgb, data=mockstudy))),
    c("|                                                     |estimate |std.error |p.value |adj.r.squared |Nmiss |",
      "|:----------------------------------------------------|:--------|:---------|:-------|:-------------|:-----|",
      "|(Intercept)                                          |53.303   |2.822     |< 0.001 |0.005         |33    |",
      "|**hgb**                                              |0.499    |0.193     |0.010   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm A: IFL**    |0.023    |0.065     |0.721   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm F: FOLFOX** |0.052    |0.063     |0.416   |              |      |",
      "|**Body Mass Index (kg/m^2):Treatment Arm G: IROX**   |0.002    |0.065     |0.973   |              |      |"
    )
  )
  expect_identical(
    as.data.frame(modelsum(age ~ bmi:arm, adjust = ~ hgb, data=mockstudy))$term.type,
    c("Intercept", "Adjuster", "Term", "Term", "Term")
  )
})


test_that("ordinal works", {
  if(require(MASS))
  {
    data(housing)
    expect_identical(
      capture.kable(summary(modelsum(Sat ~ Infl, adjust = ~ Type + Cont, weights = Freq, data = housing, family = "ordinal"))),
      c("|                   |OR    |CI.lower.OR |CI.upper.OR |p.value |",
        "|:------------------|:-----|:-----------|:-----------|:-------|",
        "|Low&#124;Medium    |NA    |NA          |NA          |< 0.001 |",
        "|Medium&#124;High   |NA    |NA          |NA          |< 0.001 |",
        "|**Cont High**      |1.434 |1.189       |1.730       |< 0.001 |",
        "|**Infl High**      |3.628 |2.832       |4.663       |< 0.001 |",
        "|**Infl Medium**    |1.762 |1.436       |2.164       |< 0.001 |",
        "|**Type Apartment** |0.564 |0.446       |0.712       |< 0.001 |",
        "|**Type Atrium**    |0.693 |0.511       |0.940       |0.018   |",
        "|**Type Terrace**   |0.336 |0.249       |0.451       |< 0.001 |"
      )
    )
    expect_identical(
      capture.kable(summary(modelsum(Sat ~ Infl, adjust = ~ Type + Cont, weights = Freq, data = housing, family = "ordinal",
                                     ordinal.stats = c("estimate", "statistic", "p.value")), text = TRUE)),
      c("|                 |estimate |statistic |p.value |",
        "|:----------------|:--------|:---------|:-------|",
        "|Low&#124;Medium  |-0.496   |-3.974    |< 0.001 |",
        "|Medium&#124;High |0.691    |5.505     |< 0.001 |",
        "|Cont High        |0.360    |3.771     |< 0.001 |",
        "|Infl High        |1.289    |10.136    |< 0.001 |",
        "|Infl Medium      |0.566    |5.412     |< 0.001 |",
        "|Type Apartment   |-0.572   |-4.800    |< 0.001 |",
        "|Type Atrium      |-0.366   |-2.360    |0.018   |",
        "|Type Terrace     |-1.091   |-7.202    |< 0.001 |"
      )
    )
    expect_identical(
      capture.kable(summary(modelsum(Sat ~ Infl, adjust = ~ Type + Cont, weights = Freq, data = housing, family = "ordinal",
                                     show.adjust = FALSE, show.intercept = FALSE), text = TRUE)),
      c("|            |OR    |CI.lower.OR |CI.upper.OR |p.value |",
        "|:-----------|:-----|:-----------|:-----------|:-------|",
        "|Infl High   |3.628 |2.832       |4.663       |< 0.001 |",
        "|Infl Medium |1.762 |1.436       |2.164       |< 0.001 |"
      )
    )
  } else skip("'MASS' is not available")
})


test_that("negbin works", {
  if(require(MASS))
  {
    data(mockstudy)
    expect_identical(
      capture.kable(summary(modelsum(fu.time ~ sex, adjust = ~ age + arm, data = mockstudy, family = negbin),
                            negbin.stats = c("estimate", "p.value", "theta"), text = TRUE, digits = 5)),
      c("|                        |estimate |p.value |theta   |",
        "|:-----------------------|:--------|:-------|:-------|",
        "|(Intercept)             |6.52819  |< 0.001 |1.84776 |",
        "|sex Female              |-0.02370 |0.545   |        |",
        "|Age in Years            |-0.00342 |0.039   |        |",
        "|Treatment Arm F: FOLFOX |0.28161  |< 0.001 |        |",
        "|Treatment Arm G: IROX   |0.09396  |0.071   |        |"
      )
    )
  } else skip("'MASS' is not available")
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
  skip_if_not(getRversion() >= "3.3.0")
  skip_if_not_installed("survival", "2.41-3")
  require(survival)
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
  x7 = rep(LETTERS[1:5], each = 200),
  x8 = runif(1000)
)

test_that("07/27/2017: Too many adjustment vars in as.data.frame.modelsum (#12)", {
  expect_equal(nrow(as.data.frame(modelsum(y ~ x1, adjust = ~ x7 + x2 + x3 + x5 + x8, data = df))), 10L)
})

#################################################################################################################################

test_that("07/27/2017: modelsum labels (#13)", {
  expect_identical(
    capture.kable(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy),
                          labelTranslations = list(sexFemale = "Female", age = "Age, yrs"), text = TRUE)),
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
  expect_warning(summary(modelsum(bmi ~ age, adjust = ~sex, data = mockstudy), labelTranslations = c(badvar = "Eek")), NA)
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

#################################################################################################################################

test_that("02/23/2018: wrapping long labels (#59)", {
  labs <- list(
    Group = "This is a really long label for the Group variable",
    time = "Another really long label. Can you believe how long this is",
    dt = "ThisLabelHasNoSpacesSoLetsSeeHowItBehaves"
  )
  expect_identical(
    capture.kable(print(summary(modelsum(Age ~ Group + time + dt, data = set_labels(mdat, labs)), text = TRUE), width = 30)),
    c("|                               |estimate |std.error |p.value |adj.r.squared |",
      "|:------------------------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)                    |40.033   |0.970     |< 0.001 |-0.021        |",
      "|This is a really long label    |-0.400   |1.372     |0.771   |              |",
      "|for the Group variable Low     |         |          |        |              |",
      "|This is a really long label    |-0.600   |1.372     |0.663   |              |",
      "|for the Group variable Med     |         |          |        |              |",
      "|(Intercept)                    |41.130   |1.197     |< 0.001 |0.009         |",
      "|Another really long label.     |-0.371   |0.275     |0.182   |              |",
      "|Can you believe how long this  |         |          |        |              |",
      "|is                             |         |          |        |              |",
      "|(Intercept)                    |41.531   |2.017     |< 0.001 |-0.001        |",
      "|ThisLabelHasNoSpacesSoLetsSeeH |0.000    |0.000     |0.348   |              |",
      "|owItBehaves                    |         |          |        |              |"
    )
  )
})

#################################################################################################################################

test_that("05/31/2018: similar column names (#98)", {
  dat <- data.frame(
    y = c(1:9, 11),
    a = c(2, 2, 1:8),
    aa = c(1, 1:9),
    b = factor(rep(c("a", "b"), each = 5))
  )
  expect_identical(
    capture.kable(summary(modelsum(y ~ b, adjust = ~a + aa, data = dat))),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |0.417    |0.295     |0.208   |0.984         |",
      "|**b b**     |-0.467   |0.548     |0.427   |              |",
      "|**a**       |-0.083   |0.217     |0.714   |              |",
      "|**aa**      |1.250    |0.183     |< 0.001 |              |"
    )
  )
})

#################################################################################################################################

test_that("05/31/2018: similar column names (#100)", {
  dat <- data.frame(
    y = 1:10,
    a = factor(rep(c("a", "b"), each = 5), levels = c("b", "a")),
    d = factor(rep(c("c", "d"), times = 5), levels = c("c", "d"))
  )
  expect_identical(
    capture.kable(summary(modelsum(y ~ a, adjust = ~ d, data = set_labels(dat, list(a = "A", d = "D"))), text = TRUE)),
    c("|            |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------|:--------|:---------|:-------|:-------------|",
      "|(Intercept) |8.000    |1.000     |< 0.001 |0.688         |",
      "|A a         |-5.000   |1.091     |0.003   |              |",
      "|D d         |-0.000   |1.091     |1.000   |              |"
    )
  )
})

#################################################################################################################################

test_that("06/19/2018: term.name (#109)", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, term.name = "Term")),
    c("|Term            |estimate |std.error |p.value |adj.r.squared |",
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

#################################################################################################################################

test_that("08/24/2018: latex (#123)", {
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex, adjust = ~ trt, data = mdat), text = "latex")),
    c(""                                                         ,
      "\\begin{tabular}{l|l|l|l|l}"                              ,
      "\\hline"                                                  ,
      " & estimate & std.error & p.value & adj.r.squared\\\\"    ,
      "\\hline"                                                  ,
      "(Intercept) & 40.632 & 1.024 & < 0.001 & -0.005\\\\"      ,
      "\\hline"                                                  ,
      "\\textbf{Sex Male} & -0.221 & 1.112 & 0.843 & \\\\"       ,
      "\\hline"                                                  ,
      "\\textbf{Treatment Arm B} & -1.373 & 1.135 & 0.229 & \\\\",
      "\\hline"                                                  ,
      "\\end{tabular}"                                           ,
      ""
    )
  )
})

#################################################################################################################################

test_that("09/05/2018: correctly label contrasts for ordinal variables (#133)", {
  mdat$Group.ord <- ordered(mdat$Group.fac, levels = c("Low", "Med", "High"))
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Phase, adjust = ~ Group.ord + trt + ht_in, data = mdat), text = TRUE)),
    c("|                 |estimate |std.error |p.value |adj.r.squared |",
      "|:----------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)      |47.686   |7.301     |< 0.001 |-0.019        |",
      "|Phase .L         |-0.679   |1.152     |0.557   |              |",
      "|Phase .Q         |-1.044   |0.960     |0.280   |              |",
      "|Group.ord .L     |0.243    |0.977     |0.804   |              |",
      "|Group.ord .Q     |0.410    |1.069     |0.702   |              |",
      "|Treatment Arm B  |-1.460   |1.159     |0.211   |              |",
      "|Height in Inches |-0.112   |0.110     |0.314   |              |"
    )
  )
})
