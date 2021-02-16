context("Testing the modelsum strata and multiple LHS output")

###########################################################################################################
#### Basic modelsum call
###########################################################################################################

test_that("A three-LHS modelsum call", {
  expect_identical(
    capture.kable(summary(modelsum(list(ht_in, time, Age) ~ Sex + dt, adjust = ~ trt + Phase, data = mdat), text = TRUE)),
    c(
      capture.kable(summary(modelsum(ht_in ~ Sex + dt, adjust = ~ trt + Phase, data = mdat), text = TRUE)), "", "",
      capture.kable(summary(modelsum(time ~ Sex + dt, adjust = ~ trt + Phase, data = mdat), text = TRUE)), "", "",
      capture.kable(summary(modelsum(Age ~ Sex + dt, adjust = ~ trt + Phase, data = mdat), text = TRUE))
    )
  )
})

test_that("A modelsum call with strata", {
  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + dt, adjust = ~ trt, data = mdat, strata = Group), text = TRUE)),
    c("|Group |                |estimate |std.error |p.value |adj.r.squared |",
      "|:-----|:---------------|:--------|:---------|:-------|:-------------|",
      "|High  |(Intercept)     |40.833   |2.062     |< 0.001 |-0.050        |",
      "|      |Sex Male        |0.333    |2.326     |0.887   |              |",
      "|      |Treatment Arm B |-1.812   |2.331     |0.444   |              |",
      "|      |(Intercept)     |39.853   |3.948     |< 0.001 |-0.047        |",
      "|      |dt              |-0.000   |0.001     |0.750   |              |",
      "|      |Treatment Arm B |-1.910   |2.347     |0.423   |              |",
      "|Low   |(Intercept)     |40.524   |1.304     |< 0.001 |-0.040        |",
      "|      |Sex Male        |-1.192   |1.463     |0.422   |              |",
      "|      |Treatment Arm B |-0.591   |1.505     |0.697   |              |",
      "|      |(Intercept)     |45.146   |2.751     |< 0.001 |0.074         |",
      "|      |dt              |0.001    |0.000     |0.054   |              |",
      "|      |Treatment Arm B |-0.363   |1.423     |0.801   |              |",
      "|Med   |(Intercept)     |40.300   |2.069     |< 0.001 |-0.056        |",
      "|      |Sex Male        |0.117    |2.095     |0.956   |              |",
      "|      |Treatment Arm B |-1.467   |2.169     |0.505   |              |",
      "|      |(Intercept)     |42.941   |4.383     |< 0.001 |-0.040        |",
      "|      |dt              |0.000    |0.001     |0.528   |              |",
      "|      |Treatment Arm B |-1.650   |2.171     |0.454   |              |"
    )
  )
})

test_that("strata levels are maintained", {
  tmp <- mdat
  tmp$Group[tmp$trt == "A" & tmp$Group == "Low"] <- NA
  expect_identical(
    capture.kable(summary(set_labels(modelsum(Age ~ Sex + dt, adjust = ~ Group, strata = trt, data = tmp), c(GroupLow = "Low")), text = TRUE)),
    c("|Treatment Arm |            |estimate |std.error |p.value |adj.r.squared |Nmiss |",
      "|:-------------|:-----------|:--------|:---------|:-------|:-------------|:-----|",
      "|A             |(Intercept) |42.139   |2.047     |< 0.001 |-0.047        |11    |",
      "|              |Sex Male    |-2.277   |2.456     |0.364   |              |      |",
      "|              |Group Med   |-0.533   |2.472     |0.831   |              |      |",
      "|              |(Intercept) |34.850   |3.600     |< 0.001 |0.065         |11    |",
      "|              |dt          |-0.001   |0.000     |0.072   |              |      |",
      "|              |Group Med   |-1.642   |2.394     |0.500   |              |      |",
      "|B             |(Intercept) |38.733   |1.470     |< 0.001 |-0.050        |0     |",
      "|              |Sex Male    |0.908    |1.407     |0.522   |              |      |",
      "|              |Low         |0.205    |1.753     |0.907   |              |      |",
      "|              |Group Med   |-0.317   |1.753     |0.857   |              |      |",
      "|              |(Intercept) |47.149   |2.812     |< 0.001 |0.114         |0     |",
      "|              |dt          |0.001    |0.000     |0.003   |              |      |",
      "|              |Low         |-0.145   |1.613     |0.929   |              |      |",
      "|              |Group Med   |-0.318   |1.610     |0.844   |              |      |"
    )
  )
})

test_that("Multiple adjustments work (#240)", {
  tmp.ms <- modelsum(list(age, bmi) ~ fu.time + ast, adjust = list(Unadjusted = ~ 1, "Adjusted for Arm" = ~ arm, "HGB + PS" = ~ hgb + ps),
                     data = set_labels(mockstudy, list(ast = "AST", ps = "PS")), strata = sex,
                     gaussian.stats = c("estimate", "std.error", "p.value", "adj.r.squared", "Nmiss", "N"))
  tmp.ms2 <- modelsum(list(age, bmi) ~ fu.time + ast, adjust = list(Unadjusted = ~ NULL, "Adjusted for Arm" = ~ arm, "HGB + PS" = ~ hgb + ps),
                      data = set_labels(mockstudy, list(ast = "AST", ps = "PS")), strata = sex,
                      gaussian.stats = c("estimate", "std.error", "p.value", "adj.r.squared", "Nmiss", "N"))
  expect_identical(
    capture.kable(summary(tmp.ms, adjustment.names = TRUE, term.name = TRUE)),
    capture.kable(summary(tmp.ms2, adjustment.names = TRUE, term.name = TRUE))
  )

  expect_identical(
    capture.kable(summary(tmp.ms, adjustment.names = TRUE, term.name = TRUE)),
    c("|sex    |adjustment       |Age in Years                |estimate |std.error |p.value |adj.r.squared |Nmiss |N   |",
      "|:------|:----------------|:---------------------------|:--------|:---------|:-------|:-------------|:-----|:---|",
      "|Male   |Unadjusted       |(Intercept)                 |61.686   |0.654     |< 0.001 |0.005         |0     |916 |",
      "|       |                 |**fu.time**                 |-0.002   |0.001     |0.022   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |61.073   |0.837     |< 0.001 |0.004         |0     |916 |",
      "|       |                 |**fu.time**                 |-0.002   |0.001     |0.016   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |1.048    |0.889     |0.239   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |0.857    |1.015     |0.399   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |57.090   |3.068     |< 0.001 |0.010         |162   |754 |",
      "|       |                 |**fu.time**                 |-0.002   |0.001     |0.008   |              |      |    |",
      "|       |                 |**hgb**                     |0.365    |0.234     |0.118   |              |      |    |",
      "|       |                 |**PS**                      |0.881    |0.746     |0.238   |              |      |    |",
      "|       |Unadjusted       |(Intercept)                 |61.556   |0.696     |< 0.001 |0.003         |162   |754 |",
      "|       |                 |**AST**                     |-0.028   |0.016     |0.074   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |61.524   |0.947     |< 0.001 |0.001         |162   |754 |",
      "|       |                 |**AST**                     |-0.028   |0.016     |0.075   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |0.322    |0.977     |0.741   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-0.445   |1.115     |0.690   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |57.707   |3.133     |< 0.001 |0.006         |162   |754 |",
      "|       |                 |**AST**                     |-0.032   |0.016     |0.043   |              |      |    |",
      "|       |                 |**hgb**                     |0.253    |0.232     |0.276   |              |      |    |",
      "|       |                 |**PS**                      |1.529    |0.739     |0.039   |              |      |    |",
      "|Female |Unadjusted       |(Intercept)                 |59.385   |0.822     |< 0.001 |-0.002        |0     |583 |",
      "|       |                 |**fu.time**                 |-0.000   |0.001     |0.835   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |59.449   |1.089     |< 0.001 |-0.003        |0     |583 |",
      "|       |                 |**fu.time**                 |-0.000   |0.001     |0.707   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |0.580    |1.216     |0.634   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-0.856   |1.353     |0.527   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |46.660   |4.632     |< 0.001 |0.015         |104   |479 |",
      "|       |                 |**fu.time**                 |-0.001   |0.001     |0.564   |              |      |    |",
      "|       |                 |**hgb**                     |1.156    |0.378     |0.002   |              |      |    |",
      "|       |                 |**PS**                      |-0.421   |0.902     |0.641   |              |      |    |",
      "|       |Unadjusted       |(Intercept)                 |61.001   |0.883     |< 0.001 |0.004         |104   |479 |",
      "|       |                 |**AST**                     |-0.033   |0.020     |0.096   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |60.687   |1.266     |< 0.001 |0.006         |104   |479 |",
      "|       |                 |**AST**                     |-0.032   |0.020     |0.102   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |1.253    |1.297     |0.334   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-1.006   |1.441     |0.486   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |47.825   |4.730     |< 0.001 |0.017         |104   |479 |",
      "|       |                 |**AST**                     |-0.026   |0.020     |0.193   |              |      |    |",
      "|       |                 |**hgb**                     |1.086    |0.376     |0.004   |              |      |    |",
      "|       |                 |**PS**                      |-0.115   |0.873     |0.895   |              |      |    |",
      ""                                                                                                                 ,
      ""                                                                                                                 ,
      "|sex    |adjustment       |Body Mass Index (kg/m^2)    |estimate |std.error |p.value |adj.r.squared |Nmiss |N   |",
      "|:------|:----------------|:---------------------------|:--------|:---------|:-------|:-------------|:-----|:---|",
      "|Male   |Unadjusted       |(Intercept)                 |26.679   |0.294     |< 0.001 |0.011         |22    |894 |",
      "|       |                 |**fu.time**                 |0.001    |0.000     |< 0.001 |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |26.901   |0.376     |< 0.001 |0.012         |22    |894 |",
      "|       |                 |**fu.time**                 |0.001    |0.000     |< 0.001 |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |-0.594   |0.397     |0.134   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-0.040   |0.451     |0.930   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |23.559   |1.321     |< 0.001 |0.036         |181   |735 |",
      "|       |                 |**fu.time**                 |0.001    |0.000     |0.021   |              |      |    |",
      "|       |                 |**hgb**                     |0.283    |0.101     |0.005   |              |      |    |",
      "|       |                 |**PS**                      |-0.794   |0.322     |0.014   |              |      |    |",
      "|       |Unadjusted       |(Intercept)                 |27.885   |0.306     |< 0.001 |0.006         |181   |735 |",
      "|       |                 |**AST**                     |-0.016   |0.007     |0.023   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |28.040   |0.412     |< 0.001 |0.004         |181   |735 |",
      "|       |                 |**AST**                     |-0.016   |0.007     |0.023   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |-0.354   |0.427     |0.408   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |0.015    |0.486     |0.976   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |24.297   |1.350     |< 0.001 |0.032         |181   |735 |",
      "|       |                 |**AST**                     |-0.011   |0.007     |0.123   |              |      |    |",
      "|       |                 |**hgb**                     |0.307    |0.100     |0.002   |              |      |    |",
      "|       |                 |**PS**                      |-0.878   |0.318     |0.006   |              |      |    |",
      "|Female |Unadjusted       |(Intercept)                 |26.198   |0.423     |< 0.001 |0.003         |11    |572 |",
      "|       |                 |**fu.time**                 |0.001    |0.001     |0.101   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |26.198   |0.560     |< 0.001 |0.002         |11    |572 |",
      "|       |                 |**fu.time**                 |0.001    |0.001     |0.164   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |0.366    |0.625     |0.558   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-0.386   |0.692     |0.577   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |27.620   |2.409     |< 0.001 |-0.000        |113   |470 |",
      "|       |                 |**fu.time**                 |0.001    |0.001     |0.099   |              |      |    |",
      "|       |                 |**hgb**                     |-0.121   |0.196     |0.539   |              |      |    |",
      "|       |                 |**PS**                      |0.184    |0.467     |0.694   |              |      |    |",
      "|       |Unadjusted       |(Intercept)                 |26.500   |0.456     |< 0.001 |0.001         |113   |470 |",
      "|       |                 |**AST**                     |0.012    |0.010     |0.247   |              |      |    |",
      "|       |Adjusted for Arm |(Intercept)                 |26.062   |0.649     |< 0.001 |0.008         |113   |470 |",
      "|       |                 |**AST**                     |0.012    |0.010     |0.225   |              |      |    |",
      "|       |                 |**Treatment Arm F: FOLFOX** |1.102    |0.669     |0.100   |              |      |    |",
      "|       |                 |**Treatment Arm G: IROX**   |-0.276   |0.741     |0.710   |              |      |    |",
      "|       |HGB + PS         |(Intercept)                 |27.324   |2.462     |< 0.001 |-0.003        |113   |470 |",
      "|       |                 |**AST**                     |0.012    |0.010     |0.255   |              |      |    |",
      "|       |                 |**hgb**                     |-0.064   |0.196     |0.743   |              |      |    |",
      "|       |                 |**PS**                      |-0.103   |0.454     |0.821   |              |      |    |"
    )
  )

  expect_identical(
    capture.kable(summary(modelsum(Age ~ Sex + time, data = mdat), text = TRUE, adjustment.names = TRUE)),
    c("|adjustment |            |estimate |std.error |p.value |adj.r.squared |",
      "|:----------|:-----------|:--------|:---------|:-------|:-------------|",
      "|unadjusted |(Intercept) |39.826   |0.779     |< 0.001 |-0.011        |",
      "|           |Sex Male    |-0.258   |1.115     |0.818   |              |",
      "|unadjusted |(Intercept) |41.130   |1.197     |< 0.001 |0.009         |",
      "|           |time        |-0.371   |0.275     |0.182   |              |"
    )
  )
})

###########################################################################################################
#### Other warnings and tests and things...
###########################################################################################################

test_that("Reordering variables and subsetting", {
  tmp.tab <- modelsum(list(Age, time) ~ Sex + dt + ethan, adjust = ~ Group, strata = trt, data = mdat)

  expect_identical(
    capture.kable(summary(tmp.tab[c(3,1,2), 2:1], text = TRUE)),
    capture.kable(summary(modelsum(list(time, Age) ~ ethan + Sex + dt, adjust = ~ Group, strata = trt, data = mdat), text = TRUE))
  )

  expect_identical(
    capture.kable(summary(tmp.tab[c(3,1,2)], text = TRUE)),
    capture.kable(summary(tmp.tab[c("ethan", "Sex", "dt")], text = TRUE))
  )
  expect_identical(
    capture.kable(summary(tmp.tab[, 2:1], text = TRUE)),
    capture.kable(summary(tmp.tab[, c("time", "Age")], text = TRUE))
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

test_that("Merging modelsum objects", {
  tb1 <- modelsum(list(Age, time) ~ Sex + Phase, strata = trt, data = mdat)
  tb2 <- modelsum(list(as.numeric(dt), ht_in) ~ Group, strata = trt, data = mdat)
  tb3 <- modelsum(list(Age, ht_in, time) ~ Group + ethan, strata = trt, data = mdat)
  tb4 <- modelsum(list(Age, time) ~ Sex + Phase + Group + ethan, strata = trt, data = mdat)

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
      capture.kable(summary(modelsum(ht_in ~ Group + ethan, data = mdat, strata = trt), text = TRUE))
    )
  )
})

test_that("Changing labels", {
  mdat.tmp <- set_labels(mdat, NULL)
  mdat.tmp$`1x` <- 1:nrow(mdat.tmp)
  mdat.tmp$`2x` <- rep(c("A", "B"), each = nrow(mdat.tmp)/2)
  tab <- modelsum(list(Age, time) ~ Sex + `1x` + `2x`, adjust = ~ Group, strata = trt, data = mdat.tmp)
  tmp <- capture.kable(summary(tab, text = TRUE, term.name = TRUE))
  expect_warning(labels(tab) <- c(hi = "hi", SexMale = "Male", Age = "Age 1", time = "Time", trt = "Trt Arm",
                                  GroupLow = "Low", "`1x`" = "onex", "`2x`B" = "B"), NA)
  expect_identical(
    capture.kable(summary(tab, text = TRUE, term.name = TRUE)),
    c("|Trt Arm |Age 1       |estimate |std.error |p.value |adj.r.squared |",
      "|:-------|:-----------|:--------|:---------|:-------|:-------------|",
      "|A       |(Intercept) |42.000   |1.795     |< 0.001 |-0.052        |",
      "|        |Male        |-2.000   |1.918     |0.305   |              |",
      "|        |Low         |-1.182   |2.303     |0.611   |              |",
      "|        |Group Med   |-0.545   |2.290     |0.813   |              |",
      "|        |(Intercept) |41.277   |2.281     |< 0.001 |-0.087        |",
      "|        |onex        |-0.019   |0.113     |0.870   |              |",
      "|        |Low         |0.237    |7.336     |0.974   |              |",
      "|        |Group Med   |-0.083   |4.085     |0.984   |              |",
      "|        |(Intercept) |41.000   |1.543     |< 0.001 |-0.087        |",
      "|        |B           |-0.667   |3.495     |0.850   |              |",
      "|        |Low         |-0.242   |4.198     |0.954   |              |",
      "|        |Group Med   |-0.333   |2.817     |0.907   |              |",
      "|B       |(Intercept) |38.733   |1.470     |< 0.001 |-0.050        |",
      "|        |Male        |0.908    |1.407     |0.522   |              |",
      "|        |Low         |0.205    |1.753     |0.907   |              |",
      "|        |Group Med   |-0.317   |1.753     |0.857   |              |",
      "|        |(Intercept) |38.753   |1.835     |< 0.001 |-0.056        |",
      "|        |onex        |0.027    |0.081     |0.739   |              |",
      "|        |Low         |-1.414   |5.083     |0.782   |              |",
      "|        |Group Med   |-1.105   |2.996     |0.714   |              |",
      "|        |(Intercept) |39.187   |1.296     |< 0.001 |-0.058        |",
      "|        |B           |0.011    |2.383     |0.996   |              |",
      "|        |Low         |0.170    |2.962     |0.955   |              |",
      "|        |Group Med   |-0.299   |2.161     |0.891   |              |",
      ""                                                                    ,
      ""                                                                    ,
      "|Trt Arm |Time        |estimate |std.error |p.value |adj.r.squared |",
      "|:-------|:-----------|:--------|:---------|:-------|:-------------|",
      "|A       |(Intercept) |4.469    |0.568     |< 0.001 |-0.016        |",
      "|        |Male        |-0.223   |0.606     |0.716   |              |",
      "|        |Low         |-1.115   |0.728     |0.136   |              |",
      "|        |Group Med   |-0.620   |0.724     |0.398   |              |",
      "|        |(Intercept) |5.331    |0.671     |< 0.001 |0.090         |",
      "|        |onex        |-0.066   |0.033     |0.058   |              |",
      "|        |Low         |2.947    |2.159     |0.182   |              |",
      "|        |Group Med   |1.316    |1.202     |0.282   |              |",
      "|        |(Intercept) |4.357    |0.475     |< 0.001 |0.004         |",
      "|        |B           |-0.967   |1.076     |0.376   |              |",
      "|        |Low         |-0.118   |1.292     |0.928   |              |",
      "|        |Group Med   |-0.190   |0.867     |0.828   |              |",
      "|B       |(Intercept) |4.301    |0.591     |< 0.001 |0.086         |",
      "|        |Male        |0.898    |0.566     |0.119   |              |",
      "|        |Low         |-1.621   |0.705     |0.026   |              |",
      "|        |Group Med   |-0.879   |0.705     |0.218   |              |",
      "|        |(Intercept) |3.710    |0.725     |< 0.001 |0.112         |",
      "|        |onex        |0.065    |0.032     |0.048   |              |",
      "|        |Low         |-5.461   |2.008     |0.009   |              |",
      "|        |Group Med   |-2.797   |1.184     |0.022   |              |",
      "|        |(Intercept) |4.750    |0.516     |< 0.001 |0.098         |",
      "|        |B           |1.700    |0.948     |0.079   |              |",
      "|        |Low         |-3.345   |1.179     |0.007   |              |",
      "|        |Group Med   |-1.750   |0.860     |0.047   |              |"
    )
  )
  labels(tab) <- NULL
  expect_identical(
    capture.kable(summary(tab, text = TRUE, term.name = TRUE)),
    tmp
  )
  expect_identical(
    tmp,
    c("|trt |Age         |estimate |std.error |p.value |adj.r.squared |",
      "|:---|:-----------|:--------|:---------|:-------|:-------------|",
      "|A   |(Intercept) |42.000   |1.795     |< 0.001 |-0.052        |",
      "|    |Sex Male    |-2.000   |1.918     |0.305   |              |",
      "|    |Group Low   |-1.182   |2.303     |0.611   |              |",
      "|    |Group Med   |-0.545   |2.290     |0.813   |              |",
      "|    |(Intercept) |41.277   |2.281     |< 0.001 |-0.087        |",
      "|    |1x          |-0.019   |0.113     |0.870   |              |",
      "|    |Group Low   |0.237    |7.336     |0.974   |              |",
      "|    |Group Med   |-0.083   |4.085     |0.984   |              |",
      "|    |(Intercept) |41.000   |1.543     |< 0.001 |-0.087        |",
      "|    |2x B        |-0.667   |3.495     |0.850   |              |",
      "|    |Group Low   |-0.242   |4.198     |0.954   |              |",
      "|    |Group Med   |-0.333   |2.817     |0.907   |              |",
      "|B   |(Intercept) |38.733   |1.470     |< 0.001 |-0.050        |",
      "|    |Sex Male    |0.908    |1.407     |0.522   |              |",
      "|    |Group Low   |0.205    |1.753     |0.907   |              |",
      "|    |Group Med   |-0.317   |1.753     |0.857   |              |",
      "|    |(Intercept) |38.753   |1.835     |< 0.001 |-0.056        |",
      "|    |1x          |0.027    |0.081     |0.739   |              |",
      "|    |Group Low   |-1.414   |5.083     |0.782   |              |",
      "|    |Group Med   |-1.105   |2.996     |0.714   |              |",
      "|    |(Intercept) |39.187   |1.296     |< 0.001 |-0.058        |",
      "|    |2x B        |0.011    |2.383     |0.996   |              |",
      "|    |Group Low   |0.170    |2.962     |0.955   |              |",
      "|    |Group Med   |-0.299   |2.161     |0.891   |              |",
      ""                                                                ,
      ""                                                                ,
      "|trt |time        |estimate |std.error |p.value |adj.r.squared |",
      "|:---|:-----------|:--------|:---------|:-------|:-------------|",
      "|A   |(Intercept) |4.469    |0.568     |< 0.001 |-0.016        |",
      "|    |Sex Male    |-0.223   |0.606     |0.716   |              |",
      "|    |Group Low   |-1.115   |0.728     |0.136   |              |",
      "|    |Group Med   |-0.620   |0.724     |0.398   |              |",
      "|    |(Intercept) |5.331    |0.671     |< 0.001 |0.090         |",
      "|    |1x          |-0.066   |0.033     |0.058   |              |",
      "|    |Group Low   |2.947    |2.159     |0.182   |              |",
      "|    |Group Med   |1.316    |1.202     |0.282   |              |",
      "|    |(Intercept) |4.357    |0.475     |< 0.001 |0.004         |",
      "|    |2x B        |-0.967   |1.076     |0.376   |              |",
      "|    |Group Low   |-0.118   |1.292     |0.928   |              |",
      "|    |Group Med   |-0.190   |0.867     |0.828   |              |",
      "|B   |(Intercept) |4.301    |0.591     |< 0.001 |0.086         |",
      "|    |Sex Male    |0.898    |0.566     |0.119   |              |",
      "|    |Group Low   |-1.621   |0.705     |0.026   |              |",
      "|    |Group Med   |-0.879   |0.705     |0.218   |              |",
      "|    |(Intercept) |3.710    |0.725     |< 0.001 |0.112         |",
      "|    |1x          |0.065    |0.032     |0.048   |              |",
      "|    |Group Low   |-5.461   |2.008     |0.009   |              |",
      "|    |Group Med   |-2.797   |1.184     |0.022   |              |",
      "|    |(Intercept) |4.750    |0.516     |< 0.001 |0.098         |",
      "|    |2x B        |1.700    |0.948     |0.079   |              |",
      "|    |Group Low   |-3.345   |1.179     |0.007   |              |",
      "|    |Group Med   |-1.750   |0.860     |0.047   |              |"
    )
  )
})

###########################################################################################################
#### Reported bugs for modelsum
###########################################################################################################

test_that("02/23/2018: wrapping long labels (#59)", {
  labs <- list(
    Group = "This is a really long label for the Group variable",
    time = "Another really long label. Can you believe how long this is",
    dt = "ThisLabelHasNoSpacesSoLetsSeeHowItBehaves",
    trt = NULL
  )
  expect_identical(
    capture.kable(print(summary(modelsum(Age ~ Group + time + dt, strata = trt, data = set_labels(mdat, labs)), text = TRUE, digits = 4), width = 30)),
    c("|trt |                               |estimate |std.error |p.value |adj.r.squared |",
      "|:---|:------------------------------|:--------|:---------|:-------|:-------------|",
      "|A   |(Intercept)                    |41.0000  |1.5200    |< 0.001 |-0.055        |",
      "|    |This is a really long label    |-0.9091  |2.2915    |0.694   |              |",
      "|    |for the Group variable Low     |         |          |        |              |",
      "|    |This is a really long label    |-0.6364  |2.2915    |0.783   |              |",
      "|    |for the Group variable Med     |         |          |        |              |",
      "|    |(Intercept)                    |43.2848  |2.1872    |< 0.001 |0.026         |",
      "|    |Another really long label.     |-0.7192  |0.5187    |0.175   |              |",
      "|    |Can you believe how long this  |         |          |        |              |",
      "|    |is                             |         |          |        |              |",
      "|    |(Intercept)                    |36.3141  |2.9357    |< 0.001 |0.035         |",
      "|    |ThisLabelHasNoSpacesSoLetsSeeH |-0.0006  |0.0004    |0.141   |              |",
      "|    |owItBehaves                    |         |          |        |              |",
      "|B   |(Intercept)                    |39.1875  |1.2837    |< 0.001 |-0.038        |",
      "|    |This is a really long label    |0.1809   |1.7423    |0.918   |              |",
      "|    |for the Group variable Low     |         |          |        |              |",
      "|    |This is a really long label    |-0.2928  |1.7423    |0.867   |              |",
      "|    |for the Group variable Med     |         |          |        |              |",
      "|    |(Intercept)                    |39.9678  |1.4192    |< 0.001 |-0.011        |",
      "|    |Another really long label.     |-0.2118  |0.3205    |0.512   |              |",
      "|    |Can you believe how long this  |         |          |        |              |",
      "|    |is                             |         |          |        |              |",
      "|    |(Intercept)                    |46.9878  |2.5356    |< 0.001 |0.148         |",
      "|    |ThisLabelHasNoSpacesSoLetsSeeH |0.0011   |0.0003    |0.002   |              |",
      "|    |owItBehaves                    |         |          |        |              |"
    )
  )
})


