## Tests for modelsum


context("Testing the modelsum output")

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

test_that("A basic modelsum call--no labels, no missings", {
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + time, data = mdat), text = TRUE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "(Intercept)        39.8            0.779           <0.001          -0.011         ",
      "Sex Male           -0.258          1.11            0.818           .              ",
      "(Intercept)        41.1            1.2             <0.001          0.009          ",
      "time               -0.371          0.275           0.182           .              ",
      "----------------------------------------------------------------------------------"
    )
  )
})

test_that("A basic modelsum tableby call--labels, no missings", {
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + trt, data = mdat), text = TRUE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "(Intercept)        39.8            0.779           <0.001          -0.011         ",
      "Sex Male           -0.258          1.11            0.818           .              ",
      "(Intercept)        40.5            0.874           <0.001          0.006          ",
      "Treatment Arm B    -1.4            1.13            0.225           .              ",
      "----------------------------------------------------------------------------------"
    )
  )
})

test_that("A basic modelsum call--adding adjustment", {
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "(Intercept)        40.6            1.02            <0.001          -0.005         ",
      "Sex Male           -0.221          1.11            0.843           .              ",
      "Treatment Arm B    -1.4            1.13            0.229           .              ",
      "(Intercept)        41.9            1.37            <0.001          0.014          ",
      "time               -0.368          0.275           0.184           .              ",
      "Treatment Arm B    -1.4            1.12            0.227           .              ",
      "----------------------------------------------------------------------------------"
    )
  )
})

test_that("A basic modelsum call--suppressing intercept and/or adjustment vars", {
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.intercept = FALSE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "Sex Male           -0.221          1.11            0.843           -0.005         ",
      "Treatment Arm B    -1.4            1.13            0.229           .              ",
      "time               -0.368          0.275           0.184           0.014          ",
      "Treatment Arm B    -1.4            1.12            0.227           .              ",
      "----------------------------------------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.adjust = FALSE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "(Intercept)        40.6            1.02            <0.001          -0.005         ",
      "Sex Male           -0.221          1.11            0.843           .              ",
      "(Intercept)        41.9            1.37            <0.001          0.014          ",
      "time               -0.368          0.275           0.184           .              ",
      "----------------------------------------------------------------------------------"
    )
  )
  expect_identical(
    capture.output(summary(modelsum(Age ~ Sex + time, adjust = ~ trt, data = mdat), text = TRUE, show.intercept = FALSE, show.adjust = FALSE)),
    c("----------------------------------------------------------------------------------",
      "                    estimate        std.error       p.value         adj.r.squared ",
      "------------------ --------------- --------------- --------------- ---------------",
      "Sex Male           -0.221          1.11            0.843           -0.005         ",
      "time               -0.368          0.275           0.184           0.014          ",
      "----------------------------------------------------------------------------------"
    )
  )
})
