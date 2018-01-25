## Tests for keep.labels


context("Testing the keep.labels function")


df <- data.frame(z = c("A", "A", "A", "B", "B", "B"),
                 x = c("C", "C", "D", "C", "D", "D"),
                 y = c(1, 2, 1, 2, 1, 2),
                 stringsAsFactors = FALSE)
attr(df$x, "label") <- "My var"
attr(df$y, "label") <- "YYYY"
attr(df, "label") <- "Data Frame Label"
df.kl <- keep.labels(df)


###########################################################################################################
#### Basic keep.labels stuff
###########################################################################################################

test_that("keep.labels actually keeps labels on data.frame subsetting", {
  expect_identical(labels(df.kl$y), "YYYY")
  expect_identical(labels(df.kl[["y"]]), "YYYY")
  expect_identical(labels(df.kl["y"]), list(y = "YYYY"))
  expect_identical(labels(df.kl[, "y", drop = FALSE]), list(y = "YYYY"))
  expect_identical(labels(df.kl[, "y", drop = TRUE]), "YYYY")
  expect_identical(labels(df.kl[1:5, "y", drop = TRUE]), "YYYY")
  expect_identical(labels(df.kl[1:5, ]), list(z = NULL, x = "My var", y = "YYYY"))
})

empty.df <- data.frame()

test_that("labels<-.data.frame works", {
  expect_error(labels(df.kl) <- list(y = NULL, z = "ZZZZ", a = "a"), NA)
  expect_identical(labels(df.kl), list(z = "ZZZZ", x = "My var", y = NULL))

  expect_error(labels(df.kl) <- NULL, NA)
  expect_identical(labels(df.kl), list(z = NULL, x = NULL, y = NULL))

  expect_error(labels(empty.df) <- list(z = "ZZZZ"), NA)
  expect_true(length(labels(empty.df)) == 0)

})


###########################################################################################################
#### keep.labels with arsenal functions
###########################################################################################################

data(mockstudy)

test_that("Keep labels in tableby", {
  expect_identical(
    capture.output(summary(tableby(sex ~ fe(arm) + age, data = mockstudy, subset = age < 80), text = TRUE)),
    c(""                                                                            ,
      ""                                                                            ,
      "|              |Male (N=891)    |Female (N=571)  |Total (N=1462)  | p value|",
      "|:-------------|:---------------|:---------------|:---------------|-------:|",
      "|Treatment Arm |                |                |                |   0.187|",
      "|-  A: IFL     |272 (30.5%)     |149 (26.1%)     |421 (28.8%)     |        |",
      "|-  F: FOLFOX  |397 (44.6%)     |273 (47.8%)     |670 (45.8%)     |        |",
      "|-  G: IROX    |222 (24.9%)     |149 (26.1%)     |371 (25.4%)     |        |",
      "|Age in Years  |                |                |                |   0.071|",
      "|-  Mean (SD)  |59.850 (10.924) |58.774 (11.372) |59.430 (11.110) |        |",
      "|-  Q1, Q3     |53.000, 68.000  |52.000, 68.000  |52.000, 68.000  |        |",
      "|-  Range      |19.000 - 79.000 |22.000 - 79.000 |19.000 - 79.000 |        |"
    )
  )
})

test_that("Keep labels in modelsum", {
  expect_identical(
    capture.output(summary(modelsum(age ~ sex + arm, data = mockstudy, subset = age < 80), text = TRUE)),
    c(""                                                                       ,
      ""                                                                       ,
      "|                        |estimate |std.error |p.value |adj.r.squared |",
      "|:-----------------------|:--------|:---------|:-------|:-------------|",
      "|(Intercept)             |59.850   |0.372     |< 0.001 |0.002         |",
      "|sex Female              |-1.076   |0.595     |0.071   |              |",
      "|(Intercept)             |59.290   |0.542     |< 0.001 |-0.001        |",
      "|Treatment Arm F: FOLFOX |0.340    |0.691     |0.623   |              |",
      "|Treatment Arm G: IROX   |-0.063   |0.792     |0.936   |              |"
    )
  )
})





