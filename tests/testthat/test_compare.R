## Tests for formulize


context("Testing the compare output")

data(mockstudy)
mockstudy2 <- muck_up_mockstudy()

# a far simpler example

df1 <- data.frame(id = paste0("person", 1:3), a = c("a", "b", "c"), b = c(1, 3, 4), c = c("f", "e", "d"),
                  row.names = paste0("rn", 1:3), stringsAsFactors = FALSE)
df2 <- data.frame(id = paste0("person", 3:1), a = c("c", "b", "a"), b = c(1, 3, 4), d = paste0("rn", 1:3),
                  row.names = paste0("rn", c(1,3,2)), stringsAsFactors = FALSE)


###########################################################################################################
#### Just test that it ran right
###########################################################################################################

test_that("Basic comparison works: by row", {
  expect_identical(
    capture.output(print(compare(df1, df2))),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "compare.data.frame(x = df1, y = df2)"               ,
      ""                                                   ,
      "Shared: 4 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 2/3 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(compare(df1, df1)) == 0)
})

test_that("Basic comparison works: by id", {
  expect_identical(
    capture.output(print(compare(df1, df2, by = "id"))),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "compare.data.frame(x = df1, y = df2, by = \"id\")"  ,
      ""                                                   ,
      "Shared: 3 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 1/2 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(compare(df1, df1, by = "id")) == 0)
})

test_that("Basic comparison works: by row.names", {
  expect_identical(
    capture.output(print(compare(df1, df2, by = "row.names"))),
    c("Compare Object"                                          ,
      ""                                                        ,
      "Function Call: "                                         ,
      "compare.data.frame(x = df1, y = df2, by = \"row.names\")",
      ""                                                        ,
      "Shared: 4 variables and 3 observations."                 ,
      "Not shared: 2 variables and 0 observations."             ,
      ""                                                        ,
      "Differences found in 3/3 variables compared."            ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(compare(df1, df1, by = "row.names")) == 0)
})

test_that("Basic comparison works: by row.names for x and something else for y", {
  expect_identical(
    capture.output(print(compare(df1, df2, by.x = "row.names", by.y = "d"))),
    c("Compare Object"                                                          ,
      ""                                                                        ,
      "Function Call: "                                                         ,
      "compare.data.frame(x = df1, y = df2, by.x = \"row.names\", by.y = \"d\")",
      ""                                                                        ,
      "Shared: 4 variables and 3 observations."                                 ,
      "Not shared: 1 variables and 0 observations."                             ,
      ""                                                                        ,
      "Differences found in 2/3 variables compared."                            ,
      "0 variables compared have non-identical attributes."
    )
  )
})

test_that("Basic mockstudy comparison works: by id", {
  expect_identical(
    capture.output(print(compare(mockstudy, mockstudy2, by = 'case'))),
    c("Compare Object"                                                  ,
      ""                                                                ,
      "Function Call: "                                                 ,
      "compare.data.frame(x = mockstudy, y = mockstudy2, by = \"case\")",
      ""                                                                ,
      "Shared: 10 variables and 1495 observations."                     ,
      "Not shared: 7 variables and 4 observations."                     ,
      ""                                                                ,
      "Differences found in 3/7 variables compared."                    ,
      "3 variables compared have non-identical attributes."
    )
  )
})

###########################################################################################################
#### Check for certain errors
###########################################################################################################

test_that("Different by-variables with overlap with non-by-variables throws an error", {
  expect_error(compare(df1, df2, by.x = "id", by.y = "b"), "non-by-variables")
  expect_error(compare(df1, df2, by.x = "c", by.y = "id"), NA)
})

test_that("Using forbidden names throws an error", {
  expect_error(compare(df1, cbind(df2, ..row.names.. = 1:3)), "reserved colnames")
})

###########################################################################################################
#### Using comparison.control
###########################################################################################################

test_that("tol.vars is working correctly", {
  expect_identical(
    capture.output(print(compare(mockstudy, mockstudy2, by = 'case', tol.vars = "._ "))),
    c("Compare Object"                                                   ,
      ""                                                                 ,
      "Function Call: "                                                  ,
      "compare.data.frame(x = mockstudy, y = mockstudy2, by = \"case\", ",
      "    tol.vars = \"._ \")"                                          ,
      ""                                                                 ,
      "Shared: 12 variables and 1495 observations."                      ,
      "Not shared: 3 variables and 4 observations."                      ,
      ""                                                                 ,
      "Differences found in 3/9 variables compared."                     ,
      "3 variables compared have non-identical attributes."
    )
  )

  expect_identical(
    capture.output(print(compare(mockstudy, mockstudy2, by = 'case', tol.vars = c("._ ", "case")))),
    c("Compare Object"                                                   ,
      ""                                                                 ,
      "Function Call: "                                                  ,
      "compare.data.frame(x = mockstudy, y = mockstudy2, by = \"case\", ",
      "    tol.vars = c(\"._ \", \"case\"))"                             ,
      ""                                                                 ,
      "Shared: 13 variables and 1495 observations."                      ,
      "Not shared: 1 variables and 4 observations."                      ,
      ""                                                                 ,
      "Differences found in 3/10 variables compared."                    ,
      "4 variables compared have non-identical attributes."
    )
  )
})

test_that("tolerances are working correctly", {

  tmp <- compare(mockstudy, mockstudy2, by = "case", tol.vars = c("._ ", "case"),
                 int.as.num = TRUE, tol.num = 10,
                 tol.factor = "labels", factor.as.char = TRUE, tol.char = "case")

  expect_identical(
    capture.output(print(tmp)),
    c("Compare Object"                                                          ,
      ""                                                                        ,
      "Function Call: "                                                         ,
      "compare.data.frame(x = mockstudy, y = mockstudy2, by = \"case\", "       ,
      "    tol.vars = c(\"._ \", \"case\"), int.as.num = TRUE, tol.num = 10, "  ,
      "    tol.factor = \"labels\", factor.as.char = TRUE, tol.char = \"case\")",
      ""                                                                        ,
      "Shared: 13 variables and 1495 observations."                             ,
      "Not shared: 1 variables and 4 observations."                             ,
      ""                                                                        ,
      "Differences found in 3/12 variables compared."                           ,
      "4 variables compared have non-identical attributes."
    )
  )
})

###########################################################################################################
#### Using helper functions
###########################################################################################################

test_that("helper functions are working correctly", {
  expect_true(n.diffs(compare(df1, df2, by = "id")) == n.diffs(summary(compare(df1, df2, by = "id"))))
  expect_identical(diffs(compare(df1, df2, by = "id")), diffs(summary(compare(df1, df2, by = "id"))))

})

