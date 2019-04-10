## Tests for formulize


context("Testing the comparedf output")

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
    capture.output(comparedf(df1, df2)),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "comparedf(x = df1, y = df2)"               ,
      ""                                                   ,
      "Shared: 4 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 2/3 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(comparedf(df1, df1)) == 0)
})

test_that("Basic comparison works: by id", {
  expect_identical(
    capture.output(comparedf(df1, df2, by = "id")),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "comparedf(x = df1, y = df2, by = \"id\")"  ,
      ""                                                   ,
      "Shared: 3 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 1/2 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(comparedf(df1, df1, by = "id")) == 0)
})

test_that("Basic comparison works: by row.names", {
  expect_identical(
    capture.output(comparedf(df1, df2, by = "row.names")),
    c("Compare Object"                                          ,
      ""                                                        ,
      "Function Call: "                                         ,
      "comparedf(x = df1, y = df2, by = \"row.names\")",
      ""                                                        ,
      "Shared: 4 variables and 3 observations."                 ,
      "Not shared: 2 variables and 0 observations."             ,
      ""                                                        ,
      "Differences found in 3/3 variables compared."            ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(comparedf(df1, df1, by = "row.names")) == 0)
})

test_that("Basic comparison works: by row.names for x and something else for y", {
  expect_identical(
    capture.output(comparedf(df1, df2, by.x = "row.names", by.y = "d")),
    c("Compare Object"                                                          ,
      ""                                                                        ,
      "Function Call: "                                                         ,
      "comparedf(x = df1, y = df2, by.x = \"row.names\", by.y = \"d\")",
      ""                                                                        ,
      "Shared: 4 variables and 3 observations."                                 ,
      "Not shared: 1 variables and 0 observations."                             ,
      ""                                                                        ,
      "Differences found in 2/3 variables compared."                            ,
      "0 variables compared have non-identical attributes."
    )
  )
})

df1$listcol <- list(1:3, "hi there", FALSE)
df2$listcol <- list(FALSE, "bye now", 1:2)

test_that("List-column comparison works: by id", {
  expect_identical(
    capture.output(comparedf(df1, df2, by = "id")),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "comparedf(x = df1, y = df2, by = \"id\")"  ,
      ""                                                   ,
      "Shared: 4 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 2/3 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(comparedf(df1, df1, by = "id")) == 0)
})

df1$testdate <- as.Date(c("2017-07-09", "2017-08-08", "2017-09-07"))
df2$testdate <- as.Date(c("2017-09-07", "2017-08-08", "2017-09-07"))

test_that("Dates comparison works: by id", {
  expect_identical(
    capture.output(comparedf(df1, df2, by = "id")),
    c("Compare Object"                                     ,
      ""                                                   ,
      "Function Call: "                                    ,
      "comparedf(x = df1, y = df2, by = \"id\")"  ,
      ""                                                   ,
      "Shared: 5 variables and 3 observations."            ,
      "Not shared: 2 variables and 0 observations."        ,
      ""                                                   ,
      "Differences found in 3/4 variables compared."       ,
      "0 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diffs(comparedf(df1, df1, by = "id")) == 0)
})

test_that("Basic mockstudy comparison works: by id", {
  expect_identical(
    capture.output(comparedf(mockstudy, mockstudy2, by = 'case')),
    c("Compare Object"                                                  ,
      ""                                                                ,
      "Function Call: "                                                 ,
      "comparedf(x = mockstudy, y = mockstudy2, by = \"case\")",
      ""                                                                ,
      "Shared: 10 variables and 1495 observations."                     ,
      "Not shared: 7 variables and 4 observations."                     ,
      ""                                                                ,
      "Differences found in 3/7 variables compared."                    ,
      "3 variables compared have non-identical attributes."
    )
  )
})


test_that("Comparison with empty data.frames works", {
  mck1 <- mockstudy[0, , drop = FALSE]
  mck2 <- mockstudy2[0, , drop = FALSE]
  expect_identical(
    capture.output(comparedf(mockstudy, mck2, by = 'case')),
    c("Compare Object"                                            ,
      ""                                                          ,
      "Function Call: "                                           ,
      "comparedf(x = mockstudy, y = mck2, by = \"case\")",
      ""                                                          ,
      "Shared: 10 variables and 0 observations."                  ,
      "Not shared: 7 variables and 1499 observations."            ,
      ""                                                          ,
      "Differences found in 0/7 variables compared."              ,
      "3 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diff.obs(comparedf(mockstudy, mck2, by = "case")) == 1499)
  expect_true(n.diffs(comparedf(mockstudy, mck2, by = "case")) == 0)

  expect_identical(
    capture.output(comparedf(mockstudy, mck2, by = 'row.names')),
    c("Compare Object"                                                 ,
      ""                                                               ,
      "Function Call: "                                                ,
      "comparedf(x = mockstudy, y = mck2, by = \"row.names\")",
      ""                                                               ,
      "Shared: 11 variables and 0 observations."                       ,
      "Not shared: 7 variables and 1499 observations."                 ,
      ""                                                               ,
      "Differences found in 0/8 variables compared."                   ,
      "3 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diff.obs(comparedf(mockstudy, mck2, by = "row.names")) == 1499)
  expect_true(n.diffs(comparedf(mockstudy, mck2, by = "row.names")) == 0)

  expect_identical(
    capture.output(comparedf(mck1, mck2, by = "case")),
    c("Compare Object"                                       ,
      ""                                                     ,
      "Function Call: "                                      ,
      "comparedf(x = mck1, y = mck2, by = \"case\")",
      ""                                                     ,
      "Shared: 10 variables and 0 observations."             ,
      "Not shared: 7 variables and 0 observations."          ,
      ""                                                     ,
      "Differences found in 0/7 variables compared."         ,
      "2 variables compared have non-identical attributes."
    )
  )
  expect_true(n.diff.obs(comparedf(mck1, mck2, by = "case")) == 0)
  expect_true(n.diffs(comparedf(mck1, mck2, by = "case")) == 0)
})

###########################################################################################################
#### Check for certain errors
###########################################################################################################

test_that("Different by-variables with overlap with non-by-variables throws an error", {
  expect_error(comparedf(df1, df2, by.x = "id", by.y = "b"), "non-by-variables")
  expect_error(comparedf(df1, df2, by.x = "c", by.y = "id"), NA)
})

test_that("Using forbidden names throws an error", {
  expect_error(comparedf(df1, cbind(df2, ..row.names.. = 1:3)), "reserved colnames")
})

###########################################################################################################
#### Using comparedf.control
###########################################################################################################

test_that("tol.vars is working correctly", {
  expect_identical(
    capture.output(comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = "._ ")),
    c("Compare Object"                                                   ,
      ""                                                                 ,
      "Function Call: "                                                  ,
      "comparedf(x = mockstudy, y = mockstudy2, by = \"case\", tol.vars = \"._ \")",
      ""                                                                 ,
      "Shared: 12 variables and 1495 observations."                      ,
      "Not shared: 3 variables and 4 observations."                      ,
      ""                                                                 ,
      "Differences found in 3/9 variables compared."                     ,
      "3 variables compared have non-identical attributes."
    )
  )

  expect_identical(
    capture.output(comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = c("._ ", "case"))),
    c("Compare Object"                                                   ,
      ""                                                                 ,
      "Function Call: "                                                  ,
      "comparedf(x = mockstudy, y = mockstudy2, by = \"case\", tol.vars = c(\"._ \", ",
      "    \"case\"))"                                                   ,
      ""                                                                 ,
      "Shared: 13 variables and 1495 observations."                      ,
      "Not shared: 1 variables and 4 observations."                      ,
      ""                                                                 ,
      "Differences found in 3/10 variables compared."                    ,
      "4 variables compared have non-identical attributes."
    )
  )

  tolvars <- c(arm = "Arm", fu.time = "fu_time", fu.stat = "fu stat")
  expect_identical(
    comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = tolvars)[1:2],
    comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = c("._ ", "case"))[1:2]
  )
  expect_warning(comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = c("hi" = "Arm")),
                 "'hi' not found in colnames of x")
  expect_warning(comparedf(mockstudy, mockstudy2, by = 'case', tol.vars = c(arm = "hi")),
                 "'hi' not found in colnames of y")
})


tmp <- comparedf(mockstudy, mockstudy2, by = "case", tol.vars = c("._ ", "case"),
                 int.as.num = TRUE, tol.num.val = 10,
                 tol.factor = "labels", factor.as.char = TRUE, tol.char = "case")

test_that("tolerances are working correctly", {
  expect_identical(
    capture.output(tmp),
    c("Compare Object"                                                            ,
      ""                                                                          ,
      "Function Call: "                                                           ,
      "comparedf(x = mockstudy, y = mockstudy2, by = \"case\", tol.vars = c(\"._ \", ",
      "    \"case\"), int.as.num = TRUE, tol.num.val = 10, tol.factor = \"labels\", " ,
      "    factor.as.char = TRUE, tol.char = \"case\")"                           ,
      ""                                                                          ,
      "Shared: 13 variables and 1495 observations."                               ,
      "Not shared: 1 variables and 4 observations."                               ,
      ""                                                                          ,
      "Differences found in 3/12 variables compared."                             ,
      "4 variables compared have non-identical attributes."
    )
  )
})

tol.minus9 <- function(x, y, tol)
{
  idx1 <- is.na(x) & !is.na(y) & y == -9
  idx2 <- tol.num.absolute(x, y, tol) # find other absolute differences
  return(!idx1 & idx2)
}

tmp2 <- comparedf(mockstudy, mockstudy2, by = "case",
                  tol.vars = c("._ ", "case"), # dots=underscores=spaces, ignore case
                  int.as.num = TRUE,           # compare integers and numerics
                  tol.num.val = 10,            # allow absolute differences <= 10
                  tol.factor = "labels",       # match only factor labels
                  factor.as.char = TRUE,       # compare factors and characters
                  tol.char = "case",           # ignore case in character vectors
                  tol.num = tol.minus9         # ignore NA -> -9 changes
)

test_that("custom tolerances are working correctly", {
  expect_identical(
    capture.output(tmp2),
    c("Compare Object"                                                            ,
      ""                                                                          ,
      "Function Call: "                                                           ,
      "comparedf(x = mockstudy, y = mockstudy2, by = \"case\", tol.vars = c(\"._ \", ",
      "    \"case\"), int.as.num = TRUE, tol.num.val = 10, tol.factor = \"labels\", " ,
      "    factor.as.char = TRUE, tol.char = \"case\", tol.num = tol.minus9)"     ,
      ""                                                                          ,
      "Shared: 13 variables and 1495 observations."                               ,
      "Not shared: 1 variables and 4 observations."                               ,
      ""                                                                          ,
      "Differences found in 2/12 variables compared."                             ,
      "4 variables compared have non-identical attributes."
    )
  )
})

###########################################################################################################
#### Using helper functions
###########################################################################################################

test_that("helper functions are working correctly", {

  expect_true(n.diffs(comparedf(mockstudy, mockstudy2, by = "case")) ==
                n.diffs(summary(comparedf(mockstudy, mockstudy2, by = "case"))))

  expect_true(n.diffs(comparedf(df1, df2, by = "id")) == n.diffs(summary(comparedf(df1, df2, by = "id"))))
  expect_identical(diffs(comparedf(df1, df2, by = "id")), diffs(summary(comparedf(df1, df2, by = "id"))))
  expect_identical(diffs(comparedf(df1, df2, by = "id"), by.var = TRUE), diffs(summary(comparedf(df1, df2, by = "id")), by.var = TRUE))

  expect_identical(diffs(comparedf(df1, df2, by = "id"), vars = "a"), diffs(summary(comparedf(df1, df2, by = "id")), vars = "a"))
  expect_identical(diffs(comparedf(df1, df2, by = "id"), vars = "b"), diffs(summary(comparedf(df1, df2, by = "id")), vars = "b"))
  expect_identical(diffs(comparedf(df1, df2, by = "id"), vars = "a", by.var = TRUE), diffs(summary(comparedf(df1, df2, by = "id")), vars = "a", by.var = TRUE))

})

###########################################################################################################
#### Summary output
###########################################################################################################

test_that("Summary output looks right (i.e. for factors)", {
  expect_identical(
    capture.kable(summary(comparedf(mockstudy, mockstudy2, by = "case"))),
    c("Table: Summary of data.frames"                                                 ,
      ""                                                                              ,
      "version   arg           ncol   nrow"                                           ,
      "--------  -----------  -----  -----"                                           ,
      "x         mockstudy       14   1499"                                           ,
      "y         mockstudy2      13   1495"                                           ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: Variables not shared"                                                   ,
      ""                                                                              ,
      "version   variable    position  class     "                                    ,
      "--------  ---------  ---------  ----------"                                    ,
      "x         age                2  integer   "                                    ,
      "x         arm                3  character "                                    ,
      "x         fu.time            6  integer   "                                    ,
      "x         fu.stat            7  integer   "                                    ,
      "y         fu_time           11  integer   "                                    ,
      "y         fu stat           12  integer   "                                    ,
      "y         Arm               13  character "                                    ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: Other variables not compared"                                           ,
      ""                                                                              ,
      "var.x    pos.x  class.x     var.y    pos.y  class.y "                          ,
      "------  ------  ----------  ------  ------  --------"                          ,
      "race         5  character   race         3  factor  "                          ,
      "ast         12  integer     ast          8  numeric "                          ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: Observations not shared"                                                ,
      ""                                                                              ,
      "version      case   observation"                                               ,
      "--------  -------  ------------"                                               ,
      "x           88989             9"                                               ,
      "x           90158             8"                                               ,
      "x           99508             7"                                               ,
      "x          112263             5"                                               ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: Differences detected by variable"                                       ,
      ""                                                                              ,
      "var.x         var.y             n   NAs"                                       ,
      "------------  ------------  -----  ----"                                       ,
      "sex           sex            1495     0"                                       ,
      "ps            ps                1     1"                                       ,
      "hgb           hgb             266   266"                                       ,
      "bmi           bmi               0     0"                                       ,
      "alk.phos      alk.phos          0     0"                                       ,
      "mdquality.s   mdquality.s       0     0"                                       ,
      "age.ord       age.ord           0     0"                                       ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: First 10 differences detected per variable (1741 differences not shown)",
      ""                                                                              ,
      "var.x   var.y     case  values.x   values.y    row.x   row.y"                  ,
      "------  ------  ------  ---------  ---------  ------  ------"                  ,
      "sex     sex      76170  Male       Male           26      20"                  ,
      "sex     sex      76240  Male       Male           27      21"                  ,
      "sex     sex      76431  Female     Female         28      22"                  ,
      "sex     sex      76712  Male       Male           29      23"                  ,
      "sex     sex      76780  Female     Female         30      24"                  ,
      "sex     sex      77066  Female     Female         31      25"                  ,
      "sex     sex      77316  Male       Male           32      26"                  ,
      "sex     sex      77355  Male       Male           33      27"                  ,
      "sex     sex      77591  Male       Male           34      28"                  ,
      "sex     sex      77851  Male       Male           35      29"                  ,
      "ps      ps       86205  0          NA              6       3"                  ,
      "hgb     hgb      88714  NA         -9            192     186"                  ,
      "hgb     hgb      88955  NA         -9            204     198"                  ,
      "hgb     hgb      89549  NA         -9            229     223"                  ,
      "hgb     hgb      89563  NA         -9            231     225"                  ,
      "hgb     hgb      89584  NA         -9            237     231"                  ,
      "hgb     hgb      89591  NA         -9            238     232"                  ,
      "hgb     hgb      89595  NA         -9            239     233"                  ,
      "hgb     hgb      89647  NA         -9            243     237"                  ,
      "hgb     hgb      89665  NA         -9            244     238"                  ,
      "hgb     hgb      89827  NA         -9            255     249"                  ,
      ""                                                                              ,
      ""                                                                              ,
      ""                                                                              ,
      "Table: Non-identical attributes"                                               ,
      ""                                                                              ,
      "var.x   var.y   name   "                                                       ,
      "------  ------  -------"                                                       ,
      "sex     sex     label  "                                                       ,
      "sex     sex     levels "                                                       ,
      "race    race    class  "                                                       ,
      "race    race    label  "                                                       ,
      "race    race    levels "                                                       ,
      "bmi     bmi     label  "
    )
  )
})


test_that("Summary output with attributes and max.print options", {
  expect_identical(
    capture.kable(summary(comparedf(mockstudy, mockstudy2, by = "case"), show.attrs = TRUE,
                          max.print.vars = 2, max.print.obs = 3, max.print.diff = 3, max.print.attrs = 3)),
    c("Table: Summary of data.frames"                                                ,
      ""                                                                             ,
      "version   arg           ncol   nrow"                                          ,
      "--------  -----------  -----  -----"                                          ,
      "x         mockstudy       14   1499"                                          ,
      "y         mockstudy2      13   1495"                                          ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: Variables not shared (5 differences not shown)"                        ,
      ""                                                                             ,
      "version   variable    position  class     "                                   ,
      "--------  ---------  ---------  ----------"                                   ,
      "x         age                2  integer   "                                   ,
      "x         arm                3  character "                                   ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: Other variables not compared"                                          ,
      ""                                                                             ,
      "var.x    pos.x  class.x     var.y    pos.y  class.y "                         ,
      "------  ------  ----------  ------  ------  --------"                         ,
      "race         5  character   race         3  factor  "                         ,
      "ast         12  integer     ast          8  numeric "                         ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: Observations not shared (1 differences not shown)"                     ,
      ""                                                                             ,
      "version     case   observation"                                               ,
      "--------  ------  ------------"                                               ,
      "x          88989             9"                                               ,
      "x          90158             8"                                               ,
      "x          99508             7"                                               ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: Differences detected by variable"                                      ,
      ""                                                                             ,
      "var.x         var.y             n   NAs"                                      ,
      "------------  ------------  -----  ----"                                      ,
      "sex           sex            1495     0"                                      ,
      "ps            ps                1     1"                                      ,
      "hgb           hgb             266   266"                                      ,
      "bmi           bmi               0     0"                                      ,
      "alk.phos      alk.phos          0     0"                                      ,
      "mdquality.s   mdquality.s       0     0"                                      ,
      "age.ord       age.ord           0     0"                                      ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: First 3 differences detected per variable (1755 differences not shown)",
      ""                                                                             ,
      "var.x   var.y     case  values.x   values.y    row.x   row.y"                 ,
      "------  ------  ------  ---------  ---------  ------  ------"                 ,
      "sex     sex      76170  Male       Male           26      20"                 ,
      "sex     sex      76240  Male       Male           27      21"                 ,
      "sex     sex      76431  Female     Female         28      22"                 ,
      "ps      ps       86205  0          NA              6       3"                 ,
      "hgb     hgb      88714  NA         -9            192     186"                 ,
      "hgb     hgb      88955  NA         -9            204     198"                 ,
      "hgb     hgb      89549  NA         -9            229     223"                 ,
      ""                                                                             ,
      ""                                                                             ,
      ""                                                                             ,
      "Table: Non-identical attributes (3 differences not shown)"                    ,
      ""                                                                             ,
      "var.x   var.y   name     attr.x                attr.y              "          ,
      "------  ------  -------  --------------------  --------------------"          ,
      "sex     sex     label    NA                    Sex (M/F)           "          ,
      "sex     sex     levels   c(\"Male\", \"Female\")   c(\"Female\", \"Male\") "  ,
      "race    race    class    NA                    factor              "
    )
  )
})


###########################################################################################################
#### Reported bugs for comparedf
###########################################################################################################

test_that("2019/04/09: Percent tolerances work when everything is zero (#206)", {
  expect_true(n.diffs(comparedf(data.frame(x = 0), data.frame(x = 0), tol.num = "percent")) == 0)
})

test_that("2019/04/10: summary breaks when no variables were compared (#207)", {

  check_it <- function(x, y, by = NULL, n = 0)
  {
    tmp <- diffs(comparedf(x, y, by = by))
    expect_true(nrow(tmp) == n)
    expect_identical(names(tmp), c("var.x", "var.y", if(is.null(by)) "..row.names.." else by, "values.x", "values.y", "row.x", "row.y"))
    expect_true(inherits(tmp$values.x, "AsIs"))
    expect_true(inherits(tmp$values.y, "AsIs"))
    expect_true(is.integer(tmp$row.x))
    expect_true(is.integer(tmp$row.y))
  }

  check_it(data.frame(x = 1, y = 1), data.frame(x = 1, z = 1))
  check_it(data.frame(x = 1, y = 1), data.frame(x = 1, z = 1), by = "x")
  check_it(data.frame(x = 1, y = 1), data.frame(x = 1, y = 1), by = "x")
  check_it(data.frame(x = 1, y = 1), data.frame(x = 1, y = 1.1), by = "x", n = 1)
})







