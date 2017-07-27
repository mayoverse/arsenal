#' Mock study data for examples
#'
#' Mock clinical study data for examples to test data manipulation and statistical functions.
#'   The function \code{muck_up_mockstudy()} is used in examples for \code{\link{compare.data.frame}}.
#'
#' @format A data frame with 1499 observations on the following 15 variables:
#'   \describe{
#'     \item{\code{case}}{a numeric identifier-patient ID}
#'     \item{\code{age}}{age in years}
#'     \item{\code{arm}}{treatment arm divided into 3 groups, character string }
#'     \item{\code{sex}}{a factor with levels \code{Male} \code{Female}}
#'     \item{\code{race}}{self-reported race/ethnicity, character string}
#'     \item{\code{fu.time}}{survival or censoring time in years}
#'     \item{\code{fu.stat}}{censoring status; 1=censor, 2=death}
#'     \item{\code{ps}}{integer, ECOG performance score }
#'     \item{\code{hgb}}{numeric, hemoglobin count}
#'     \item{\code{bmi}}{numeric, body mass index, kg/m^2}
#'     \item{\code{alk.phos}}{numeric, alkaline phosphatase}
#'     \item{\code{ast}}{numeric, aspartate transaminase }
#'     \item{\code{mdquality.s}}{integer, LASA QOL 0=Clinically Deficient, 1=Not Clinically Deficient  }
#'     \item{\code{age.ord}}{an ordered factor split of age, with levels
#'       \code{10-19} < \code{20-29} < \code{30-39} < \code{40-49} <
#'         \code{50-59} < \code{60-69} < \code{70-79} < \code{80-89}}
#'   }
#' @examples
#' data(mockstudy)
#' str(mockstudy)
#' @name mockstudy
NULL
#> NULL

#' @rdname mockstudy
"mockstudy"

#' @rdname mockstudy
#' @export
muck_up_mockstudy <- function()
{
  mockstudy <- arsenal::mockstudy
  mockstudy2 <- mockstudy[c(1, 3, 6, 10:nrow(mockstudy), 2, 4), ]
  mockstudy2$sex <- factor(mockstudy2$sex, levels = c("Female", "Male"))
  mockstudy2$ps[3] <- NA
  mockstudy2$ast[1:3] <- 36
  mockstudy2$age <- NULL
  mockstudy2$fu_time <- mockstudy2$fu.time
  mockstudy2$fu.time <- NULL
  mockstudy2$`fu stat` <- mockstudy2$fu.stat
  mockstudy2$fu.stat <- NULL
  mockstudy2$Arm <- mockstudy2$arm
  mockstudy2$arm <- NULL
  mockstudy2$hgb[is.na(mockstudy2$hgb)] <- -9
  mockstudy2$race[mockstudy2$race == "Caucasian"] <- "caucasian"
  mockstudy2$race <- factor(mockstudy2$race)
  attr(mockstudy2$sex, "label") <- "Sex (M/F)"
  mockstudy2
}
