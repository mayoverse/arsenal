
# set.seed(100)
# nsubj <- 90 # need multiple of 3
# mdat <- data.frame(
#   Group = c(rep("High", nsubj/3), rep("Med", nsubj/3), rep("Low", nsubj/3)),
#   Sex = sample(c("Male", "Female"), nsubj, replace=TRUE),
#   Age = round(rnorm(nsubj, mean=40, sd=5)),
#   Phase = ordered(sample(c("I", "II", "III"), nsubj, replace=TRUE), levels=c("I", "II", "III")),
#   ht_in = round(rnorm(nsubj, mean=65, sd=5)),
#   time = round(runif(nsubj,0,7)),
#   status = rbinom(nsubj, 1, prob=0.4),
#   dt = as.Date(round(rnorm(90, mean=100, sd=2000)), origin="1950/01/01"),
#   missing = as.character(NA),
#   trt = factor(sample(c("A", "B"), nsubj, replace=TRUE)),
#   ethan = factor(c(NA, NA, NA, sample(c("Ethan", "Heinzen"), nsubj - 3, replace=TRUE))),
#   weights = c(20, 1.5, rep(1, nsubj - 2)),
#   stringsAsFactors = FALSE
# )
# mdat$Group.fac <- factor(mdat$Group)
# attr(mdat$ht_in, "label") <- "Height in Inches"
# attr(mdat$trt, "label") <- "Treatment Arm"
# attr(mdat$Age, "label") <- "Age in Years"
#
# class(mdat$Sex) <- c("dummyClassToTriggerErrors", class(mdat$Sex))
#
# saveRDS(mdat, "tests/testthat/mdat.rds", compress = TRUE)


## As of 2019-02-26 the random number generator behind sample() changed, so I've saved out a copy to test against instead.
mdat <- readRDS("mdat.rds")
