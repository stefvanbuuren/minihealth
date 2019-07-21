context("convert_bds_individual")

empty <- new("individual")
js <- minihealth::convert_individual_bds(empty)

test_that("handles the empty individual object",
          expect_true(is.individual(convert_bds_individual(js)))
)


#library(jamesclient)
#fn <- file.path(path.package("jamesclient"), "testdata", "client3.json")
#data("installed.cabinets", package = "jamestest")
# ind2 <- convert_bds_individual(fn)
