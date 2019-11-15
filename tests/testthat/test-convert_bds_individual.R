context("convert_bds_individual")

empty <- new("individual")
js <- minihealth::convert_individual_bds(empty)

# test_that("handles the empty individual object",
#           expect_true(is.individual(convert_bds_individual(js)))
# )

# testfiles: for interactive use only
jtf <- file.path(getwd(), "tests", "testthat", "data", paste0("test", 1:21, ".json"))

# testfiles: R CMD CHECK
jtf <- file.path("data", paste0("test", 1:21, ".json"))

test_that("test1.json (client3.json) passes convert_individual_bds()",
          expect_s4_class(convert_bds_individual(jtf[1]), "individual")
)

#test_that("test2.json passes convert_individual_bds()",
#          expect_s4_class(convert_bds_individual(jtf[2]), "individual")
#)


test_that("test8.json returns error message",
          expect_error(convert_bds_individual(jtf[8]), "lexical error: invalid char in json text.")
)

test_that("test14.json return error message",
          expect_error(convert_bds_individual(jtf[14]), "premature EOF")
)

# test_that("minimal file test21.json turns into S4-object",
#           expect_s4_class(convert_bds_individual(jtf[21]), "individual")
# )



#library(jamesclient)
#fn <- file.path(path.package("jamesclient"), "testdata", "client3.json")
#data("installed.cabinets", package = "jamestest")
# ind2 <- convert_bds_individual(fn)
