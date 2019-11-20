context("convert_bds_individual")

empty <- new("individual")
js <- minihealth::convert_individual_bds(empty)

# preferred action
# test_that("handles the empty individual object",
#           expect_true(is.individual(convert_bds_individual(js)))
# )

# we should get rid of the error below
#test_that("handles the empty individual object",
#          expect_error(convert_bds_individual(js),"should have required property 'Waarde'"))


# testfiles: for interactive use only
jtf <- file.path(getwd(), "tests", "testthat", "data", paste0("test", 1:21, ".json"))

# testfiles: R CMD CHECK
jtf <- file.path("data", paste0("test", 1:21, ".json"))

test_that("test1.json (client3.json) passes convert_individual_bds()",
          expect_s4_class(convert_bds_individual(jtf[1], schema = "string"), "individual")
)

test_that("test2.json (missing Referentie) PASSES",
          expect_s4_class(convert_bds_individual(jtf[2], schema = "string"), "individual"))

test_that("test3.json (missing OrganisatieCode) FAILS",
          expect_error(convert_bds_individual(jtf[3], schema = "string"),
                       "should have required property 'OrganisatieCode'"))

test_that("test4.json (wrong type) FAILS",
          expect_error(convert_bds_individual(jtf[4], schema = "string"),
                       ".OrganisatieCode should be integer"))

test_that("test5.json (missing ClientGegevens) FAILS",
          expect_error(convert_bds_individual(jtf[5], schema = "string"),
                       "should have required property 'ClientGegevens'"))

test_that("test6.json (Missing ContactMomenten) PASSES",
          expect_s4_class(convert_bds_individual(jtf[6], schema = "string"), "individual"))

test_that("test7.json (Missing Referentie & OrganisatieCode) FAILS",
          expect_error(convert_bds_individual(jtf[7], schema = "string"),
                       "should have required property 'OrganisatieCode'"))

test_that("test8.json returns error message",
          expect_error(convert_bds_individual(jtf[8], schema = "string"), "lexical error: invalid char in json text."))

test_that("test9.json (Bdsnummer 19 missing) FAILS",
          expect_error(convert_bds_individual(jtf[9], schema = "string"),
                       "Required BDS number(s) missing: 19", fixed = TRUE))

test_that("test10.json (Bdsnummer 20 missing) FAILS",
          expect_error(convert_bds_individual(jtf[10], schema = "string"),
                       "Required BDS number(s) missing: 20", fixed = TRUE))

# test_that("test11.json (Bdsnummer 82 missing) PASSES",
#           expect_s4_class(convert_bds_individual(jtf[11], schema = "string"), "individual"))


test_that("test14.json return error message",
          expect_error(convert_bds_individual(jtf[14]), "premature EOF"))

# test_that("minimal file test21.json turns into S4-object",
#           expect_s4_class(convert_bds_individual(jtf[21]), "individual")
# )



#library(jamesclient)
#fn <- file.path(path.package("jamesclient"), "testdata", "client3.json")
#data("installed.cabinets", package = "jamestest")
# ind2 <- convert_bds_individual(fn)
