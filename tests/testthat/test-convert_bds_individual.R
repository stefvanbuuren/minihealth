context("convert_bds_individual")

empty <- new("individual")
js <- minihealth::convert_individual_bds(empty)

# test the empty object
test_that("handles the empty individual object",
           expect_error(is.individual(convert_bds_individual(js, schema = "string")),
          "should have required property 'Elementen'"))

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
                       "required BDS number(s) missing: 19", fixed = TRUE))

test_that("test10.json (Bdsnummer 20 missing) FAILS",
          expect_error(convert_bds_individual(jtf[10], schema = "string"),
                       "required BDS number(s) missing: 20", fixed = TRUE))

test_that("test11.json (Bdsnummer 82 missing) PASSES",
          expect_s4_class(convert_bds_individual(jtf[11], schema = "string"), "individual"))

test_that("test12.json (Bdsnummer 91 missing) PASSES",
          expect_s4_class(convert_bds_individual(jtf[12], schema = "string"), "individual"))

test_that("test13.json (Bdsnummer 110 missing) PASSES",
          expect_s4_class(convert_bds_individual(jtf[13], schema = "string"), "individual"))

test_that("test14.json return error message",
          expect_error(convert_bds_individual(jtf[14], schema = "string"), "premature EOF"))

test_that("test15.json (Bdsnummer 19 numeric) PASSES with message",
          expect_message(convert_bds_individual(jtf[15], schema = "string"),
                       '[{"bdsnummer":19,"description":"Sex of child","expected":"one of: 0, 1, 2, 3","supplied":"2","supplied_type":"numeric"},{"bdsnummer":62,"description":"Caretaker relation","expected":"one of: 01, 02, 03, 04, 05, 06, 07, 08, 98","supplied":"1","supplied_type":"numeric"}]'))

test_that("test16.json (Bdsnummer 20 numeric) PASSES",
          expect_s4_class(convert_bds_individual(jtf[16], schema = "string"), "individual"))

test_that("test17.json (Bdsnummer 82 numeric) PASSES",
          expect_s4_class(convert_bds_individual(jtf[17], schema = "string"), "individual"))

test_that("test18.json (Bdsnummer 91 numeric) FAILS",
          expect_message(convert_bds_individual(jtf[18], schema = "string"),
                         '[{"bdsnummer":91,"description":"Smoking during pregnancy","expected":"one of: 1, 2, 99","supplied":"1","supplied_type":"numeric"}]'))

test_that("test19.json (Bdsnummer 110 numeric) PASSES",
          expect_s4_class(convert_bds_individual(jtf[19], schema = "string"), "individual"))

test_that("test20.json (missing Groepen) PASSES",
           expect_s4_class(convert_bds_individual(jtf[20], schema = "string"), "individual"))

test_that("test21.json (minimal data) PASSES",
          expect_s4_class(convert_bds_individual(jtf[21], schema = "string"), "individual"))


# test_that("minimal file test21.json turns into S4-object",
#           expect_s4_class(convert_bds_individual(jtf[21]), "individual")
# )



#library(jamesclient)
#fn <- file.path(path.package("jamesclient"), "testdata", "client3.json")
#data("installed.cabinets", package = "jamestest")
# ind2 <- convert_bds_individual(fn)
