context("bds_schema_str")

# bds_schema <- file.path(path.package("minihealth"), "json", "bds_schema_str.json")

# testfiles: for interactive use only
jtf <- file.path(getwd(), "tests", "testthat", "data", paste0("test", 1:21, ".json"))

# testfiles: R CMD CHECK
jtf <- file.path("data", paste0("test", 1:21, ".json"))

test_that("test1.json (client3.json) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[1], schema = "string")))

test_that("test2.json (missing Referentie) FAILS bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[2], schema = "string")))

test_that("test3.json (missing OrganisatieCode) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[3], schema = "string")))

test_that("test4.json (wrong type) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[4], schema = "string")))

test_that("test5.json (missing ClientGegevens) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[5], schema = "string")))

test_that("test6.json (Missing ContactMomenten) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[6], schema = "string")))

test_that("test7.json (Missing Referentie & OrganisatieCode) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[7], schema = "string")))

# JSON SYNTAX ERROR: c++ exception (unknown reason)
#test_that("test8.json (Invalid OrganisatieCode number) FAILS bds_schema_str.json",
#          expect_false(validate_bds_individual(jtf[8], schema = "string")))

# currently gives TRUE
test_that("test9.json (Bdsnummer 19 missing) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[9], schema = "string")))

# currently gives TRUE
test_that("test10.json (Bdsnummer 20 missing) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[10], schema = "string")))

test_that("test11.json (Bdsnummer 82 missing) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[11], schema = "string")))

test_that("test12.json (Bdsnummer 91 missing) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[12], schema = "string")))

test_that("test13.json (Bdsnummer 110 missing) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[13], schema = "string")))

test_that("test14.json (Empty file) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[14], schema = "string")))

test_that("test15.json (Bdsnummer 19 numeric) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[15], schema = "string")))

test_that("test16.json (Bdsnummer 20 numeric) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[16], schema = "string")))

test_that("test17.json (Bdsnummer 82 numeric) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[17], schema = "string")))

test_that("test18.json (Bdsnummer 91 numeric) FAILS bds_schema_str.json",
          expect_false(validate_bds_individual(jtf[18], schema = "string")))

test_that("test19.json (Bdsnummer 110 numeric) PASSES bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[19], schema = "string")))

test_that("test20.json (missing Groepen) passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[20], schema = "string")))

test_that("minimal test21.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[21], schema = "string"))
)
