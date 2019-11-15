context("bds_schema_str")

# bds_schema <- file.path(path.package("minihealth"), "json", "bds_schema_str.json")

# testfiles: for interactive use only
jtf <- file.path(getwd(), "tests", "testthat", "data", paste0("test", 1:21, ".json"))

# testfiles: R CMD CHECK
jtf <- file.path("data", paste0("test", 1:20, ".json"))

test_that("test1.json (client3.json) passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[1], schema = "string"))
)

# validate_bds_individual(jtf[2], schema = "string")
# validate_bds_individual(jtf[3], schema = "string")
# validate_bds_individual(jtf[4], schema = "string")
# validate_bds_individual(jtf[5], schema = "string")
# validate_bds_individual(jtf[6], schema = "string")
# validate_bds_individual(jtf[7], schema = "string")
# validate_bds_individual(jtf[8], schema = "string")

test_that("test9.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[9], schema = "string"))
)
test_that("test10.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[10], schema = "string"))
)
test_that("test11.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[11], schema = "string"))
)
test_that("test12.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[12], schema = "string"))
)
test_that("test13.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[13], schema = "string"))
)

# validate_bds_individual(jtf[14], schema = "string")
# validate_bds_individual(jtf[15], schema = "string")

test_that("test16.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[16], schema = "string"))
)
test_that("test17.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[17], schema = "string"))
)

# validate_bds_individual(jtf[18], schema = "string")

test_that("test19.json passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[19], schema = "string"))
)

# validate_bds_individual(jtf[20], schema = "string")

test_that("test21.json (minimal) passes bds_schema_str.json",
          expect_true(validate_bds_individual(jtf[21], schema = "string"))
)

