
jtf <- system.file("extdata", "test", paste0("test", 1:24, ".json"), package = "jamestest")

test_that("test1.json (client3.json) passes individual_to_donordata()", {
          expect_silent(individual_to_donordata(convert_bds_individual(jtf[1])))})

test_that("test3.json (client3.json) passes individual_to_donordata()", {
  expect_message(individual_to_donordata(convert_bds_individual(jtf[3])))})

test_that("test5.json passes individual_to_donordata()", {
  expect_message(individual_to_donordata(convert_bds_individual(jtf[5])))})


# invalid json --> error
test_that("test8.json passes individual_to_donordata()", {
  expect_error(individual_to_donordata(convert_bds_individual(jtf[8])))})

test_that("test9.json passes individual_to_donordata()", {
  expect_message(individual_to_donordata(convert_bds_individual(jtf[9])))})

# invalid json --> error
test_that("test14.json passes individual_to_donordata()", {
  expect_error(individual_to_donordata(convert_bds_individual(jtf[14])))})

