context("Checkout testthat")

test_that("new xyz object has proper length",
          {
            expect_length(new("xyz")@x, 0)
            expect_length(new("xyz", x = 1:3)@x, 3)
          })
