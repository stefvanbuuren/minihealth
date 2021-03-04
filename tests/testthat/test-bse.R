library(donorloader)
smocc_bs <- load_data(dnr = "smocc_bs")

child <- new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4))
boy <-   new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4), sex = "male")
girl <-  new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4), sex = "female")

d1 <- new("bse", child)
test_that("Returns broken stick estimates in hgt", {
  expect_equal(data.frame(d1)$y, c(NA_real_, NA_real_, NA_real_))
  })

d2 <- new("bse", girl)
test_that("Returns broken stick estimates in hgt", {
  expect_equal(data.frame(d2)$y, c(50.66, 54.744, 63.376))
  })

d2boy <- new("bse", boy)
test_that("Returns broken stick estimates in hgt", {
  expect_equal(data.frame(d2boy)$y, c(50.595, 54.858, 63.471))
})

d4 <- new("bse", new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4),
                     sex = "female"), at = "knots")
test_that("Obtain bse estimates at all knots", {
  expect_equal(data.frame(d4)$y, c(50.66, 51.142, 53.246, 56.294, 59.595,
                                     63.376, 66.28, 67.999, 70.643, 74.511,
                                     78.705, 84.123, 92.26))
})

# adapt transformation to y by setting ga
d5 <- new("bse", girl, ga = 32, usetransform = TRUE)
d6 <- new("bse", girl, ga = 32, usetransform = FALSE)
test_that("transform_y() converts -0.126 into 41.22 cm (usetransform)", {
  expect_equal(data.frame(d5)$y, c(41.22, 47.502, 58.786))
})

test_that("transform_y() converts -0.126 into 41.22 cm (no usetransform)", {
  expect_equal(data.frame(d6)$y, c(41.22, 47.502, 58.786))
})

test_that("stores the name of transform2y()", {
  expect_equal(d5@transform, "transform2y()")
})

