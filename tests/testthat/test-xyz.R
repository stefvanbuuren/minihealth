test_that("new xyz object has proper length",
          {
            expect_length(new("xyz")@x, 0)
            expect_length(new("xyz", x = 1:3)@x, 3)
          })

child <- new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4))
test_that("No Z-score since we haven't specified the child's sex", {
  expect_equal(data.frame(child)$z, rep(NA_real_, 3))
})

boy <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
girl <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "female")

test_that("Provides Z-score for boy", {
  expect_equal(data.frame(boy)$z, c(-0.154, -2.312, -1.829))
})

test_that("Provides Z-score for girl", {
  expect_equal(data.frame(girl)$z, c(0.052, -1.824, -1.314))
})

# specify length (in cm) for boy at ages 0, 0.2 and 0.5 years
d1 <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
test_that("Provides Z-score for d1", {
  expect_equal(data.frame(d1)$z, c(-0.154, -2.312, -1.829))
})

# Z-scores of weight (in kg) at same ages
d2 <- new("xyz", x = c(0, 0.2, 0.5), y = c(3.2, 5.2, 7.0), yname = "wgt",
  sex = "female")
test_that("Provides Z-score for d2", {
  expect_equal(data.frame(d2)$z, c(-0.369, 0.099, -0.429))
  })

# Head circumference of girl, relative to WHO standard
d3 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41),
          pkg = "centile", refcode = "who_2007_hdc_female_")
test_that("Uses WHO reference for Z-scores in d3", {
  expect_equal(data.frame(d3)$z, c(0.947, -0.652, -0.921))
})

# Standard weight centiles at age 0.5 year of Dutch girls
d5 <- new("xyz", x = rep(0.5, 5), z = -2:2, sex = "female", yname = "wgt")
test_that("Returns standard weight centiles at age 0.5 year of Dutch girls", {
  expect_equal(data.frame(d5)$y, c(5.87, 6.57, 7.34, 8.187, 9.116))
})

# calculate centiles at 1 year using the female WHO head circumference reference
d6 <- new("xyz", yname = "hdc", x = rep(1, 5), z = -2:2, pkg = "centile",
 refcode = "who_2007_hdc_female_")
test_that("Returns centiles at 1 year using the female WHO head circumference reference", {
  expect_equal(data.frame(d6)$y, c(42.178, 43.537, 44.896, 46.255,
                                   47.614))
})

# calculate P50 for preterms, female and born a GA week 32
d7 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
  refcode = "nl_2012_hgt_female_32")
test_that("Returns P50 for preterms, female and born a GA week 32", {
  expect_equal(head(data.frame(d7)$y, 3), c(41.57, 45.793, 49.857))})

#'# use nlreferences::transform2y() as an alternative
d8 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
   usetransform = TRUE, ga = 32, sex = "female")
test_that("Use transform_y function instead of reference call", {
  expect_equal(head(data.frame(d8)$y, 3), c(41.57, 45.793, 49.857))
})

#'# use nlreferences::transform2z function
d9 <- new("xyz", yname = "hgt", x = seq(0, 0.25, 1/12), y = c(43, 46, 48, 50),
   usetransform = TRUE, ga = 32, sex = "female")
test_that("Use transform_z function instead of reference call", {
  expect_equal(data.frame(d9)$y, c(43, 46, 48, 50))
})


d10 <- new("xyz", yname = "wfh", xname = "hgt", x = c(60, 70, 80), z = rep(0, 3))
