library(donorloader)
smocc_bs <- load_data(dnr = "smocc_bs")

child <- new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4))
boy <-   new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4), sex = "male")
girl <-  new("xyz", x = c(0, 0.2, 0.5), y = c(51, 54.1, 63.4), sex = "female")

d1 <- new("bse", child)
test_that("Returns broken stick estimates in hgt_z", {
  expect_equal(data.frame(d1)$hgt_z, c(-0.186800879792024, 0.0939706645376884,
                                       0.0690021611092986))
})

d2 <- new("bse", girl)
test_that("Returns broken stick estimates in hgt_z", {
  expect_equal(data.frame(d2)$hgt_z, c(-0.125540937662353, -1.51990513649756,
                                       -1.32386127980939))})

d2boy <- new("bse", boy)

d3 <- new("bse", new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4),
sex = "female"))
test_that("Calculate predicted value for each x", {
  expect_equal(data.frame(d3)$hgt_z, c(-0.125540937662353, -1.51990513649756,
                                       -1.32386127980939))})


d4 <- new("bse", new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4),
                     sex = "female"), at = "knots")
test_that("Obtain bse estimates at all knots", {
  expect_equal(data.frame(d4)$hgt_z, c(-0.125540937662353, -1.24354883770978,
                                       -1.49686560138033, -1.54457273298282, -1.17597617211549, -1.32386127980939,
                                       -1.11270039650077, -1.27454361747446, -1.25460024828542, -1.16979367353307,
                                       -1.16151692040689, -1.06886922255481, -1.18603356061138))
})

d5 <- new("bse", girl, usetransform = TRUE, ga = 32, sex = "female")
test_that("transform_y() converts -0.126 into 41.22 cm", {
  expect_equal(data.frame(d5)$hgt, c(41.22, 47.502, 58.786))
})

test_that("stores the name of transform_y()", {
  expect_equal(d5@transform, "transform_y()")
})

