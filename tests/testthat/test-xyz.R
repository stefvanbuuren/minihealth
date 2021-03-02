library(donorloader)
smocc_bs <- load_data(dnr = "smocc_bs")

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

# Obtain
test_that("Uses correct reference for Z-scores in d2", {
  expect_equal(head(eval(d2@call)@table@table$M), c(3.34, 3.51,
                                                    3.69, 3.86, 4.03, 4.21))
})

# List available WHO references in clopus package
# find.references() only searches packages in the search list
# so let attach clopus first
#library("clopus")
#find.reference(libname = "who")
# detach("package:clopus")

# Head circumference of girl, relative to WHO standard
d3 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41), libname = "clopus::who",
          prefix = "who2011", sex = "female", sub = "", yname = "hdc")
test_that("Uses WHO reference for Z-scores in d3", {
  expect_equal(data.frame(d3)$z, c(0.947, -0.652, -0.921))
})

#'# Shortcut specification of WHO standard
#'d3@call
#'d4 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41), call = d3@call)
#'d4

# Standard weight centiles at age 0.5 year of Dutch girls
d5 <- new("xyz", x = rep(0.5, 5), z = -2:2, sex = "f", yname = "wgt")
test_that("Returns standard weight centiles at age 0.5 year of Dutch girls", {
  expect_equal(data.frame(d5)$y, c(5.87048826730353, 6.56973250654311,
                                   7.33976501305483, 8.18652601500635, 9.11635448688022))
})

# calculate centiles at 1 year using the female WHO head circumference reference
ref <- clopus::create.reference.call(libname = "clopus::who", prefix = "who2011",
                                sex = "female", yname = "hdc", sub = "")
d6 <- new("xyz", yname = "hdc", x = rep(1, 5), z = -2:2, call = ref)
test_that("Returns centiles at 1 year using the female WHO head circumference reference", {
  expect_equal(data.frame(d6)$y, c(42.1783954305, 43.53741021525,
                                   44.896425, 46.25543978475, 47.6144545695))
})

# calculate P50 for preterms, female and born a GA week 32
ref <- clopus::create.reference.call(libname = "clopus::preterm", prefix = "pt2012a",
                                sex = "female", yname = "hgt", sub = "32")
d7 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13), call = ref)
test_that("Returns P50 for preterms, female and born a GA week 32", {
expect_equal(head(data.frame(d7)$y, 3), c(41.57, 45.7934031413613,
                                          49.8565625))
})

# use transform_y function instead of reference call
d8 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
  usetransform = TRUE, ga = 32, sex = "female")
test_that("Use transform_y function instead of reference call", {
  expect_equal(head(data.frame(d8)$y, 3), c(41.57, 45.793, 49.857))
})

# use transform_z function
d9 <- new("xyz", yname = "hgt", x = seq(0, 0.25, 1/12), y = c(43, 46, 48, 50),
  usetransform = TRUE, ga = 32, sex = "female")
test_that("Use transform_z function instead of reference call", {
  expect_equal(data.frame(d9)$y, c(43, 46, 48, 50))
})
