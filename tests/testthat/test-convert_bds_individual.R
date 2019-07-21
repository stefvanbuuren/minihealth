data("installed.cabinets", package = "jamestest")
ind <- installed.cabinets[[3]][[5]]
js <- minihealth::convert_individual_bds(ind)

ind2 <- convert_bds_individual(js)

library(jamesclient)
fn <- file.path(path.package("jamesclient"), "testdata", "client3.json")

ind2 <- convert_bds_individual(fn)
