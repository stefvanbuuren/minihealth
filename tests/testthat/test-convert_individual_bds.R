# library(minihealth)
# fn <- system.file( "testdata", "client3.json", package = "minihealth")
# con <- curl::curl(fn, open = "r")
# txt <- readLines(con)
# d <- jsonlite::fromJSON(txt)
# b <- d$ClientGegevens$Elementen
#
# data("installed.cabinets", package = "jamestest")
# ind <- installed.cabinets[[2]][[1]]
#
# z <- convert_individual_bds(ind)
