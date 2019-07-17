library(minihealth)
fn <- paste0("file://", file.path(path.package("minihealth"), "testdata", "client3.json"))
con <- curl::curl(fn, open = "r")
txt <- readLines(con)
d <- jsonlite::fromJSON(txt)
b <- d$ClientGegevens$Elementen

data("installed.cabinets", package = "jamestest")
ind <- installed.cabinets[[2]][[1]]

z <- convert_individual_bds(ind)
