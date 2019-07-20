# context("donordata_to_individual")
#
# library(donordata)
# library(donorloader)
# library(jamestest)
#
# con <- NULL
# dnr <- "smocc"
# ids <- as.integer(c(34071, 34072, 34073, 34075, 34076, 34077, 34078, 34079, 34080, 34081))
# names <- c("Laura S", "Thomas S", "Anne S", "Jeroen S",
#            "Mark S", "Kevin S", "Linda S", "Iris S",
#            "Tim S", "Rick S")
#
# child <- load_child_data(con = con, dnr = dnr, ids = ids[1])
# time <- load_time_data(con = con, dnr = dnr, ids = ids[1])
#
# laura <- donordata_to_individual(dnr = dnr, id = ids[1])
# kevin <- donordata_to_individual(dnr = dnr, id = ids[6])
#
# # lollypop.preterm
# dnr <- "lollypop.preterm"
# ids <- as.integer(c(53696, 53675, 53676, 53684, 53519, 53520, 53531, 53582, 53583, 53584))
# names <- c("Jurre P", "Sanne P",  "Milan P", "Roos P",  "Bram P",
#            "Freek P", "Anouk P",  "Sharon P", "Nick P", "Simon P")
#
# id <- ids[1]
# child <- load_child_data(con = con, dnr = dnr, ids = id)
# time <- load_time_data(con = con, dnr = dnr, ids = id)
#
# jurre <- donordata_to_individual(dnr = dnr, id = ids[1])
# anouk <- donordata_to_individual(dnr = dnr, id = ids[6])
#
