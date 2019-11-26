context("donordata_to_individual")

library(donorloader)
library(jamestest)

con <- NULL
dnr <- "smocc"
ids <- as.integer(c(34071, 34072, 34073, 34075, 34076, 34077, 34078, 34079, 34080, 34081))
names <- c("Laura S", "Thomas S", "Anne S", "Jeroen S",
           "Mark S", "Kevin S", "Linda S", "Iris S",
           "Tim S", "Rick S")

child <- load_data(con = con, dnr = dnr, element = "child", ids = ids[1])
time <- load_data(con = con, dnr = dnr, element = "time", ids = ids[1])

laura <- donordata_to_individual(dnr = dnr, id = ids[1])
kevin <- donordata_to_individual(dnr = dnr, id = ids[6])

# lollypop.preterm
dnr <- "lollypop.preterm"
ids <- as.integer(c(53696, 53675, 53676, 53684, 53519, 53520, 53531, 53582, 53583, 53584))
names <- c("Jurre P", "Sanne P",  "Milan P", "Roos P",  "Bram P",
           "Freek P", "Anouk P",  "Sharon P", "Nick P", "Simon P")

id <- ids[1]
child <- load_data(con = con, dnr = dnr, element = "child", ids = id)
time <- load_data(con = con, dnr = dnr, element = "time", ids = id)

jurre <- donordata_to_individual(dnr = dnr, id = ids[1])
anouk <- donordata_to_individual(dnr = dnr, id = ids[6])

# terneuzen
dnr <- "terneuzen"
terneuzen <- load_data(dnr = dnr)
ids <- as.integer(terneuzen$child$id[seq(100, 1000, by = 100)])
names <- c("T 163", "T 1017",  "T 1413", "T 2035",  "T 2602",
           "T 3254", "T 4207",  "T 5002", "T 5270", "T 6021")

id <- ids[1]
child <- load_data(con = con, dnr = dnr, element = "child", ids = id)
time <- load_data(con = con, dnr = dnr, element = "time", ids = id)

t163 <- donordata_to_individual(dnr = dnr, id = ids[1])
t4207 <- donordata_to_individual(dnr = dnr, id = ids[6])

# graham
dnr <- "graham"
child <- load_data(con = con, dnr = dnr, element = "child", ids = 1)
time <- load_data(con = con, dnr = dnr, element = "time", ids = 1)

g1 <- donordata_to_individual(dnr = dnr, id = 1)


