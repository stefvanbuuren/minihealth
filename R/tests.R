
# should be caught bu validObject()
# new("person", ga = 40, hgt = new("xyz", y = c(1, 2)))@hgt


#zz <- NULL
#as.POSIXct(ifelse(is.null(zz), NA, zz), format = "%d-%m-%y", tz = "UTC")

# as.POSIXct(ifelse(is.null(zz), NA, zz), format = "%d-%m-%y", tz = "UTC")

# no data
new("bse", data = new("xyz", x = as.numeric(NA), y = as.numeric(NA)))

