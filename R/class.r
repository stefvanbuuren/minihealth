# class.r

setClass("personID",
         slots = c(
           src   = "character",
           id    = "integer",
           name  = "character",
           dob   = "POSIXct"
         ),
         prototype = list(
           id    = as.integer(0),
           dob   = as.POSIXct(Sys.Date())
         )
)

setClass("personBG",
         slots = c(
           sex   = "character",
           etn   = "character",
           ga    = "numeric",
           bw    = "numeric",
           mult  = "numeric",
           goodhealth = "logical",

           hgtm  = "numeric",
           wgtm  = "numeric",
           landm = "character",
           edum  = "character",
           agem  = "character",
           smo   = "logical",

           hgtf  = "numeric",
           wgtf  = "numeric",
           landf = "character",
           eduf  = "character"
         )
)


setClass("personAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz"
         ), prototype = list(
           hgt = new("xyz", yname = "hgt"),
           wgt = new("xyz", yname = "wgt"),
           hdc = new("xyz", yname = "hdc")
         )
)

setClass("personBS",
         slots = c(
           bs.hgt = "bse",
           bs.wgt = "bse",
           bs.hdc = "bse"
         ), prototype = list(
           bs.hgt = new("bse", data = new("xyz", yname = "hgt")),
           bs.wgt = new("bse", data = new("xyz", yname = "wgt")),
           bs.hdc = new("bse", data = new("xyz", yname = "hdc"))
         )
)


setClass("person",
         contains = c("personID", "personBG", "personAN", "personBS"),
         slots = c(
           src    = "character"
         ),
         prototype = list(
           src    = ""
         )
)


