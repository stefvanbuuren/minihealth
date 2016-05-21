# class.r

setClass("personID",
         slots = c(
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
           ga    = "numeric",
           bw    = "numeric",
           mult  = "numeric",
           goodhealth = "logical",

           hgtm  = "numeric",
           wgtm  = "numeric",
           landm = "character",
           edum  = "character",
           agem  = "numeric",
           smo   = "logical",

           hgtf  = "numeric",
           wgtf  = "numeric",
           landf = "character",
           eduf  = "character"
         ), prototype = list(
           sex   = "male"
         )
)




setClass("personAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz"
         )
)

setClass("person",
         contains = c("personID", "personBG", "personAN"),
         slots = c(
           test    = "integer"
         ),
         prototype = list(
           test = as.integer(0)
         )
)

