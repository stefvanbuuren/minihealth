# class-individual.R

#' An S4 class to represent individual identifiers
#'
#' @slot id   Unique numerical individual identifier in source (\code{integer}, length 1)
#' @slot name Name of the individual (\code{character})
#' @slot dob  Date of birth of class (\code{date}, length 1)
#' @slot src  Name of data source (\code{character})
#' @author Stef van Buuren 2016
#' @examples
#' # Create a new ID for Ron and Jasper
#' ron <- new("individualID", name = c("Ron", "Smith"),
#'            dob = as.Date("1999-08-22"), id = as.integer(204))
#' jasper <- new("individualID", name = c("Jasper", "Fielding"),
#'            id = as.integer(220))
#'@export
setClass("individualID",
         slots = c(
           id    = "integer",
           name  = "character",
           dob   = "Date",
           src   = "character"
         ), prototype = list(
           id    = 0L,
           name  = NA_character_,
           dob   = as.Date(NA),
           src   = NA_character_
           )
)

validIndividualID <- function(object) {
  if(length(object@id) != 1) return("Slot id not of length 1")
  if(length(object@dob) != 1) return("Slot dob not of length 1")
  TRUE
  }
setValidity("individualID", validIndividualID)




#' An S4 class to represent individual background variables
#'
#' @slot sex  Either \code{"male"} or \code{"female"} (\code{character})
#' @slot etn  Etnicity code (\code{character})
#' @slot ga   Gestational age in weeks (\code{numeric})
#' @slot bw   Birth weight in grammes (\code{numeric})
#' @slot mult Multiplicity, 1 = singleton, 2 = twin (\code{integer})
#' @slot goodhealth In good health, \code{TRUE} or \code{FALSE} (\code{logical})
#' @slot hgtm Height of mother in cm (\code{numeric})
#' @slot wgtm Weight of mother in kg (\code{numeric})
#' @slot landm Country of birth, mother (\code{character})
#' @slot edum Education mother, \code{"low"}, \code{"middle"}, \code{"high"} (\code{character})
#' @slot agem Mother age when pregnant, \code{"(15,25]"},
#'\code{"(25,36]"} or \code{"(36,45]"} (\code{character})
#' @slot smo  Mother smoked during pregnancy, \code{TRUE} or \code{FALSE} (\code{logical})
#' @slot hgtf Height of father in cm (\code{numeric})
#' @slot wgtf Weight of father in kg (\code{numeric})
#' @slot landf Country of birth, father (\code{character})
#' @slot eduf Education father, \code{"low"}, \code{"middle"}, \code{"high"} (\code{character})
setClass("individualBG",
         slots = c(
           sex   = "character",
           etn   = "character",
           ga    = "numeric",
           bw    = "numeric",
           mult  = "integer",
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

#' An S4 class to represent individual anthropometric data
#'
#'The \code{individualAN} class stores anthropometric measures as the collection of three \code{xyz}-class for height, weight and head circumference, respectively.
#' @slot hgt  Length/height in cm (\code{xyz})
#' @slot wgt  Weight in kg (\code{xyz})
#' @slot hdc  Head circumference in cm (\code{xyz})
#' @seealso \code{\link{xyz-class}}
#' @examples
#' # create object with height and weight measures
#' # add Z-scores calculate according to Dutch 1997 references
#' new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0.5), y = c(50, 70)),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0.3), y = c(3, 6)))
#'
#' # calculate -2 and +2 centiles for height and head circumference
#' # using the WHO Child Growth Standard for girls
#' hgtref <- create.reference.call(libname = "who", prefix = "who2011",
#'                                 sex = "female", yname = "hgt", sub = "")
#' hdcref <- create.reference.call(libname = "who", prefix = "who2011",
#'                                 sex = "female", yname = "hdc", sub = "")
#' new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0, 0.25, 0.25), z = c(-2, 2, -2, 2), call = hgtref),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0, 1, 1), z = c(-2, 2, -2, 2), call = hdcref))

#' @author Stef van Buuren 2016
setClass("individualAN",
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

#' An S4 class to represent individual broken stick estimates
#'
#' @slot hgt  Length/height in cm (\code{bse})
#' @slot wgt  Weight in kg (\code{bse})
#' @slot hdc  Head circumference in cm (\code{bse})
#' @author Stef van Buuren 2016
setClass("individualBS",
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

#' An S4 class to represent individual data
#'
#'A collection of object of \code{individualID}, \code{individualBG}, \code{individualAN} and \code{individualBS}, representing data of an individual.
#'The type of information covers individual identifyers, fixed background variables, time-varying anthroponetric measures and time-varying broken stick estimates. The \code{new("individual")} function can automatically calculate standard deviation scores relative to a growth reference, and predictions according the a broken stick model.
#'@name individual-class
#'@author Stef van Buuren 2016
#'@seealso \code{\link[=individualID-class]{individualID}}
#'@keywords classes
#'@export
setClass("individual",
         contains = c("individualID",
                      "individualBG",
                      "individualAN",
                      "individualBS")
)

#' Convert single individual donor data to individual class
#'
#' This function takes data from the \pkg{donordata} package, extract cases identified by \code{id} and save as a an object of class \code{individual}. The function automatically calculates standard deviation scores and broken stick conditional means per visit.
#' @aliases donordata.to.individual
#' @param id the id number of the individual in specified source
#' @param src A character indicating the source
#' @param \dots Additional parameter passed down to \code{new("xyz",... )} and \code{new("bse",... )}. Useful parameters are \code{model = "bsmodel"} for setting the broken stick model, or \code{call = as.call(...)} for setting proper reference standards.
#' @author Stef van Buuren 2016
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' require("donordata")
#' p <- donordata.to.individual(10001)
#' p
#' @export
donordata.to.individual <- function(id, src = donordata::smocc, ...) {

  get.child.data <- function(id, donor){
    if (id[1] == 0) return(NULL)
    select <- donor$child$id %in% id
    if (all(!select)) return(NULL)
    return(donor$child[select, , drop = FALSE])
  }
  get.time.data <- function(id, donor){
    if (id[1] == 0) return(NULL)
    select <- donor$time$id %in% id
    if (all(!select)) return(NULL)
    return(donor$time[select, , drop = FALSE])
  }
  child <- get.child.data(id, src)
  time <- get.time.data(id, src)

  if (is.null(child) & is.null(time)) return(new("individual"))

  # information at child level
  if (is.null(child)) {
    pid <- new("individualID")
    pbg <- new("individualBG")
  } else {
    if (is.null(child$dob)) child$dob <- NA
    dob <- as.Date(child$dob, format = "%d-%m-%y")
    pid <- new("individualID",
               id    = as.integer(id),
               name  = as.character(child$name),
               dob   = dob)
    pbg <- new("individualBG",
               sex   = as.character(child$sex),
               ga    = as.numeric(child$ga),
               bw    = as.numeric(child$bw),
               mult  = as.integer(child$mult),
               goodhealth = ifelse(as.numeric(child$goodhealth) == 1, TRUE, FALSE),

               hgtm  = as.numeric(child$hgtm),
               wgtm  = as.numeric(child$wgtm),
               landm = as.character(child$etn),
               edum  = as.character(child$edum),
               agem  = as.character(child$agem),
               smo   = ifelse(as.numeric(child$smo) == 1, TRUE, FALSE),

               hgtf  = as.numeric(child$hgtf),
               wgtf  = as.numeric(child$wgtf),
               eduf  = as.character(child$eduf)
    )
  }

  # information at visit level
  if (is.null(time)) {
    pan <- new("individualAN")
    pbs <- new("individualBS")
  } else {
    pan <- new("individualAN",
               hgt = new("xyz", yname = "hgt",
                         x = as.numeric(time$age),
                         y = as.numeric(time$hgt),
                         ...),
               wgt = new("xyz", yname = "wgt",
                         x = as.numeric(time$age),
                         y = as.numeric(time$wgt),
                         ...),
               hdc = new("xyz", yname = "hdc",
                         x = as.numeric(time$age),
                         y = as.numeric(time$hdc),
                         ...))
    pbs <- new("individualBS",
               bs.hgt = new("bse", yname = "hgt",
                            data = pan@hgt,
                            ...),
               bs.wgt = new("bse", yname = "wgt",
                            data = pan@wgt,
                            ...),
               bs.hdc = new("bse", yname = "hdc",
                            data = pan@hdc,
                            ...))
  }

  p <- new("individual",
           pid, pbg, pan, pbs)

  return(p)
}


#' Is this object of class `individual`?
#'
#' @param x An object
#' @return A logical
#' @export
is.individual <- function(x)
{
  inherits(x,"individual")
}

