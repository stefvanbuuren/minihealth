#'@include class-individualID.R
#'@include class-individualBG.R
#'@include class-individualAN.R
#'@include class-individualBS.R
NULL

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

#' Convert single individual from donor data to class individual
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
#'
#' # from lollypop.preterm
#' # calculating Z-score relative to preterm growth references
#' q <- donordata.to.individual(id = 50001, src = donordata::lollypop.preterm)
#'ref.hgt <- create.reference.call(libname = "preterm", prefix = "pt2012a",
#'                                 sex = "female", yname = "hgt", sub = "32")
#'ref.wgt <- create.reference.call(libname = "preterm", prefix = "pt2012a",
#'                                 sex = "female", yname = "wgt", sub = "32")
#'ref.hdc <- create.reference.call(libname = "preterm", prefix = "pt2012a",
#'                                 sex = "female", yname = "hdc", sub = "32")
#'
#' # overwrite hgt, wgt and hdc slots
#' q@hgt <- new("xyz", x = q@hgt@x, y = q@hgt@y, call = ref.hgt)
#' q@wgt <- new("xyz", x = q@wgt@x, y = q@wgt@y, call = ref.wgt)
#' q@hdc <- new("xyz", x = q@hdc@x, y = q@hdc@y, call = ref.hdc)
#'
#' # and update broken stick estimate
#' q@bs.hgt <- new("bse", data = q@hgt)
#' q@bs.wgt <- new("bse", data = q@wgt)
#' q@bs.hdc <- new("bse", data = q@hdc)
#'
#' q
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

  p <- new("individual", pid, pbg, pan, pbs)

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

