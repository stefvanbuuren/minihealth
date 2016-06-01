#' Convert single person donor data to person class
#'
#' This function takes data from the \pkg{donordata} package, extract cases identified by \code{id} and save as a an object of class \code{person}. The function automatically calculates standard deviation scores and broken stick conditional means per visit.
#' @aliases donordata.to.person
#' @param id the id number of the person in specified source
#' @param src character vector with the name of the dataset. Default is \code{src = "smocc"}
#' @param \dots Additional parameter passed down to \code{new("xyz",... )} and \code{new("bse",... )}. Useful parameters are \code{model = "bsmodel"} for setting the broken stick model, or \code{call = as.call(...)} for setting proper reference standards.
#' @author Stef van Buuren 2016
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' require("donordata")
#' p <- donordata.to.person(10001)
#' p
#' @export
donordata.to.person <- function(id, src = "smocc", ...) {

  child <- get.child.data(id, src)
  time <- get.time.data(id, src)

  if (is.null(child) & is.null(time)) return(new("person"))

  # information at child level
  if (is.null(child)) {
    pid <- new("personID")
    pbg <- new("personBG")
  } else {
    pid <- new("personID",
               id    = as.integer(id),
               name  = as.character(child$name),
               dob   = as.POSIXct(child$dob, format = "%d-%m-%y", tz = "UTC"))
    pbg <- new("personBG",
               sex   = as.character(child$sex),
               ga    = as.numeric(child$ga),
               bw    = as.numeric(child$bw),
               mult  = as.numeric(child$mult),
               goodhealth = ifelse(as.numeric(child$goodhealth) == 1, TRUE, FALSE),

               hgtm  = as.numeric(child$hgtm),
               wgtm  = as.numeric(child$wgtm),
               landm = as.character(child$etn),
               edum  = as.character(child$edum),
               agem  = as.character(child$agem),
               smo   = ifelse(as.numeric(child$smo) == 1, TRUE, FALSE),

               hgtf  = as.numeric(child$hgtf),
               wgtf  = as.numeric(child$wgtf),
               eduf  = as.character(child$eduf))
  }

  # information at visit level
  if (is.null(time)) {
    pan <- new("personAN")
    pbs <- new("personBS")
  } else {
    pan <- new("personAN",
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
    pbs <- new("personBS",
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

  p <- new("person",
             src = src,
             pid, pbg, pan, pbs)

  return(p)
}


