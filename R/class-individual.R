#'@include class-individualID.R
#'@include class-individualBG.R
#'@include class-individualAN.R
#'@include class-individualBS.R
NULL

#' An S4 class to represent individual data
#'
#'A collection of objects of \linkS4class{individualID},
#'\linkS4class{individualBG}, \linkS4class{individualAN} and
#'\linkS4class{individualBS}
#'representing data of an individual. The
#'type of information covers individual identifyers, fixed background
#'variables, time-varying anthroponetric measures and time-varying
#'broken stick estimates. The \code{new("individual")} function can
#'automatically calculate standard deviation scores relative to a
#'growth reference, and predictions according the a broken stick
#'model.
#'@name individual-class
#'@slot child If constructed from a `donordata` object, this slot
#'contains the child level record. This slot is included for backward
#'compatibility, and should be removed when feasible.
#'@slot time If constructed from a `donordata` object, this slot contains
#'the time level data frame. This slot is included for backward
#'compatibility, and should be removed when feasible.
#'@author Stef van Buuren 2016/2019
#'@seealso \linkS4class{individualID}, \linkS4class{individualBG},
#'         \linkS4class{individualAN}, \linkS4class{individualBS}
#'@keywords classes
#'@export
setClass("individual",
         contains = c("individualID",
                      "individualBG",
                      "individualAN",
                      "individualBS"),
         slots = c(
           child = "data.frame",
           time = "data.frame"
         ),
         prototype = list(
           child = data.frame(),
           time = data.frame()
         )
)

#' Is this object of class `individual`?
#'
#' @param x An object
#' @return A logical
#' @export
is.individual <- function(x)
{
  inherits(x,"individual")
}

