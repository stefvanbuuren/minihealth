#'@include class-individualID.R
#'@include class-individualBG.R
#'@include class-individualAN.R
#'@include class-individualBS.R
#'@include class-individualRW.R
NULL

#' An S4 class to represent individual data
#'
#'A collection of objects of \linkS4class{individualID},
#'\linkS4class{individualBG}, \linkS4class{individualAN},
#'\linkS4class{individualBS} and \linkS4class{individualRW}
#'representing data of an individual. The
#'type of information covers individual identifyers, fixed background
#'variables, time-varying anthroponetric measures and time-varying
#'broken stick estimates. The \code{new("individual")} function can
#'automatically calculate standard deviation scores relative to a
#'growth reference, and predictions according the a broken stick
#'model.
#'@name individual-class
#'@author Stef van Buuren 2016/2019
#'@seealso \linkS4class{individualID}, \linkS4class{individualBG},
#'         \linkS4class{individualAN}, \linkS4class{individualBS},
#'         \linkS4class{individualRW}
#'@keywords classes
#'@export
setClass("individual",
         contains = c("individualID",
                      "individualBG",
                      "individualAN",
                      "individualBS",
                      "individualRW"))

#' Is this object of class `individual`?
#'
#' @param x An object
#' @return A logical
#' @export
is.individual <- function(x)
{
  inherits(x,"individual")
}
