#' An S4 class to represent individual identifiers
#'
#' @slot id   Unique numerical individual identifier in source (\code{integer}, length 1)
#' @slot name Name of the individual (\code{character})
#' @slot dob  Date of birth of class (\code{Date}, length 1)
#' @slot src  Name of data source (\code{character})
#' @author Stef van Buuren 2016
#' @examples
#' # Create a new ID for Ron and Jasper
#' ron <- new("individualID", name = c("Ron", "Smith"),
#'            dob = format(as.Date("1999-08-22"), "%d-%m-%y"),
#'            id = as.integer(204))
#' jasper <- new("individualID", name = c("Jasper", "Fielding"),
#'            id = as.integer(220))
#'@export
setClass("individualID",
         slots = c(
           id    = "integer",
           name  = "character",
           dob   = "character",
           src   = "character"
         ), prototype = list(
           id    = NA_integer_,
           name  = NA_character_,
           dob   = NA_character_,
           src   = NA_character_
           )
)

validIndividualID <- function(object) {
  if(length(object@id) != 1) return("Slot id not of length 1")
  if(length(object@dob) != 1) return("Slot dob not of length 1")
  TRUE
  }
setValidity("individualID", validIndividualID)

