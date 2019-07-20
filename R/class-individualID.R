#' An S4 class to represent individual identifiers
#'
#' @slot id   Unique numerical individual identifier within \code{src} (\code{integer}, length 1)
#' @slot name Name of the individual (\code{character})
#' @slot dob  Date of birth of class (\code{Date}, length 1)
#' @slot src  Study name (\code{character})
#' @slot dnr  Name of data object (\code{character})
#' @author Stef van Buuren 2019
#' @examples
#' # Create a new ID for Ron and Jasper
#' ron <- new("individualID", name = c("Ron", "Smith"),
#'            dob = as.Date("1999-08-22", "%Y-%m-%d"),
#'            id = as.integer(204))
#' jasper <- new("individualID", name = c("Jasper", "Fielding"),
#'            id = as.integer(220))
#'@export
setClass("individualID",
         slots = c(
           id    = "integer",
           name  = "character",
           dob   = "Date",
           src   = "character",
           dnr   = "character"),
         prototype = list(
           id    = NA_integer_,
           name  = NA_character_,
           dob   = as.Date(NA), # as.character(Sys.Date(), format = "%d-%m-%y"),
           src   = NA_character_,
           dnr   = NA_character_)
)

validIndividualID <- function(object) {
  if(length(object@id) != 1) return("Slot `id` not of length 1")
  if(length(object@dob) != 1) return("Slot `dob` not of length 1")
  # if (!is.na(object@dob))
  #   if (is.na(as.Date(object@dob, format="%y-%m-%d")))
  #       return ("Slot `dob` not formatted as `%y-%m-%d`")
  TRUE
}
setValidity("individualID", validIndividualID)

