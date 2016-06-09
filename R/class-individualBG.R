#' An S4 class to represent individual background variables
#'
#' This class stores individual time-invariant background variables,
#' such as sex or birth weight. Each slot can only have
#' length zero (if entry is unset or missing) or length one.
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
         ), prototype = list(
           sex   = NA_character_,
           etn   = NA_character_,
           ga    = NA_real_,
           bw    = NA_real_,
           mult  = NA_integer_,
           goodhealth = NA,
           hgtm  = NA_real_,
           wgtm  = NA_real_,
           landm = NA_character_,
           edum  = NA_character_,
           agem  = NA_character_,
           smo   = NA,
           hgtf  = NA_real_,
           wgtf  = NA_real_,
           landf = NA_character_,
           eduf  = NA_character_
         )
)

validIndividualBG <- function(object) {
  v <- slotNames(object)
  for (i in 1:length(v)) {
    if (length(slot(object, v[i])) > 1) return(paste("Length of slot", v[i],"higher than 1"))
  }
  TRUE
}
setValidity("individualBG", validIndividualBG)


cleanout <- function(){}
