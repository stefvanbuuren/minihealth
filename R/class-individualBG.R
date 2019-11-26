#' An S4 class to represent individual background variables
#'
#' This class stores individual time-invariant background variables,
#' such as sex or birth weight. Each slot has length one.
#' @slot sex  Sex: \code{"male"}, \code{"female"} (\code{character})
#' @slot etn  Etnicity: \code{"NL"}, \code{"TU"},
#'            \code{"MA"}, \code{"HS"}, \code{"other"} (\code{character})
#' @slot edu  Education: \code{"low"}, \code{"middle"}, \code{"high"}
#' @slot ga   Gestational age, weeks (\code{numeric})
#' @slot bw   Birth weight, grammes (\code{numeric})
#' @slot twin Twin: \code{0}, \code{1} (\code{numeric})
#' @slot agem Mother age, years (\code{numeric})
#' @slot smo  Smoked during pregnancy, \code{0}, \code{1} (\code{numeric})
#' @slot hgtm Height of mother in cm (\code{numeric})
#' @slot wgtm Weight of mother in kg (\code{numeric})
#' @slot hgtf Height of father in cm (\code{numeric})
#' @slot wgtf Weight of father in kg (\code{numeric})
#' @slot bfexc06 Days exclusive breastfeeding, 0-6 m (\code{numeric})
#' @slot durbrst Days mixed breastfeeding 0-6m (\code{numeric})
setClass("individualBG",
         slots = c(
           sex   = "character",
           etn   = "character",
           edu   = "character",
           ga    = "numeric",
           bw    = "numeric",
           twin  = "numeric",
           agem  = "numeric",
           smo   = "numeric",
           hgtm  = "numeric",
           wgtm  = "numeric",
           hgtf  = "numeric",
           wgtf  = "numeric",
           bfexc06 = "numeric",
           durbrst = "numeric"),
         prototype = list(
           sex   = NA_character_,
           etn   = NA_character_,
           edu   = NA_character_,
           ga    = NA_real_,
           bw    = NA_real_,
           twin  = NA_real_,
           agem  = NA_real_,
           smo   = NA_real_,
           hgtm  = NA_real_,
           wgtm  = NA_real_,
           hgtf  = NA_real_,
           wgtf  = NA_real_,
           bfexc06 = NA_real_,
           durbrst = NA_real_)
)

validIndividualBG <- function(object) {
  v <- slotNames(object)
  for (i in 1:length(v)) {
    if (length(slot(object, v[i])) > 1) return(paste("Length of slot", v[i],"higher than 1"))
    if (length(slot(object, v[i])) == 0) return(paste("Length of slot", v[i],"equals zero."))
  }
  TRUE
}
setValidity("individualBG", validIndividualBG)

