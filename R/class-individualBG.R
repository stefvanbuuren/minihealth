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
