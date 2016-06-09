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
