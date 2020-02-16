#' An S4 class to represent individual broken stick estimates
#'
#' @slot hgt  Length/height in cm (\code{bse})
#' @slot wgt  Weight in kg (\code{bse})
#' @slot hdc  Head circumference in cm (\code{bse})
#' @slot bmi  Body mass index kg/m**2 (\code{bse})
#' @slot wfh  Weight for height kg/m (\code{bse})
#' @slot dsc  D-score (D) (\code{bse}) - currently unavailable
#' @author Stef van Buuren 2016-2020
setClass("individualBS",
         slots = c(
           bs.hgt = "bse",
           bs.wgt = "bse",
           bs.hdc = "bse",
           bs.bmi = "bse",
           bs.wfh = "bse",
           bs.dsc = "bse"
         ), prototype = list(
           bs.hgt = new("bse",
                        data = new("xyz", yname = "hgt")),
           bs.wgt = new("bse", data = new("xyz", yname = "wgt")),
           bs.hdc = new("bse", data = new("xyz", yname = "hdc")),
           bs.bmi = new("bse", data = new("xyz", yname = "bmi")),
           bs.wfh = new("bse", data = new("xyz", yname = "wfh", xname = "hgt")),
           bs.dsc = new("bse", data = new("xyz", yname = "dsc"))
         )
)
