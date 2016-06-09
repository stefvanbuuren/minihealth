#' An S4 class to represent individual anthropometric data
#'
#'The \code{individualAN} class stores anthropometric measures as the collection of three \code{xyz}-class for height, weight and head circumference, respectively.
#' @slot hgt  Length/height in cm (\code{xyz})
#' @slot wgt  Weight in kg (\code{xyz})
#' @slot hdc  Head circumference in cm (\code{xyz})
#' @seealso \code{\link{xyz-class}}
#' @examples
#' # create object with height and weight measures
#' # add Z-scores calculate according to Dutch 1997 references
#' new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0.5), y = c(50, 70)),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0.3), y = c(3, 6)))
#'
#' # calculate -2 and +2 centiles for height and head circumference
#' # using the WHO Child Growth Standard for girls
#' hgtref <- create.reference.call(libname = "who", prefix = "who2011",
#'                                 sex = "female", yname = "hgt", sub = "")
#' hdcref <- create.reference.call(libname = "who", prefix = "who2011",
#'                                 sex = "female", yname = "hdc", sub = "")
#' new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0, 0.25, 0.25), z = c(-2, 2, -2, 2), call = hgtref),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0, 1, 1), z = c(-2, 2, -2, 2), call = hdcref))

#' @author Stef van Buuren 2016
setClass("individualAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz"
         ), prototype = list(
           hgt = new("xyz", yname = "hgt"),
           wgt = new("xyz", yname = "wgt"),
           hdc = new("xyz", yname = "hdc")
         )
)
