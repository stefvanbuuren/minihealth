#' An S4 class to represent individual D-score data
#'
#' The \code{individualDS} class stores individual milestone 0/1 data
#' and calculates the D-score and DAZ.
#' @slot mst An numeric \code{matrix} or a \code{data.frame} with
#' milestone responses data.
#' The matrix should contain a column indicating age of measurement
#' for each row, and columns with 0/1/NA responses on
#' milestones. Colomn names of milestones must adhere to the 9-position
#' GSED milestones naming convention.
#' @slot dfa  D-score for age in D-units (\code{xyz})
#' @slot xname Name of the age variable
#' @slot xunit Time unit of the age variable
#' @slot key The conversion key for the D-score
#' @slot population The reference population for DAZ calculation
#' @seealso \code{\link{xyz-class}}, \code{\link[dscore]{dscore}}
#' @author Stef van Buuren 2019
#' @keywords classes
#' @export
setClass("individualDS",
         slots = c(
           mst = "data.frame",
           dfa = "xyz",
           xname = "character",
           xunit = "character",
           key = "character",
           population = "character"),
         prototype = list(
           mst = data.frame(),
           dfa = new("xyz"),
           xname = "age",
           xunit = "decimal",
           key = "gsed",
           population = "gcdg"
         )
)

# setMethod(
#   "initialize", "individualDS",
#   function (.Object, mst, dfa, ...) {
#
#     # convert to numeric, do nothing if data has no rows
#     if (missing(mst))
#       .Object@mst <- data.frame(age = numeric(0))
#     else
#       .Object@mst <- mst
#
#     if (missing(dfa)) {
#       # calculate the D-score
#       .Object@dfa <- new("xyz", yname = "dfa", ...)
#       #d <- dscore(data = .Object@mst,
#       #            xname = .Object@xname, xunit = .Object@xunit,
#       #            key = .Object@key, population = .Object@key)
#       #.Object@dfa <- new("xyz",
#       #                   x = d$a, y = d$d, z = d$daz,
#       #                   yname = "dfa", ...)
#     }
#     callNextMethod(.Object, ...)
#   }
# )
