#' Get the x, y and z coordinates of the measurement
#'
#' The function removes rows with incomplete \code{"x"} or \code{"y"}
#' components.
#' @param ind An object of class \code{individual}.
#' @param yname Name of outcome variable, e.g. \code{"hgt"}. Must be one
#' of \code{slotNames(ind)}.
#' @return  A data frame with names \code{"x"}, \code{"y"} and \code{"z"}
#' @examples
#' # Extract anthropometric measures
#' ind <- new("individual",
#'            hgt = new("xyz", x = c(0.3, 2, 6)),
#'            wgt = new("xyz", x = c(0.2, 1), y = c(1, NA)))
#' get_xyz(ind, "wgt")
#' get_xyz(ind, "hgt")
#' @export
get_xyz <- function(ind, yname) {
  y <- slot(ind, yname)@y
  x <- slot(ind, yname)@x
  z <- slot(ind, yname)@z
  mis <- is.na(y) | is.na(x)
  y <- y[!mis]
  x <- x[!mis]
  z <- z[!mis]
  data.frame(x, y, z)
}
