#' Get the x, y and z coordinates of the measurement
#'
#' @param ind An object of class \code{individual}.
#' @param yname A vector of name of outcome variable
#' @return  A data frame
#' @examples
#' # Extract anthropometric measures
#' ind <- new("individual",
#'            hgt = new("xyz", x = c(0.3, 2, 6)),
#'            wgt = new("xyz", x = c(0.2, 1), y = c(1, NA)))
#' get_xyz(ind, "wgt")
#' get_xyz(ind, "hgt")
#' @export
get_xyz <- function(ind, yname) {
  # remove the incomplete xyz-combinations
  y <- slot(ind, yname)@y
  x <- slot(ind, yname)@x
  z <- slot(ind, yname)@z
  mis <- is.na(y) | is.na(x)
  if (all(mis)) return(data.frame(NULL))
  y <- y[!mis]
  x <- x[!mis]
  z <- z[!mis]
  data.frame(x, y, z)
}
