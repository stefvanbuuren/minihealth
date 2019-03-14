#' Get a range from the antropometric data
#'
#' @param ind An object of class \code{individual}.
#' @param ynames A vector of name of outcome variable
#' @param slotname The name of the slot, either \code{"x"} (often age),
#' \code{"y"} (measure) or \code{"z"} (SDS).
#' @return  A numeric vector of length 2 (if success), or
#' \code{numeric(0)} (if failed)
#' @examples
#' # Find age range of anthropometric measures
#' ind <- new("individual",
#'            hgt = new("xyz", x = c(0.3, 2, 6)),
#'            wgt = new("xyz", x = c(0.2, 1), y = c(1, NA)))
#' get_range(ind)
#' get_range(ind, slotname = "y")
#' get_range(ind, ynames = c("hdc", "bmi"))
#' @export
get_range <- function(ind,
                      ynames = c("hgt", "wgt", "hdc", "bmi"),
                      slotname = "x") {

  if (!is.individual(ind)) stop("Object not of S4 class 'individual'.")

  ynames <- ynames[ynames %in% slotNames(ind)]

  # find the highest age across measurements
  rng <- c(NA, NA)
  for (yname in ynames) {
    v <- slot(slot(ind, yname), slotname)
    if (sum(!is.na(v)) > 0L) rng <- range(c(v, rng), na.rm = TRUE, finite = TRUE)
  }

  if (any(is.na(rng))) rng <- numeric(0)
  rng
}

