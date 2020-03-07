#' Tidy individual class
#'
#' This function takes data from an object of class
#' \linkS4class{individual} and saves a \code{data.frame}
#' with anthropometric measures.
#'
#' @param ind Object of class \code{individual}
#' @param remove_duplicates Logical. Should function return data frame
#' with duplicated rows removed? Default is \code{TRUE}.
#' @param \dots Additional parameters. Currently ignored.
#' @return A \code{data.frame}
#' @author Stef van Buuren 2020
#' @seealso \linkS4class{individual}
#' @examples
#' data("installed.cabinets", package = "jamestest")
#' ind <- installed.cabinets[[3]][[8]]
#' b <- tidy_individual(ind)
#' @export
tidy_individual <- function(ind, remove_duplicates = TRUE) {
  ynames <- c("hgt", "wgt", "hdc", "bmi", "wfh", "dsc")

  x <- c(ind@hgt@x, ind@wgt@x, ind@hdc@x, ind@bmi@x, ind@dsc@x)
  y <- c(ind@hgt@y, ind@wgt@y, ind@hdc@y, ind@bmi@y, ind@dsc@y)
  z <- c(ind@hgt@z, ind@wgt@z, ind@hdc@z, ind@bmi@z, ind@dsc@z)
  yname <- rep(ynames, c(length(ind@hgt@x), length(ind@wgt@x),
                         length(ind@hdc@x), length(ind@bmi@x),
                         0, length(ind@dsc@x)))

  # bind, remove rows with both y and z missing
  b <- data.frame(x, yname, y, z, stringsAsFactors = FALSE)
  idx <- !((is.na(y) & is.na(z)) | duplicated(b))
  b[idx, ]
}
