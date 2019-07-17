#' Convert data of class individual to bds format
#'
#' This function takes data from an object of class
#' \linkS4class{individual} and saves it into JSON bds format (which
#' can be sent to JAMES).
#'
#' Functions \code{convert_bds_individual()} and
#' \code{convert_individual_bds()} are inverse operations.
#' @param x Object of class \code{individual}
#' @param \dots Additional parameters. Currently ignored.
#' @return Data in bds format as JSON
#' @author Stef van Buuren 2019
#' @seealso \linkS4class{individual}, \linkS4class{bse}, \linkS4class{xyz},
#'          \code{\link[jsonlite]{toJSON}}
#' @examples
#' data("installed.cabinets", package = "jamestest")
#' x <- installed.cabinets[[2]][[1]]
#' b <- convert_individual_bds(x)
#' @export
convert_individual_bds <- function(x = NULL, ...) {
  x
}

