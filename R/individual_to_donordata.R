#' Convert object of class 'individual' to donordata
#'
#'This function returns the slots \code{"id"}, \code{"child"} and
#' \code{"time"}, either separate or as a \code{list} from
#' the \code{individual} object. The code
#' assumes that slots \code{"id"}, \code{"child"} and \code{"time"}
#' are up-to-data and consistent with the other slots.
#' @param x An object of class \code{individual}.
#' @param type The type of slot. This one of \code{"id"}, \code{"child"} and
#'\code{"time"}. The default \code{type = NULL} return a list of all three slots.
#' @return This function returns slots \code{"id"}, \code{"child"} and
#'\code{"time"}, or a list of these slots (if \code{type = NULL}).
#' @author Stef van Buuren 2017
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' require("donordata")
#' p <- donordata_to_individual(dnr = "smocc", id = 10001)
#' p
#' q <- individual_to_donordata(p)
#' q
#' @export
individual_to_donordata <- function(x, type = NULL)
{
  if (!is.individual(x)) stop("Object not of S4 class 'individual'.")

  if (is.null(type)) return(list(id = slot(x, "id"),
                                 child = slot(x, "child"),
                                 time = slot(x, "time")))
  slot(x, match.arg(type, c("id", "child", "time")))
}
