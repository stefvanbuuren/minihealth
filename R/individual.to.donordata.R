#' Convert object of class 'individual' to donordata
#'
#'@details The code assumes that slots \code{"id"}, \code{"child"} and
#' \code{"time"} are up-to-data.
#' @param x An object of class \code{individual}.
#' @param type The type of slot. This one of \code{"id"}, \code{"child"} and
#'\code{"time"}. The default \code{type = NULL} return a list of all three slots.
#' @return This function returns slots \code{"id"}, \code{"child"} and
#'\code{"time"}, or a list of these slots (if \code{type = NULL}).
#' @author Stef van Buuren 2017
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' require("donordata")
#' p <- donordata.to.individual(src = "smocc", id = 10001)
#' p
#' q <- individual.to.donordata(p)
#' q
#' @export
individual.to.donordata <- function(x, type = NULL)
{
  if (!is.individual(x)) stop("Object not of S4 class 'individual'.")

  if (is.null(type)) return(list(id = slot(x, "id"),
                                 child = slot(x, "child"),
                                 time = slot(x, "time")))
  slot(x, match.arg(type, c("id", "child", "time")))
}

#   # get the mask of the data
#   child <- load_child_data(src, rows = 1)
#   child <- rbind(child, NA)[-1, ]
#
#   time <- load_time_data(src, rows = 1)
#   time <- rbind(time, NA)[-(1:nrow(time)), ]
#
#   # determine set of ID and background variables
#   from_vars <- c(slotNames("individualID"), slotNames("individualBG"))
#   vars <- intersect(names(to), from_vars)
#
#   # generic variable conversion
#   classes <- sapply(child, class)
#   for (yvar in vars) {
#     if (length(slot(x, yvar) == 0)) child[1, yvar] <- NA
#     else if (is.factor(to[[yvar]])) child[1, yvar] <- slot(x, yvar)
#     else child[1, yvar] <- as(slot(x, yvar), classes[yvar])
#   }
#
#   # broken stick estimates
#   x@bs.hgt@y
#   from_vars <- paste0(rep(slotNames("individualBS"), each = 18), 0:17)
#
#     r[1, paste0("hgt", 0:11)] <- x@bs.hgt@y
#     r[1, paste0("wgt", 0:11)] <- x@bs.wgt@y
#     r[1, paste0("hdc", 0:11)] <- x@bs.hdc@y
#     r[1, c("hgt12", "wgt12", "hdc12")] <- NA
#     return(r[1,])
#   }
#
#   if (src == "lollypop") {
#     r[1, "id"] <- as.numeric(x@id)
#     r[1, "src"] <- src
#     r[1, "rec"] <- 1
#     r[1, "nrec"] <- length(x@hgt@x)
#     r[1, "sex"] <- x@sex
#     r[1, "etn"] <- "NL"
#     r[1, "ga"] <- as.numeric(x@ga)
#     r[1, "bw"] <- as.numeric(x@bw)
#     r[1, "mult"] <- as.numeric(x@mult)
#     r[1, paste0("hgt", 0:11)] <- x@bs.hgt@y
#     r[1, paste0("hgt", 11:13)] <- NA
#     r[1, paste0("wgt", 0:11)] <- x@bs.wgt@y
#     r[1, paste0("wgt", 11:13)] <- NA
#     r[1, paste0("hdc", 0:9)] <- x@bs.hdc@y[1:10]
#     return(r[1,])
#   }
# }
#
