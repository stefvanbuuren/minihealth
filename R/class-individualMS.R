#' @include class-ird.R
NULL

#' An S4 class to assemble individual milestone data
#'
#' @author Stef van Buuren 2020
#' @keywords classes
#' @slot ddi  Individual response data, \code{ddi} instrument
#' @examples
#' # create object with DDI
#' data <- data.frame(age = c(0.0821, 0.159, 0.255),
#'                    k1430 = c(1, NA, NA),
#'                    k1431 = c(2, NA, NA),
#'                    k1437 = c(3, 1, 1),
#'                    k1438 = c(0, 1, 1),
#'                    k1439 = c(0, 1, 1))
#' map <- data.frame(from = c("k1430", "k1431", "k1437", "k1438", "k1439"),
#'                   to = c(879, 927, 928, 881, 883))
#' z <- new("individualMS",
#'      ddi = new("ird", mst = data, map = map, instrument = "ddi"))
#' z
#' @export
setClass("individualMS",
         slots = c(
           ddi = "ird"),
         prototype = list(
           ddi = new("ird", instrument = "ddi"))
)

#' as("individualMS", "data.frame")
#'
#' @name as
#' @family individualMS
setAs("individualMS", "data.frame", function(from) {

  sn <- slotNames("individualMS")
  df <- vector("list", length(sn))
  for (i in seq_along(sn))
    df[[i]] <- as(slot(from, sn[i]), "data.frame")
  do.call(rbind.data.frame, df)
})

#' @export
as.data.frame.individualMS <-
  function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")

