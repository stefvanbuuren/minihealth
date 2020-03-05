#' An S4 class to represent raw individual response data
#'
#' The \code{ird} class stores raw individual milestone
#' 0/1/M+/M- data.
#' @slot mst A numeric \code{matrix} or a \code{data.frame} with
#' milestone responses data.
#' There should be a variable with decimal age named \code{"age"} (or
#' optionally a different name defined by \code{xname}). The remaining
#' columns may contain 0 (fail), 1 (pass), 2 (M+), 3 (M-) or \code{NA}
#' responses on milestones.
#' @slot map A data frame with two columns that maps column names
#' in \code{mst} into \code{bds} numbers. The first column is a
#' character vector with names in \code{mst} (do not include the age variable),
#' the second is an integer vector with the BDS number.
#' @slot bds A calculated data frame containing age plus the
#' mapped variables from \code{mst}. Values are coded into the
#' BDS-codes: 1 (pass), 2 (fail, M-), 3 (M+). If not specified, the
#' \code{bds} slot is converted from \code{mst} and \code{map}.
#' @slot xname Name of the age variable
#' @slot instrument A 3-letter code identifying the measurement instrument,
#' for example, \code{instrument = "ddi"}.
#' @details
#' Conversion of currently only one-way, from \code{mst} to \code{bds}.
#' @author Stef van Buuren 2020
#' @keywords classes
#' @examples
#' data <- data.frame(age = c(0.0821, 0.159, 0.255),
#'                    k1430 = c(1, NA, NA),
#'                    k1431 = c(2, NA, NA),
#'                    k1437 = c(3, 1, 1),
#'                    k1438 = c(0, 1, 1),
#'                    k1439 = c(0, 1, 1))
#' map <- data.frame(from = c("k1430", "k1431", "k1437", "k1438", "k1439"),
#'                   to = c(879, 927, 928, 881, 883))
#' new("ird", mst = data, map = map)
#' @export
setClass("ird",
         slots = c(
           mst = "data.frame",
           map = "data.frame",
           bds = "data.frame",
           xname = "character",
           instrument = "character"),
         prototype = list(
           mst = data.frame(age = numeric(0)),  # uses source names
           map = data.frame(from = NA_character_, to = NA_integer_),
           bds = data.frame(),  # uses BDS numbers
           xname = "age",
           instrument = NA_character_))

setMethod(
  "initialize", "ird",
  function(.Object, mst, map, bds,
           xname = "age", instrument, ...) {

    if (missing(mst)) {
      mst <- data.frame(age = numeric(0))
      colnames(mst)[which(colnames(mst) == "age")] <- xname
      .Object@mst <- mst
    }
    else
      .Object@mst <- mst

    if (!missing(xname)) .Object@xname <- xname

    if (!missing(map)) .Object@map <- map

    if (missing(bds)) {
      # create bds from mst and map
      mst <- .Object@mst
      xname <- .Object@xname
      map <- .Object@map

      # remove rows with no BDS mapping
      map <- map[!is.na(map[[2]]), ]
      age <- mst[[xname]]

      # find set of unique matches (ex age)
      from <- intersect(map[[1L]], colnames(mst))
      from <- setdiff(from, xname)
      to <- map[map[[1L]] %in% from, 2L]
      if (length(to)) to <- paste0("n", to)
      else to <- character(0)

      # extract, rename and recode
      bds <- mst[, c(xname, from), drop = FALSE]
      colnames(bds) <- c(xname, to)
      for (i in to)
        bds[[i]] <- dplyr::recode(bds[[i]],
                                  `0` = 2L,
                                  `1` = 1L,
                                  `2` = 3L,
                                  `3` = 2L)
      .Object@bds <- bds
    }

    if (missing(instrument)) .Object@instrument <- NA_character_
    else .Object@instrument <- instrument

    check <- validObject(.Object)
    .Object
  }
)

setValidity("ird", function(object) {
  if (ncol(object@map) != 2L) return("Argument `map` should have 2 columns.")
  TRUE
})

setMethod("show",
          signature(object = "ird" ),
          function (object) {
            cat("Object of class ird\n")
            # print(head(object@mst, 3))
            print(head(object@bds, 3))
            # print(head(object@map))
          }
)
