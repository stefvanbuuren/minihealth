#' Convert single individual from donor data to class individual
#'
#' This function takes data from the \pkg{donordata} package, extract cases identified by \code{id} and save as a an object of class \code{individual}. The function automatically calculates standard deviation scores and broken stick conditional means per visit.
#' @param con A database connection. The default \code{con = NULL} reads the data from
#' the donordata package.
#' @param dnr A character indicating the source, e.g. \code{dnr = "smocc"}
#' @param id the id number of the individual in specified source. If specified as a vector,
#' only the first element is used.
#' @param \dots Additional parameter passed down to \code{new("xyz",... )} and \code{new("bse",... )}.
#' Useful parameters are \code{models = "bsmodel"} for setting the broken stick model,
#' or \code{call = as.call(...)} for setting proper reference standards.
#' @return An object of class \code{individual}. If \code{id} is not found in data
#' \code{dnr}, then the function will set only slots \code{id} and \code{dnr}.
#' @author Stef van Buuren 2017
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' require("donorloader")
#' smocc <- load_data(dnr = "smocc")
#' smocc_bs <- load_data(dnr = "smocc_bs")
#' lollypop <- load_data(dnr = "lollypop")
#' terneuzen <- load_data(dnr = "terneuzen")
#' terneuzen_bs <- load_data(dnr = "terneuzen_bs")
#'
#' p <- donordata_to_individual(dnr = "smocc", id = 10001)
#'
#' # from lollypop
#' # calculating Z-score relative to preterm growth references
#' q <- donordata_to_individual(dnr = "lollypop", id = 50001)
#'
#' # use models argument to estimate brokenstick estimates
#' p <- donordata_to_individual(dnr = "terneuzen", id = 11,
#'   models = "terneuzen_bs")
#'
#' @export
donordata_to_individual <- function(con = NULL, dnr, id, ...) {

  id <- id[1]

  # we use as.data.frame() to avoid tibble warnings if column doesn't exist
  # see https://github.com/tidyverse/tibble/issues/450
  child <- as.data.frame(load_data(con = con, dnr = dnr, element = "child", ids = id))
  time <- as.data.frame(load_data(con = con, dnr = dnr, element = "time", ids = id))

  if (nrow(child) == 0 & nrow(time) == 0) return(
    new("individual", dnr = dnr, id = as.integer(id), ...))

  # information at child level
  if (is.null(child)) {
    pid <- new("individualID")
    pbg <- new("individualBG")
  } else {
    pid <- new("individualID",
               src   = set.slot(child, "src", "character"),
               dnr   = set.slot(child, "src", "character"),
               id    = set.slot(child, "id", "integer"),
               name  = set.slot(child, "name", "character"),
               dob   = set.slot(child, "dob", "Date"))

    pbg <- new("individualBG",
               sex   = set.slot(child, "sex", "character"),
               etn   = set.slot(child, "etn", "character"),
               edu   = set.slot(child, "edu", "character"),

               gad   = set.slot(child, "ga") * 7,
               ga    = set.slot(child, "ga"),
               bw    = set.slot(child, "bw"),
               twin  = set.slot(child, "twin"),
               agem  = set.slot(child, "agem"),
               smo   = set.slot(child, "smo"),

               hgtm  = set.slot(child, "hgtm"),
               wgtm  = set.slot(child, "wgtm"),
               hgtf  = set.slot(child, "hgtf"),
               wgtf  = set.slot(child, "wgtf"),

               bfexc06 = set.slot(child, "bfexc06"),
               durbrst  = set.slot(child, "durbrst")
    )
  }

  # information at visit level
  if (is.null(time)) {
    pan <- new("individualAN")
    pbs <- new("individualBS")
    mil <- new("individualRW")
  } else {
    pan <- new("individualAN",
               hgt = new("xyz", yname = "hgt",
                         x = time$age,
                         y = time$hgt,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...),
               wgt = new("xyz", yname = "wgt",
                         x = time$age,
                         y = time$wgt,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...),
               hdc = new("xyz", yname = "hdc",
                         x = time$age,
                         y = time$hdc,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...),
               bmi = new("xyz", yname = "bmi",
                         x = time$age,
                         y = time$wgt/(time$hgt/100)^2,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...),
               wfh = new("xyz", yname = "wfh",
                         xname = "hgt",
                         x = time$hgt,
                         y = time$wgt,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...),
               dsc = new("xyz",
                         yname = "dsc",
                         x = time$age,
                         y = time$dsc,
                         usetransform = TRUE,
                         sex = pbg@sex,
                         ga = pbg@ga,
                         ...))
    pbs <- new("individualBS",
               bs.hgt = new("bse", yname = "hgt",
                            data = pan@hgt,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.wgt = new("bse", yname = "wgt",
                            data = pan@wgt,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.hdc = new("bse", yname = "hdc",
                            data = pan@hdc,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.bmi = new("bse", yname = "bmi",
                            data = pan@bmi,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.wfh = new("bse", yname = "wfh",
                            xname = "hgt",
                            data = pan@wfh,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...),
               bs.dsc = new("bse", yname = "dsc",
                            data = pan@dsc,
                            at = "knots",
                            usetransform = TRUE,
                            sex = pbg@sex,
                            ga = pbg@ga,
                            ...))
    prw <- new("individualRW",
               ddi = new("ird", mst = time,
                         map = load_data(dnr = "smocc_bds"),
                         instrument = "ddi", ...))
  }

  new("individual", pid, pbg, pan, pbs, prw)
}

set.slot <- function(data, name,
                     type = c("numeric", "character", "integer", "Date")) {
  type <- match.arg(type)
  if (!name %in% names(data)) return(switch(type,
                                            "numeric" = NA_real_,
                                            "character" = NA_character_,
                                            "integer" = NA_integer_,
                                            "Date" = as.Date(NA)))
  if (type == "numeric") return(as.numeric(data[, name]))
  if (type == "character") return(as.character(data[, name]))
  if (type == "integer") return(as.integer(data[, name]))
  if (type == "Date") return(dmy(data[, name]))
  NA
}


#' @rdname donordata_to_individual
#' @note \code{donordata.to.individual()} is deprecated, but exported
#' for legacy reasons
#' @export
donordata.to.individual <- function(con = NULL, dnr, id, ...) {
  .Deprecated("donordata_to_individual")
}
