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
#' lollypop.preterm <- load_data(dnr = "lollypop.preterm")
#' terneuzen <- load_data(dnr = "terneuzen")
#' terneuzen_bs <- load_data(dnr = "terneuzen_bs")
#'
#' p <- donordata_to_individual(dnr = "smocc", id = 10001)
#'
#' # from lollypop.preterm
#' # calculating Z-score relative to preterm growth references
#' q <- donordata_to_individual(dnr = "lollypop.preterm", id = 50001)
#'
#' # overwrite hgt, wgt and hdc slots
#' q@hgt <- new("xyz", x = q@hgt@x, y = q@hgt@y, yname = "hgt",
#'              libname = "clopus::preterm", prefix = "pt2012a",
#'              sex = q@sex, sub = q@ga)
#' q@wgt <- new("xyz", x = q@wgt@x, y = q@wgt@y, yname = "wgt",
#'              libname = "clopus::preterm", prefix = "pt2012a",
#'              sex = q@sex, sub = q@ga)
#' q@hdc <- new("xyz", x = q@hdc@x, y = q@hdc@y, yname = "hdc",
#'              libname = "clopus::preterm", prefix = "pt2012b",
#'              sex = q@sex, sub = q@ga)
#'
#' # update broken stick estimate
#' q@bs.hgt <- new("bse", data = q@hgt)
#' q@bs.wgt <- new("bse", data = q@wgt)
#' q@bs.hdc <- new("bse", data = q@hdc)
#'
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
    mil <- new("individualMS")
  } else {
    pan <- new("individualAN",
               hgt = new("xyz", yname = "hgt",
                         x = time$age,
                         y = time$hgt,
                         sex = pbg@sex,
                         ...),
               wgt = new("xyz", yname = "wgt",
                         x = time$age,
                         y = time$wgt,
                         sex = pbg@sex,
                         ...),
               hdc = new("xyz", yname = "hdc",
                         x = time$age,
                         y = time$hdc,
                         sex = pbg@sex,
                         ...),
               bmi = new("xyz", yname = "bmi",
                         x = time$age,
                         y = time$wgt/(time$hgt/100)^2,
                         sex = pbg@sex,
                         ...),
               wfh = new("xyz", yname = "wfh",
                         xname = "hgt",
                         x = time$hgt,
                         y = time$wgt,
                         sex = pbg@sex,
                         ...),
               dsc = new("xyz",
                         yname = "dsc",
                         x = time$age,
                         y = time$dsc,
                         libname = "clopus::dscore",
                         prefix = "nl2014",
                         sub = ifelse(!is.na(pbg@ga) && pbg@ga < 37,
                                      pbg@ga, 40),
                         sex = pbg@sex,
                         ...))
    pbs <- new("individualBS",
               bs.hgt = new("bse", yname = "hgt",
                            data = pan@hgt,
                            at = "knots",
                            sex = pbg@sex,
                            ...),
               bs.wgt = new("bse", yname = "wgt",
                            data = pan@wgt,
                            at = "knots",
                            sex = pbg@sex,
                            ...),
               bs.hdc = new("bse", yname = "hdc",
                            data = pan@hdc,
                            at = "knots",
                            sex = pbg@sex,
                            ...),
               bs.bmi = new("bse", yname = "bmi",
                            data = pan@bmi,
                            at = "knots",
                            sex = pbg@sex,
                            ...),
               bs.wfh = new("bse", yname = "wfh",
                            xname = "hgt",
                            data = pan@wfh,
                            at = "knots",
                            sex = pbg@sex,
                            ...),
               bs.dsc = new("bse", yname = "dsc",
                            data = pan@dsc,
                            at = "knots",
                            sex = pbg@sex,
                            ...))
    mil <- new("individualMS",
               ddi = new("ird", mst = time,
                         map = load_data(dnr = "smocc_bds"),
                         instrument = "ddi", ...))
  }

  new("individual", pid, pbg, pan, pbs, mil,
      child = child, time = time)
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
  donordata_to_individual(con = con, dnr = dnr, id = id, ...)
}
