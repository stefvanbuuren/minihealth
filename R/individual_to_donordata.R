#' Convert object of class 'individual' to donordata
#'
#'This function returns the slots \code{"id"}, \code{"child"} and
#' \code{"time"}, either separate or as a \code{list} from
#' the \code{individual} object. The code
#' assumes that slots \code{"id"}, \code{"child"} and \code{"time"}
#' are up-to-data and consistent with the other slots.
#' @param x An object of class \code{individual}.
#' @param elements Requested list elements. Can be \code{"child"},
#'\code{"time"} or \code{"list"}. The default \code{"list"}
#'returns a list of both elements.
#' @return If \code{elements == "list"}, the function returns
#' a list of \code{"child"} and \code{"time"}. The function
#' returns a \code{"tibble"} for the choices \code{"child"} and
#' \code{"time"}.
#' @author Stef van Buuren 2019
#' @seealso \code{\link{xyz-class}}, \code{\link{bse-class}}
#' @examples
#' library(donorloader)
#' smocc <- load_data(dnr = "smocc")
#' smocc_bs <- load_data(dnr = "smocc_bs")
#' p <- donordata_to_individual(dnr = "smocc", id = 10001)
#' p
#' q <- individual_to_donordata(p)
#' q
#' @export
individual_to_donordata <- function(x, elements = c("list", "child", "time")) {
  if (!is.individual(x)) stop("Object not of S4 class 'individual'.")

  elements <- match.arg(elements)
  child <- tibble(
    src  = slot(x, "src"),
    id   = as.numeric(slot(x, "id")),
    dob  = format(slot(x, "dob"), format = "%d-%m-%y"),
    sex  = slot(x, "sex"),
    etn  = slot(x, "etn"),
    edu  = slot(x, "edu"),
    ga   = slot(x, "ga"),
    bw   = slot(x, "bw"),
    twin = slot(x, "twin"),
    agem = slot(x, "agem"),
    smo  = slot(x, "smo"),
    hgtm = slot(x, "hgtm"),
    hgtf = slot(x, "hgtf"))
  if (elements == "child") return(child)

  hdc.df <- as(slot(x, "hdc"), "data.frame")
  hgt.df <- as(slot(x, "hgt"), "data.frame")
  wgt.df <- as(slot(x, "wgt"), "data.frame")
  bmi.df <- as(slot(x, "bmi"), "data.frame")

  time <- tibble(
    src  = slot(x, "src"),
    id   = as.numeric(slot(x, "id")),
    rec  = 1L:nrow(hgt.df),
    nrec = nrow(hgt.df),
    dob  = format(slot(x, "dob"), format = "%d-%m-%y"),
    dom  = format(slot(x, "dob")  + round(hgt.df$age * 365.25), format = "%d-%m-%y"),
    age  = hgt.df$age,
    sex  = slot(x, "sex"),
    etn  = slot(x, "etn"),
    ga   = slot(x, "ga"),
    bw   = slot(x, "bw"),
    hgt  = hgt.df$hgt,
    wgt  = wgt.df$wgt,
    hdc  = hdc.df$hdc,
    hgt.z = hgt.df$hgt.z,
    wgt.z = wgt.df$wgt.z,
    hdc.z = hdc.df$hdc.z,
    bmi  = bmi.df$bmi,
    bmi.z = bmi.df$bmi.z)

  if (elements == "time") return(time)
  list(child = child, time = time)
}

#' @rdname individual_to_donordata
#' @param type Same as \code{elements}
#' @note \code{individual.to.donordata()} is deprecated, but exported
#' for legacy reasons
#' @export
individual.to.donordata <- function(x, type = NULL) {
  individual_to_donordata(x = x, elements = type)
}
