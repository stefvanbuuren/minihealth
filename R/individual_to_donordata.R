#' Convert object of class 'individual' to donordata
#'
#'This function returns the slots \code{"id"}, \code{"child"} and
#' \code{"time"}, either separate or as a \code{list} from
#' the \code{individual} object. The code
#' assumes that slots \code{"id"}, \code{"child"} and \code{"time"}
#' are up-to-data and consistent with the other slots.
#' @param x An object of class \code{individual}.
#' @param element Requested list elements. Can be \code{"child"},
#'\code{"time"}. The default \code{NULL} returns a list of both
#'elements.
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
individual_to_donordata <- function(x, element = NULL) {
  if (!is.individual(x)) stop("Object not of S4 class 'individual'.")

  if (is.null(element)) element <- ""
  else element <- match.arg(element, c("child", "time"))

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
  if (element == "child") return(child)

  hdc.df <- as(slot(x, "hdc"), "data.frame")
  hgt.df <- as(slot(x, "hgt"), "data.frame")
  wgt.df <- as(slot(x, "wgt"), "data.frame")
  bmi.df <- as(slot(x, "bmi"), "data.frame")

  time <- tibble(
    src  = slot(x, "src"),
    id   = as.numeric(slot(x, "id")),
    rec  = ifelse(nrow(hgt.df) >= 1L, 1L:nrow(hgt.df), integer(0)),
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

  if (element == "time") return(time)

  # if NULL, return list with both elements
  list(child = child, time = time)
}
