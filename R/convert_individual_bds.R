#' Convert data of class individual to bds format
#'
#' This function takes data from an object of class
#' \linkS4class{individual} and saves it into JSON bds format (which
#' can be sent to JAMES).
#'
#' Functions \code{convert_bds_individual()} and
#' \code{convert_individual_bds()} are inverse operations.
#'
#' If the slot \code{slot(ind, "dob")} is not set, the conversion
#' uses the artificial birth date \code{01 Jan 2000}.
#' @param ind Object of class \code{individual}
#' @param \dots Additional parameters. Currently ignored.
#' @return Data in bds format as JSON, or \code{NULL} for invalid
#' JSON
#' @author Stef van Buuren 2019
#' @seealso \linkS4class{individual}, \linkS4class{bse}, \linkS4class{xyz},
#'          \code{\link[jsonlite]{toJSON}}
#' @examples
#' data("installed.cabinets", package = "jamestest")
#' ind <- installed.cabinets[[3]][[8]]
#' b <- convert_individual_bds(ind)
#' @export
convert_individual_bds <- function(ind = NULL, ...) {
  if (!is.individual(ind)) stop("Object not of class `individual`.")
  bds <- list(
    Referentie      = as_bds_reference(ind),
    OrganisatieCode = 0L,
    ClientGegevens  = as_bds_clientdata(ind),
    Contactmomenten = as_bds_contacts(ind)
  )
  result <- toJSON(bds, auto_unbox = TRUE)
  if (validate(result)) return(result)
  NULL
}

as_bds_reference <- function(ind) {
  if (length(slot(ind, "name")) < 1L | is.na(slot(ind, "name"))) return('')
  else slot(ind, "name")
}

as_bds_clientdata <- function(ind) {

  x <- list(
    Elementen = data.frame(
      Bdsnummer = as.integer(c(19, 20, 82, 91, 110, 238, 240)),
      Waarde = NA_character_,
      stringsAsFactors = FALSE),
    Groepen = data.frame(
      rbind(
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(NA_character_, NA_character_, "01"),
          stringsAsFactors = FALSE)),
        list(Elementen = data.frame(
          Bdsnummer = as.integer(c(63, 71, 62)),
          Waarde = c(encode_agep(ind), NA_character_, "02"),
          stringsAsFactors = FALSE))))
  )

  x$Elementen[x$Elementen$Bdsnummer == 19L, 2L] <-
    switch(slot(ind, "sex"),
           "male" = "1",
           "female" = "2",
           NA_character_)

  x$Elementen[x$Elementen$Bdsnummer == 20L, 2L] <-
    format(as.Date(get_dob(ind), format = "%d-%m-%y"), format = "%Y%m%d")

  x$Elementen[x$Elementen$Bdsnummer == 82L, 2L] <-
    as.character(slot(ind, "ga"))

  x$Elementen[x$Elementen$Bdsnummer == 91L, 2L] <-
    as.character(slot(ind, "smo") + 1L)

  x$Elementen[x$Elementen$Bdsnummer == 110L, 2L] <-
    as.character(slot(ind, "bw"))

  # height mother mm
  x$Elementen[x$Elementen$Bdsnummer == 238L, 2L] <-
    as.character(slot(ind, "hgtm") * 10)

  # height father mm
  x$Elementen[x$Elementen$Bdsnummer == 240L, 2L] <-
    as.character(slot(ind, "hgtf") * 10)

  x
}

encode_agep <- function(ind, which_parent = "02") {
  # returns approximate dob of mother as YYYYDDMM
  # calculated from mother's age in years and child dob
  # note: not exact!
  # which_parent: "01" = father, "02" = mother
  dob  <- as.Date(get_dob(ind), format = "%d-%m-%y")
  aged <- (slot(ind, "agem") + 0.5) * 365.25
  dobm <- format(dob - aged, format = "%Y%m%d")
  dobm
}


as_bds_contacts <- function(ind) {
  # this function produces a JSON string with data coded according
  # to the BDS schema

  # extract measurements
  z <- new("individualAN", hgt = ind@hgt, wgt = ind@wgt, hdc = ind@hdc,
           bmi = ind@bmi, wfh = ind@wfh)
  z <- as(z, "data.frame")[, c("age", "hgt", "wgt", "hdc")]

  # calculate measurement dates
  dob <- as.Date(get_dob(ind), format = "%d-%m-%y")
  days <- round(z$age * 365.25)
  z$age <- format(dob + days, format = "%Y%m%d")

  # set proper units
  z$hgt <- z$hgt * 10
  z$wgt <- z$wgt * 1000
  z$hdc <- z$hdc * 10
  colnames(z) <- c("time", "235", "245", "252")

  # remove duplicate rows
  z <- z %>% distinct(.data$time, .keep_all = TRUE)

  # reshuffle
  z <- tidyr::gather(z, key = "Bdsnummer", value = "Waarde", "235", "245", "252") %>%
    dplyr::mutate(Bdsnummer = as.integer(.data$Bdsnummer),
                  Waarde = as.character(.data$Waarde)) %>%
    dplyr::arrange(.data$time, .data$Bdsnummer)
  # NOTE: here we should delete rows with missing values

  f <- as.factor(z$time)
  z <- split(z[, c("Bdsnummer", "Waarde")], f)

  data.frame(
    Tijdstip = names(z),
    Elementen = I(z),
    stringsAsFactors = FALSE)
}

get_dob <- function(ind) {
  if (is.na(slot(ind, "dob"))) return("01-01-00")
  slot(ind, "dob")
}
