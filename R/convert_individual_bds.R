#' Convert data of class individual to bds format
#'
#' This function takes data from an object of class
#' \linkS4class{individual} and saves it into JSON bds format (which
#' can be sent to JAMES).
#'
#' Functions \code{convert_bds_individual()} and
#' \code{convert_individual_bds()} are inverse operations.
#' @param ind Object of class \code{individual}
#' @param \dots Additional parameters. Currently ignored.
#' @return Data in bds format as JSON
#' @author Stef van Buuren 2019
#' @seealso \linkS4class{individual}, \linkS4class{bse}, \linkS4class{xyz},
#'          \code{\link[jsonlite]{toJSON}}
#' @examples
#' data("installed.cabinets", package = "jamestest")
#' ind <- installed.cabinets[[2]][[1]]
#' b <- convert_individual_bds(ind)
#' @export
convert_individual_bds <- function(ind = NULL, ...) {
  bds <- list(
    Referentie      = slot(ind, "name"),
    OrganisatieCode = 0L,
    ClientGegevens  = as_bds_clientdata(ind),
    Contactmomenten = as_bds_contacts(ind)
  )
  bds
}

as_bds_clientdata <- function(ind) {
  if (!is.individual(ind)) stop("Object not of class `individual`.")

  x <- list(
    Elementen = data.frame(
      Bdsnummer = as.integer(c(19, 20, 82, 91, 110, 238, 240)),
      Waarde = NA_character_,
      stringsAsFactors = FALSE),
    Groepen = list(
      data.frame(
        Bdsnummer = as.integer(c(63, 71, 62)),
        Waarde = c(NA_character_, NA_character_, "01"),
        stringsAsFactors = FALSE),
      data.frame(
        Bdsnummer = as.integer(c(63, 71, 62)),
        Waarde = c(NA_character_, NA_character_, "02"),
        stringsAsFactors = FALSE)
    )
  )

  x$Elementen[x$Elementen$Bdsnummer == 19L, 2L] <-
    switch(slot(ind, "sex"),
           "male" = "1",
           "female" = "2",
           NA_character_)

  x$Elementen[x$Elementen$Bdsnummer == 20L, 2L] <-
    format(as.Date(slot(ind, "dob"), format = "%d-%m-%y"), format = "%Y%m%d")

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

  # approximate mother's dob
  x$Groepen[[2]][x$Groepen[[2]]$Bdsnummer == 63L, "Waarde"] <-
    encode_agep(ind)

  x
}

encode_agep <- function(ind, which_parent = "02") {
  # returns approximate dob of mother as YYYYDDMM
  # calculated from mother's age in years and child dob
  # note: not exact!
  # which_parent: "01" = father, "02" = mother
  dob <- as.Date(slot(ind, "dob"), format = "%d-%m-%y")
  aged <- (slot(ind, "agem") + 0.5) * 365.25
  dobm <- format(dob - aged, format = "%Y%m%d")
  dobm
}

as_bds_contacts <- function(ind) {NULL}


