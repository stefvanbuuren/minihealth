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

  # required elements
  bds <- list(
    OrganisatieCode = 0L,
    ClientGegevens  = as_bds_clientdata(ind))

  # optional elements
  bds$Referentie <- as_bds_reference(ind)
  bds$Contactmomenten <- as_bds_contacts(ind)

  result <- toJSON(bds, auto_unbox = TRUE)
  if (validate(result)) return(result)
  NULL
}

as_bds_reference <- function(ind) {
  if (length(slot(ind, "name")) < 1L | is.na(slot(ind, "name"))) return(NULL)
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

  x$Elementen[1L, 2L] <- switch(slot(ind, "sex"), "male" = "1", "female" = "2", NA_character_)
  x$Elementen[2L, 2L] <- format(as.Date(slot(ind, "dob"), format = "%d-%m-%y"), format = "%Y%m%d")
  x$Elementen[3L, 2L] <- as.character(slot(ind, "ga"))
  x$Elementen[4L, 2L] <- as.character(slot(ind, "smo") + 1L)
  x$Elementen[5L, 2L] <- as.character(slot(ind, "bw"))
  x$Elementen[6L, 2L] <- as.character(slot(ind, "hgtm") * 10)
  x$Elementen[7L, 2L] <- as.character(slot(ind, "hgtf") * 10)

  keep <- !is.na(x$Elementen[, 2L])
  if (!any(keep)) x$Elementen <- NULL
  else x$Elementen <- x$Elementen[keep, ]

  if (is.na((x$Groepen[2, ])[[1]][1,2])) x$Groepen <- NULL
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

  # return NULL if there are no measurements
  if (nrow(z) == 0L) return(NULL)

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
  z <- gather(z, key = "Bdsnummer", value = "Waarde", "235", "245", "252") %>%
    drop_na() %>%
    mutate(Bdsnummer = as.integer(.data$Bdsnummer),
           Waarde = as.character(.data$Waarde)) %>%
    arrange(.data$time, .data$Bdsnummer)

  f <- as.factor(z$time)
  z <- split(z[, c("Bdsnummer", "Waarde")], f)

  data.frame(
    Tijdstip = names(z),
    Elementen = I(z),
    stringsAsFactors = FALSE)
}
