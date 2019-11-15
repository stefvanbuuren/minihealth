#' Convert json BSD data for single individual to class individual
#'
#' This function takes data from a json source and saves it as a an object
#' of class \linkS4class{individual}. The function automatically calculates
#' standard deviation scores and broken stick conditional means per visit.
#' @param txt a JSON string, URL or file
#' @param \dots Additional parameter passed down to
#'   \code{fromJSON(txt, ...)}, \code{new("xyz",... )} and
#'   \code{new("bse",... )}. Useful parameters are \code{models =
#'   "bsmodel"} for setting the broken stick model, or \code{call =
#'   as.call(...)} for setting proper reference standards.
#' @return An object of class \linkS4class{individual}.
#' @author Stef van Buuren 2019
#' @seealso \linkS4class{individual}, \linkS4class{bse}, \linkS4class{xyz},
#'          \code{\link[jsonlite]{fromJSON}}
#' @examples
#' library(donorloader)
#' smocc_bs <- load_data(dnr = "smocc_bs")
#' fn <- file.path(path.package("minihealth"), "testdata", "client3.json")
#' p <- convert_bds_individual(fn)
#' @export
convert_bds_individual <- function(txt = NULL, ...) {

  # json schema validation
  if(!validate_bds_individual(txt, verbose = FALSE)){

    # use string schema as fall-back
    fallback <- validate_bds_individual(txt, verbose = FALSE, schema = "string")
    if(!fallback){
      # Return some kind of error or warning?
    }
  }

  d <- fromJSON(txt, ...)
  b <- d$ClientGegevens$Elementen

  if(exists("fallback")){
    # batch convert all relevant bds numbers?
  } # from here on assume all bds numbers are correct format.. ?

  # is this child or message number?
  pid <- new("individualID",
             id = 0L,
             name = d$Referentie,
             dob = ymd(b[b$Bdsnummer == 20, 2]),
             src = as.character(d$OrganisatieCode),
             dnr = NA_character_)

  pbg <- new("individualBG",

             sex = switch(b[b$Bdsnummer == 19, 2],
                          "1" = "male",
                          "2" = "female",
                          NA_character_),

             # weken, volgens BDS in dagen
             ga = as.numeric(extract_ga(b)),

             # 1 = Nee, volgens BDS 1 = Ja, 2 = Nee
             smo = as.numeric(b[b$Bdsnummer == 91, 2]) - 1,

             # in grammen, conform BSD
             bw = as.numeric(b[b$Bdsnummer == 110, 2]),

             # in mm, conform BSD, convert to cm
             hgtm = as.numeric(b[b$Bdsnummer == 238, 2]) / 10,

             # in mm, conform BSD, convert to cm
             hgtf = as.numeric(b[b$Bdsnummer == 240, 2]) / 10,

             # 510, passief roken, 1 = Nee, 2 = niet als..

             # agem (63 geboortedatum moeder, 62==2)
             agem = extract_agep(d, which_parent = "02"),

             # etn (71?)
             # etn = as.character(b[b$Bdsnummer == 71, 2])
             etn = "NL"

             # edu (66 opleiding moeder, 62==2)

  )

  if (length(d$Contactmomenten) == 0L) {
    time <- data.frame(age = numeric(), hgt = numeric(), wgt = numeric(),
                       hdc = numeric(), bmi = numeric())
  }
  else {
    time <-
      data.frame(
        age = round((ymd(d$Contactmomenten[[1]]) - ymd(pid@dob)) / 365.25, 4),
        hgt = extract_field(d, 235) / 10,
        wgt = extract_field(d, 245) / 1000,
        hdc = extract_field(d, 252) / 10,
        stringsAsFactors = FALSE)
    time$bmi <- time$wgt / (time$hgt / 100)^2
  }

  if (is.null(time)) {
    pan <- new("individualAN")
    pbs <- new("individualBS")
  } else {
    pan <- new("individualAN",
               hgt = new("xyz", yname = "hgt",
                         x = as.numeric(time$age),
                         y = as.numeric(time$hgt),
                         sex = pbg@sex),
               wgt = new("xyz", yname = "wgt",
                         x = as.numeric(time$age),
                         y = as.numeric(time$wgt),
                         sex = pbg@sex),
               hdc = new("xyz", yname = "hdc",
                         x = as.numeric(time$age),
                         y = as.numeric(time$hdc),
                         sex = pbg@sex),
               bmi = new("xyz", yname = "bmi",
                         x = as.numeric(time$age),
                         y = as.numeric(time$bmi),
                         sex = pbg@sex),
               wfh = new("xyz", yname = "wfh",
                         xname = "hgt",
                         x = as.numeric(time$hgt),
                         y = as.numeric(time$wgt),
                         sex = pbg@sex))
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
                            ...))
  }

  new("individual", pid, pbg, pan, pbs)
}

extract_dob <- function(d) {
  b <- d$ClientGegevens$Elementen
  ymd(b[b$Bdsnummer == 20, 2])
}

extract_agep <- function(d, which_parent = "02") {
  # returns age of parent in completed years
  # which_parent: "01" = father, "02" = mother
  dob <- extract_dob(d)
  p <- d$ClientGegevens$Groepen[[1]]
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62, "Waarde"]
    if (parent == which_parent) {
      dobp <- ymd(pp[pp$Bdsnummer == 63, 2])
      agep <- as.numeric(trunc(difftime(dob, dobp, "days")/365.25))
      return(agep)
    }
  }
}

extract_field <- function(d, f = 245) {
  z <- d$Contactmomenten[[2]]
  as.numeric(unlist(lapply(z, function(x, f2 = f)
    ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA))))
}

extract_ga <- function(b) {
  ga <- as.numeric(b[b$Bdsnummer == 82, 2])
  # convert days to weeks
  if (!is.na(ga) & ga > 50) ga <- trunc(ga / 7)
  ga
}
