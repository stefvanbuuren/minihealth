#' Convert json BSD data for single individual to class individual
#'
#' This function takes data from a json source and saves it as a an object
#' of class \linkS4class{individual}. The function automatically calculates
#' standard deviation scores and broken stick conditional means per visit.
#' @param txt a JSON string, URL or file
#' @param schema A JSON string, URL or file that selects the JSON validation
#' schema.
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
#' fn <- system.file("extdata", "allegrosultum", "client3.json", package = "jamestest")
#' p <- convert_bds_individual(fn)
#' @export
convert_bds_individual <- function(txt = NULL, schema = NULL, ...) {

  # PHASE 1: check JSON syntax: if needed, warn and exit
  err <- catch_cnd(d <- fromJSON(txt, ...))
  if (!is.null(err)) {
    warning(conditionMessage(err))
    return(new("individual"))
  }

  # PHASE 2: JSON schema validation
  valid <- validate_bds_individual(txt, schema)
  mess <- parse_valid(valid)

  if (length(mess$required) > 0L) {
    if (any(grepl("required", mess$required)) |
        any(grepl("verplicht", mess$required)) |
        any(grepl("should", mess$required)))
      # throw warnings if required elements are missing
      warning(mess$required, call. = FALSE)
  }
  # inform user about ill-formed BDS elements
  throw_messages(mess$supplied)

  # PHASE 3: Range checks
  r <- check_ranges(d)

  # convert ddi
  ddi <- convert_ddi_gsed(d, r)

  #
  b <- d$ClientGegevens$Elementen

  # is this child or message number?
  pid <- new("individualID",
             id = 0L,
             name = as.character(d$Referentie),
             dob = extract_dob(d),
             src = as.character(d$OrganisatieCode),
             dnr = NA_character_)

  pbg <- new("individualBG",

             sex = extract_sex(b),

             # convert to completed weeks
             ga = trunc(r$ga / 7),

             # 1 = Nee, volgens BDS 1 = Ja, 2 = Nee
             smo = extract_field2(d, 91L, "ClientGegevens", "Elementen") - 1L,

             # in grammen, conform BSD
             bw = r$bw,

             # convert to cm
             hgtm = r$hgtm / 10,

             # convert to cm
             hgtf = r$hgtf / 10,

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
        age = round((r$dom - r$dob) / 365.25, 4L),
        hgt = r$hgt / 10,
        wgt = r$wgt / 1000,
        hdc = r$hdc / 10,
        stringsAsFactors = FALSE)
    time$bmi <- time$wgt / (time$hgt / 100)^2
  }

  if (is.null(time)) {
    pan <- new("individualAN")
    pbs <- new("individualBS")
    pds <- new("individualDS")
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

    ds <- dscore(data = ddi, key = "dutch")
    pds <- new("individualDS",
               mst = ddi,
               dfa = new("xyz", x = ds$a, y = ds$d, z = ds$daz, yname = "dfa"),
               key = "dutch",
               population = "dutch")
  }

  new("individual", pid, pbg, pan, pbs, pds)
}

extract_dob <- function(d) {
  b <- d$ClientGegevens$Elementen
  dob <- ymd(b[b$Bdsnummer == 20, 2])
  if (length(dob) == 0L) return(as.Date(NA))
  dob
}

extract_sex <- function(b) {
  s <- b[b$Bdsnummer == 19L, 2L]
  if (length(s) == 0L) return(NA_character_)
  switch(s,
       "1" = "male",
       "2" = "female",
       NA_character_)
}

extract_agep <- function(d, which_parent = "02") {
  # returns age of parent in completed years
  # which_parent: "01" = father, "02" = mother
  dob <- extract_dob(d)
  p <- d$ClientGegevens$Groepen[[1]]
  if (is.null(p)) return(NA_real_)
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62, "Waarde"]
    if (parent == which_parent) {
      dobp <- ymd(pp[pp$Bdsnummer == 63, 2])
      agep <- as.numeric(trunc(difftime(dob, dobp, "days")/365.25))
      aa <- 1
      if (length(agep) == 0L) agep <- NA_real_
      return(agep)
    }
  }
  NA_real_
}

extract_field <- function(d, f = 245L) {
  z <- d$Contactmomenten[[2L]]
  as.numeric(unlist(lapply(z, function(x, f2 = f)
    ifelse("Waarde" %in% names(x), x[x$Bdsnummer == f2, "Waarde"], NA))))
}

extract_ga <- function(b) {
  ga <- as.numeric(b[b$Bdsnummer == 82, 2])
  if (length(ga) == 0L) ga <- NA_real_
  # convert days to weeks
  if (!is.na(ga) & ga > 50) ga <- trunc(ga / 7)
  as.numeric(ga)
}

extract_field2 <- function(d, f, l1, l2) {
  b <- d[[l1]][[l2]]
  v <- b[b$Bdsnummer == f, "Waarde"]
  ifelse (length(v) == 0L, NA_real_, as.numeric(v))
}

extract_field3 <- function(d, f, l1, l2, l3, which_parent = "02") {
  p <- d[[l1]][[l2]][[l3]]
  if (is.null(p)) return(NA)
  for (i in 1L:length(p)) {
    pp <- p[[i]]
    parent <- pp[pp$Bdsnummer == 62L, "Waarde"]
    if (parent == which_parent) return(pp[pp$Bdsnummer == f, 2L])
  }
}
