#' An S4 class to represent individual anthropometric data
#'
#' The \code{individualAN} class stores anthropometric measures as
#' the collection of \code{xyz}-class for height, weight,
#' head circumference, bmi, and weight for height.
#' @slot hgt  Length/height in cm (\code{xyz})
#' @slot wgt  Weight in kg (\code{xyz})
#' @slot hdc  Head circumference in cm (\code{xyz})
#' @slot bmi  Body mass index kg/m**2 (\code{xyz})
#' @slot wfh  Weight for height kg/m (\code{xyz})
#' @slot dsc  D-score (D) (\code{xyz})
#' @seealso \code{\link{xyz-class}}
#' @examples
#' # create object with height and weight measures
#' # do not calculate Z-scores since reference is undefined
#' z <- new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0.5), y = c(50, 70)),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0.3), y = c(3, 6)))
#' z
#'
#' # calculate -2 and +2 centiles for height and head circumference
#' # using the WHO Child Growth Standard for girls
#' hgtref <- clopus::create.reference.call(libname = "clopus::who",
#'   prefix = "who2011", sex = "female", yname = "hgt", sub = "")
#' hdcref <- clopus::create.reference.call(libname = "clopus::who",
#'   prefix = "who2011", sex = "female", yname = "hdc", sub = "")
#' new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0, 0.25, 0.25),
#'        z = c(-2, 2, -2, 2), call = hgtref),
#'      hdc = new("xyz", yname = "wgt", x = c(0, 0, 1, 1),
#'        z = c(-2, 2, -2, 2), call = hdcref))
#' @author Stef van Buuren 2016-2020
setClass("individualAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz",
           bmi = "xyz",
           wfh = "xyz",
           dsc = "xyz"),
         prototype = list(
           hgt = new("xyz", yname = "hgt"),
           wgt = new("xyz", yname = "wgt"),
           hdc = new("xyz", yname = "hdc"),
           bmi = new("xyz", yname = "bmi"),
           wfh = new("xyz", yname = "wfh", xname = "hgt"),
           dsc = new("xyz", yname = "dsc")
         )
)

# #' as("individualAN", "data.frame")
# #'
# #' @name as
# #' @family individualAN
# setAs("individualAN", "data.frame", function(from) {
#   # note: preserve only first row in case of duplicate ages
#   ynames <- c("hgt", "wgt", "hdc", "bmi", "wfh")
#   hgt <- as(from@hgt, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#   wgt <- as(from@wgt, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#   hdc <- as(from@hdc, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#   bmi <- as(from@bmi, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#   dsc <- as(from@dsc, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#   # wfh <- as(from@wfh, "data.frame") %>% distinct(.data$age, .keep_all = TRUE)
#
#   m <- full_join(hgt, wgt, by = "age")
#   m <- full_join(m, hdc, by = "age")
#   m <- full_join(m, bmi, by = "age")
#   m <- full_join(m, dsc, by = "age")
#   # m <- full_join(m, wfh, by = "age")  # cannot merge by age
#   arrange(m, .data$age)
# }
# )

#' as("individualAN", "data.frame")
#'
#' @name as
#' @family individualAN
setAs("individualAN", "data.frame", function(from) {

  sn <- slotNames("individualAN")
  df <- vector("list", length(sn))
  for (i in seq_along(sn))
    df[[i]] <- as(slot(from, sn[i]), "data.frame")
  do.call(rbind.data.frame, df)
})
