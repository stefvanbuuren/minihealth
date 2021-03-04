#' An S4 class to represent individual anthropometric data
#'
#' The \code{individualAN} class stores anthropometric measures as
#' the collection of \code{xyz}-class for height, weight,
#' head circumference, bmi, and weight for height.
#' @slot hgt  Length/height in cm (\code{xyz})
#' @slot wgt  Weight in kg (\code{xyz})
#' @slot hdc  Head circumference in cm (\code{xyz})
#' @slot wfh  Weight for height kg/m (\code{xyz})
#' @slot bmi  Body mass index kg/m**2 (\code{xyz})
#' @slot dsc  D-score (D) (\code{xyz})
#' @seealso \code{\link{xyz-class}}
#' @examples
#' # create object with height and weight measures
#' # do not calculate Z-scores since reference is undefined (no sex)
#' z <- new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0.5), y = c(50, 70), verbose = TRUE),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0.3), y = c(3, 6), verbose = TRUE))
#' data.frame(z)
#'
#' # specify sex, and we get a reference
#' z <- new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0.5), y = c(50, 70), sex = "male"),
#'      wgt = new("xyz", yname = "wgt", x = c(0, 0.3), y = c(3, 6), sex = "male"))
#' data.frame(z)
#'
#' # calculate -2 and +2 centiles for height and head circumference
#' # using the WHO Child Growth Standard for girls
#' hgtref <- "who_2006_hgt_female_"
#' hdcref <- "who_2007_hdc_female_"
#' z <- new("individualAN",
#'      hgt = new("xyz", yname = "hgt", x = c(0, 0, 0.25, 0.25),
#'        z = c(-2, 2, -2, 2), refcode = hgtref, pkg = "centile"),
#'      hdc = new("xyz", yname = "wgt", x = c(0, 0, 0.25, 0.25),
#'        z = c(-2, 2, -2, 2), refcode = hdcref, pkg = "centile"))
#' data.frame(z)
#' @author Stef van Buuren 2016-2020
setClass("individualAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz",
           wfh = "xyz",
           bmi = "xyz",
           dsc = "xyz"),
         prototype = list(
           hgt = new("xyz", yname = "hgt"),
           wgt = new("xyz", yname = "wgt"),
           hdc = new("xyz", yname = "hdc"),
           wfh = new("xyz", yname = "wfh", xname = "hgt"),
           bmi = new("xyz", yname = "bmi"),
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
    df[[i]] <- data.frame(slot(from, sn[i]))
  do.call(rbind.data.frame, df)
})

#' @export
as.data.frame.individualAN <-
  function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")
