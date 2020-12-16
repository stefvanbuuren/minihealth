#' An S4 class for anthropometric data
#'
#'The \code{xyz} class stores three variables useful for growth curves:
#'\code{x} (usually age), \code{y} (height, weight, etc) and \code{z} (Z-score,
#'or standard deviation score (SDS)
#'corresponding to \code{y} given \code{x}). The class calculates
#'\code{z} (when \code{y} is specified), or \code{y} (when \code{z} is specified).
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{x}:}{Numeric vector of decimal ages (in years) at which
#'    measurements \code{y} and Z-scores \code{z} are made. The length
#'    of \code{x} determines how many measurements (including \code{NA}'s)
#'    are stored.}
#'    \item{\code{y}:}{Numeric vector of measurement of length \code{length(x)}.}
#'    \item{\code{z}:}{Numeric vector of Z-score of length \code{length(x)}.}
#'    \item{\code{xname}:}{A character scalar with the name of the \code{x}
#'    variable,usually something like \code{"age"}.}
#'    \item{\code{yname}:}{A character scalar with the name of the measurement.
#'    Currently restricted to one of \code{c("hgt", "wgt", "hdc", "bmi", "dsc")}
#'    for height (in cm), weight (in kg), head circumference (in cm),
#'    body mass index and D-score, respectively.}
#'    \item{\code{zname}:}{A character scalar with the name of the \code{z}
#'    variable. By default, is it equal to \code{paste0(yname, ".z")}.}
#'    \item{\code{call}:}{An object of class \code{call} that specifies the
#'    appropriate reference table from the \pkg{clopus} package. Use the function
#'    \code{\link[clopus]{create.reference.call}} to set \code{call} manually.
#'    Alternatively, specify one or more of \code{libname}, \code{prefix},
#'    \code{sex},
#'    \code{yname} and \code{sub} arguments, which are passed down to
#'    \code{create.reference.call()}. See below for examples.}
#'    \item{\code{found}:}{Internal logical flag indicating
#'    whether the reference specified by
#'    \code{call} was actually found.}
#'    \item{\code{usetransform}:}{Logical indicating whether conversion to
#'    and from should be done using \code{clopus::transform_z()} and
#'    \code{clopus::transform_y()}.}
#'    \item{\code{transform}:}{Name of the \code{clopus} transform function.}
#'}
#'
#' @details Changed Feb 2020: The function rounds `z` to 3 digits after the
#' decimal point.
#'@name xyz-class
#'@rdname xyz-class
#'@aliases xyz-class
#'@author Stef van Buuren 2016
#'@seealso \code{\link[clopus]{create.reference.call}}
#'@keywords classes
#'@examples
#'# specify length (in cm) for boy at ages 0, 0.2 and 0.5 years
#'d1 <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
#'d1
#'
#'# Z-scores of weight (in kg) at same ages
#'d2 <- new("xyz", x = c(0, 0.2, 0.5), y = c(3.2, 5.2, 7.0), yname = "wgt",
#'   sex = "female")
#'d2
#'
#'# Obtain reference used to calculate Z-scores in d2
#'eval(d2@call)
#'
#'# List available WHO references in clopus package
#'# find.references() only searches packages in the search list
#'# so let attach clopus first
#'library("clopus")
#'find.reference(libname = "who")
#' # detach("package:clopus")
#'
#'# Head circumference of girl, relative to WHO standard
#'d3 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41),
#'libname = "clopus::who", prefix = "who2011", sex = "female", sub = "", yname = "hdc")
#'d3
#'
#'# Shortcut specification of WHO standard
#'d3@call
#'d4 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41), call = d3@call)
#'d4
#'
#'# Standard weight centiles at age 0.5 year of Dutch girls
#'d5 <- new("xyz", x = rep(0.5, 5), z = -2:2, sex = "f", yname = "wgt")
#'d5
#'
#'# calculate centiles at 1 year using the female WHO head circumference reference
#'ref <- clopus::create.reference.call(libname = "clopus::who", prefix = "who2011",
#'                                 sex = "female", yname = "hdc", sub = "")
#'d6 <- new("xyz", yname = "hdc", x = rep(1, 5), z = -2:2, call = ref)
#'d6
#'
#'# calculate P50 for preterms, female and born a GA week 32
#'ref <- clopus::create.reference.call(libname = "clopus::preterm", prefix = "pt2012a",
#'                                 sex = "female", yname = "hgt", sub = "32")
#'d7 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13), call = ref)
#'d7
#'
#'# use transform_y function instead of reference call
#'d8 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
#'   usetransform = TRUE, ga = 32, sex = "female")
#'d8
#'
#'# use transform_z function
#'d9 <- new("xyz", yname = "hgt", x = seq(0, 0.25, 1/12), y = c(43, 46, 48, 50),
#'   usetransform = TRUE, ga = 32, sex = "female")
#'d9
#'@export
setClass("xyz",
         slots = c(
           x = "numeric",
           y = "numeric",
           z = "numeric",
           xname = "character",
           yname = "character",
           zname = "character",
           call  = "language",
           found = "logical",
           usetransform = "logical",
           transform = "character"
         ), prototype = list(
           call = quote(as.numeric(NULL)),
           found = FALSE)
)

setMethod(
  "initialize", "xyz",
  function (.Object, x = numeric(0), y, z,
            xname = "age", yname = "hgt",
            call = quote(as.numeric(NULL)),
            usetransform = FALSE,
            ...) {

    # store flag
    slot(.Object, "usetransform") <- usetransform
    slot(.Object, "transform") <- "not set"

    # direct call specification overides everything else
    if (!missing(call)) slot(.Object, "call") <- as.call(call)
    # else, create new call from any ... arguments
    else {
      call <- create.reference.call(yname = yname, ...)
      slot(.Object, "call") <- call
    }

    # obtain reference table
    ref <- eval(call)
    .Object@found <- is.reference(ref)

    # parse sex and ga
    catch <- function(sex = NA_character_, ga = NA_real_, ...) {
      sex <- match.arg(sex, c("male", "female", NA_character_))
      list(sex = sex, ga = as.numeric(ga))
    }
    sexga <- catch(...)

    # initialize x
    slot(.Object, "x") <- as.numeric(x)
    lx <- length(slot(.Object, "x"))

    # initialize y
    if (missing(y)) {
      if (missing(z)) slot(.Object, "y") <- rep(NA_real_, lx)
      else if (usetransform) {
        slot(.Object, "transform") <- "transform_y()"
        df <- data.frame(z = as.numeric(z),
                         x = slot(.Object, "x"),
                         sex = sexga$sex,
                         ga = sexga$ga)
        names(df) <- c(paste0(yname, ".z"), "age", "sex", "ga")
        slot(.Object, "y") <-
          as.numeric(transform_y(df, ynames = yname)[[yname]])
      }
      else slot(.Object, "y") <-
          as.numeric(z2y(z = as.numeric(z),
                         x = slot(.Object, "x"),
                         ref = ref))
    }
    else slot(.Object, "y") <- as.numeric(y)
    if (length(slot(.Object, "y")) == 0)
      slot(.Object, "y") <- rep(NA_real_, lx)

    # initialize z
    if (missing(z)) {
      if (missing(y)) slot(.Object, "z") <- rep(NA_real_, lx)
      else if (usetransform && lx) {
        slot(.Object, "transform") <- "transform_z()"
        df <- data.frame(y = slot(.Object, "y"),
                         x = slot(.Object, "x"),
                         sex = sexga$sex,
                         ga = sexga$ga)
        names(df) <- c(yname, "age", "sex", "ga")
        slot(.Object, "z") <-
          as.numeric(transform_z(df, ynames = yname)[[paste0(yname, ".z")]])
      }
      else slot(.Object, "z") <-
          round(as.numeric(y2z(y = as.numeric(y),
                               x = slot(.Object, "x"),
                               ref = ref)), 3L)
    }
    else slot(.Object, "z") <- round(as.numeric(z), 3L)
    if (length(slot(.Object, "z")) == 0)
      slot(.Object, "z") <- rep(NA_real_, lx)

    # administrative names
    .Object@xname <- as.character(xname[1])
    .Object@yname <- as.character(yname[1])
    if (length(.Object@zname) == 0) .Object@zname <- paste0(yname[1], ".z")

    check <- validObject(.Object)
    return(.Object)
  }
)

setValidity("xyz", function(object) {
  if (length(object@y) > 1 && length(object@x) != length(object@y))
    return(paste0("Non-conformable length: ",
                  object@xname, " (", length(object@x),") and ",
                  object@yname, " (", length(object@y),")."))
  if (length(object@z) > 1 && length(object@x) != length(object@z))
    return(paste0("Non-conformable length: ",
                  object@xname, " (", length(object@x),") and ",
                  object@zname, " (", length(object@z),")."))
  if (length(object@xname) > 1) return("xname not of length 1")
  if (length(object@yname) > 1) return("yname not of length 1")
  if (length(object@zname) > 1) return("zname not of length 1")
  return(TRUE)
})

setMethod("show", signature(object = "xyz" ),
          function (object) {
            if (object@usetransform) {
              cat(paste("package: clopus, function:",
                        object@transform, "\n"))
            } else {
              if (!object@found) cat("No reference\n")
              else {
                cat(paste("package: clopus, library:",
                          strsplit(as.character(object@call[[2]]),
                                   '\\[\\[\\"')[[1]][1],
                          ", member:",
                          strsplit(as.character(object@call[[2]]),
                                   '\\"')[[1]][2],
                          "\n"))
              }
            }
            if (length(object@x) | length(object@y) | length(object@z)) {
              df <- data.frame(object@x, object@y, object@z)
              names(df) <- c(object@xname, object@yname, object@zname)
              print(df)
              return()
            }
            cat("No data for ", object@xname, ", ", object@yname,
                " and ", object@zname, "\n", sep = "")
          }
)


#' as("xyz", "data.frame")
#'
#' @name as
#' @family xyz
setAs("xyz", "data.frame", function(from) {
  library <- ifelse(from@found,
                    strsplit(as.character(from@call[[2]]), '\\[\\[\\"')[[1]][1],
                    NA_character_)
  member <- ifelse(from@found,
                   strsplit(as.character(from@call[[2]]), '\\"')[[1]][2],
                   NA_character_)
  if (length(from@x))
    data.frame(xname = from@xname, yname = from@yname, zname = from@zname,
               x = from@x, y = from@y, z = from@z,
               found = from@found,
               library = library,
               member  = member,
               stringsAsFactors = FALSE)
  else
    data.frame(xname = character(), yname = character(), zname = character(),
               x = numeric(), y = numeric(), z = numeric(),
               found = logical(),
               library = character(),
               member = character(),
               stringsAsFactors = FALSE)
})

# OLD VERSION PRE 0.71.0, names the columns with xname/yname/zname
# setAs("xyz", "data.frame", function(from) {
#   df <- data.frame(x = from@x, y = from@y, z = from@z)
#   names(df) <- c(from@xname, from@yname, from@zname)
#   df
#   }
# )

#' @export
as.data.frame.xyz <-
  function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")

