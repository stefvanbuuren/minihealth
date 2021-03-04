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
#'    Currently restricted to one of \code{c("hgt", "wgt", "hdc", "wfh", "bmi", "dsc")}
#'    for height (in cm), weight (in kg), head circumference (in cm),
#'    weight-for-height (kg/m), body mass index (kg/m^2) and D-score, respectively.}
#'    \item{\code{zname}:}{A character scalar with the name of the \code{z}
#'    variable. By default, is it equal to \code{paste0(yname, "_z")}.}
#'    \item{\code{call}:}{An object of class \code{call} that specifies the
#'    appropriate reference table from the \pkg{clopus} package. Use the function
#'    \code{\link[clopus]{create.reference.call}} to set \code{call} manually.
#'    Alternatively, specify one or more of \code{libname}, \code{prefix},
#'    \code{sex},
#'    \code{yname} and \code{sub} arguments, which are passed down to
#'    \code{create.reference.call()}. DEPRECATED.}
#'    \item{\code{refcode}:}{Since \code{minihealth 0.90.0}, Name of the reference. See
#'    \code{\link[centile]{make_refcode}}.}
#'    \item{\code{pkg}:}{Since \code{minihealth 0.90.0}, Name of the package containing
#'    the reference. See \code{\link[centile]{make_refcode}}. The default is
#'    \code{nlreferences}.}
#'    \item{\code{found}:}{Internal logical flag indicating
#'    whether the reference specified by
#'    \code{call} was actually found.}
#'    \item{\code{usetransform}:}{Logical indicating whether conversion to
#'    and from should be done using \code{clopus::transform_z()} and
#'    \code{clopus::transform_y()}.}
#'    \item{\code{transform}:}{Name of the \code{clopus} transform function.}
#'    \item{\code{sex}:}{Sex specified by user argument.}
#'    \item{\code{ga}:}{Gestational age specified by user argument.}
#'    \item{\code{age}:}{Decimal age specified by user argument.}
#'    \item{\code{digits}:}{Number of digits for \code{round()} function. Default
#'    is 3.}
#'}
#'
#'@name xyz-class
#'@rdname xyz-class
#'@aliases xyz-class
#'@author Stef van Buuren
#'@keywords classes
#'@examples
#'# specify length (in cm) for boy at ages 0, 0.2 and 0.5 years
#'d1 <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#'d1
#'
#'# Z-scores of weight (in kg) at same ages
#'d2 <- new("xyz", x = c(0, 0.2, 0.5), y = c(3.2, 5.2, 7.0), yname = "wgt",
#'   sex = "female")
#'d2
#'
#'# As as data.frame
#'data.frame(d2)
#'
#'# Head circumference of girl, relative to WHO standard
#'d3 <- new("xyz", x = c(0, 0.2, 0.5), y = c(35, 38, 41), pkg = "centile",
#' refcode = "who_2007_hdc_female_")
#'d3
#'data.frame(d3)
#'
#'# Standard weight centiles at age 0.5 year of Dutch girls
#'d5 <- new("xyz", x = rep(0.5, 5), z = -2:2, sex = "female", yname = "wgt")
#'d5
#'
#'# calculate centiles at 1 year using the female WHO head circumference reference
#'d6 <- new("xyz", yname = "hdc", x = rep(1, 5), z = -2:2, pkg = "centile",
#' refcode = "who_2007_hdc_female_")
#'d6
#'
#'# calculate P50 for preterms, female and born a GA week 32
#'d7 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
#'  refcode = "nl_2012_hgt_female_32")
#'d7
#'
#'# use nlreferences::transform2y() as an alternative
#'d8 <- new("xyz", yname = "hgt", x = seq(0, 1, 1/12), z = rep(0, 13),
#'   usetransform = TRUE, ga = 32, sex = "female")
#'d8
#'
#'# use nlreferences::transform2z function
#'d9 <- new("xyz", yname = "hgt", x = seq(0, 0.25, 1/12), y = c(43, 46, 48, 50),
#'   usetransform = TRUE, ga = 32, sex = "female", digits = 10)
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
           refcode = "character",
           pkg = "character",
           usetransform = "logical",
           transform = "character",
           sex = "character",
           ga = "numeric",
           age = "numeric",
           digits = "integer",
           call = "language",  # deprecated
           found = "logical"   # deprecated
         ), prototype = list()
)

setMethod(
  "initialize", "xyz",
  function (.Object, x = numeric(0), y, z,
            xname = "age", yname = "hgt",
            refcode = character(0),
            pkg = "nlreferences",
            usetransform = FALSE,
            verbose = FALSE,
            sex = NA_character_,
            ga = NA_real_,
            age = NA_real_,
            digits = 3L,
            call = NULL,
            ...) {

    if (!missing(call)) {
      stop("Argument `call` is deprecated; please use `refcode` and `pkg` instead.",
           call. = FALSE)
    }

    # store flag
    slot(.Object, "usetransform") <- usetransform
    slot(.Object, "transform") <- "not set"

    # filter sex, ga and age from dots
    # catch <- function(sex = NA_character_,
    #                   ga = NA_real_,
    #                   age = NA_real_, ...) {
    #   list(sex = sex, ga = ga, age = age)
    # }
    # sexga <- catch(...)
    .Object@sex <- sex
    .Object@ga  <- ga
    .Object@age <- age
    .Object@digits <- as.integer(digits)

    # direct refcode/pkg specification overrides everything else
    # else, create automatic refcode from the data
    if (!missing(refcode)) {
      slot(.Object, "refcode") <- refcode
    } else {
      df <- data.frame()
      if (length(x)) {
        df <- data.frame(xname = xname,
                         yname = yname,
                         x = as.numeric(x),
                         sex = .Object@sex,
                         age = .Object@age,
                         ga = .Object@ga)
      }
      slot(.Object, "refcode") <- nlreferences::set_refcodes(df)
    }
    slot(.Object, "pkg") <- pkg

    # obtain reference table
    #ref <- centile::load_reference(refcode,
    #                               pkg = pkg,
    #                               verbose = verbose)
    #.Object@found <- !is.null(ref)

    # initialize x
    slot(.Object, "x") <- as.numeric(x)
    lx <- length(slot(.Object, "x"))

    # initialize y
    if (missing(y)) {
      if (missing(z)) slot(.Object, "y") <- rep(NA_real_, lx)
      else if (usetransform) {
        slot(.Object, "transform") <- "transform2y()"
        df <- data.frame(z = as.numeric(z),
                         x = slot(.Object, "x"),
                         sex = .Object@sex,
                         ga = .Object@ga)
        names(df) <- c(paste0(yname, "_z"), "age", "sex", "ga")
        slot(.Object, "y") <-
          nlreferences::transform2y(df, znames = paste0(yname, "_z"),
                                    verbose = verbose)[[yname]]
      }
      else {
        .Object@y <- centile::z2y(z = as.numeric(z),
                                  x = .Object@x,
                                  refcode = .Object@refcode,
                                  pkg = .Object@pkg,
                                  verbose = verbose,
                                  dec = .Object@digits,
                                  ...)
      }
    }
    else slot(.Object, "y") <- as.numeric(y)
    if (length(slot(.Object, "y")) == 0)
      slot(.Object, "y") <- rep(NA_real_, lx)

    # initialize z
    if (missing(z)) {
      if (missing(y)) slot(.Object, "z") <- rep(NA_real_, lx)
      else if (usetransform && lx) {
        slot(.Object, "transform") <- "transform2z()"
        if (yname != "wfh") {
          df <- data.frame(y = slot(.Object, "y"),
                           x = slot(.Object, "x"),
                           sex = .Object@sex,
                           ga = .Object@ga)
          names(df) <- c(yname, "age", "sex", "ga")
        } else {
          df <- data.frame(y = slot(.Object, "y"),
                           x = slot(.Object, "x"),
                           sex = .Object@sex,
                           ga = .Object@ga,
                           age = .Object@age)
          names(df) <- c("wgt", "hgt", "sex", "ga", "age")
        }
        slot(.Object, "z") <-
          nlreferences::transform2z(df, ynames = yname,
                                    verbose = verbose)[[paste0(yname, "_z")]]
      }
      else {
        .Object@z <- centile::y2z(y = as.numeric(y),
                                  x = .Object@x,
                                  refcode = .Object@refcode,
                                  pkg = .Object@pkg,
                                  verbose = verbose,
                                  dec = .Object@digits,
                                  ...)
      }
    }
    else slot(.Object, "z") <- round(as.numeric(z), .Object@digits)
    if (!length(slot(.Object, "z")))
      slot(.Object, "z") <- rep(NA_real_, lx)

    # administrative names
    .Object@xname <- as.character(xname[1L])
    .Object@yname <- as.character(yname[1L])
    if (!length(.Object@zname)) .Object@zname <- paste0(yname[1L], "_z")

    # check <- validObject(.Object)
    return(.Object)
  }
)

setValidity("xyz", function(object) {
  if (length(object@y) > 1L && length(object@x) != length(object@y))
    return(paste0("Non-conformable length: ",
                  object@xname, " (", length(object@x),") and ",
                  object@yname, " (", length(object@y),")."))
  if (length(object@z) > 1L && length(object@x) != length(object@z))
    return(paste0("Non-conformable length: ",
                  object@xname, " (", length(object@x),") and ",
                  object@zname, " (", length(object@z),")."))
  if (length(object@xname) > 1L) return("xname not of length 1")
  if (length(object@yname) > 1L) return("yname not of length 1")
  if (length(object@zname) > 1L) return("zname not of length 1")
  return(TRUE)
})

setMethod("show", signature(object = "xyz" ),
          function (object) {
            if (object@usetransform) {
              cat(paste0("Transform ", object@pkg, "::", object@transform, "\n"))
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
  if (length(from@x) && from@usetransform)
    return(data.frame(xname = from@xname,
                      yname = from@yname,
                      zname = from@zname,
                      x = from@x,
                      y = from@y,
                      z = from@z,
                      pkg = from@pkg,
                      func = from@transform))
  if (length(from@x) && ! from@usetransform)
    return(data.frame(xname = from@xname,
                      yname = from@yname,
                      zname = from@zname,
                      x = from@x,
                      y = from@y,
                      z = from@z,
                      pkg = from@pkg,
                      refcode = from@refcode))
  data.frame(xname = character(),
             yname = character(),
             zname = character(),
             x = numeric(),
             y = numeric(),
             z = numeric(),
             pkg = character(),
             refcode = character())
})

#' @export
as.data.frame.xyz <-
  function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")

