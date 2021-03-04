#'@include class-xyz.R
#'@include imports.R
NULL

#' An S4 class for broken stick estimates
#'
#'The \code{bse} class stores three variables that result from predicting
#'a given broken stick model. The \code{bse} class extends
#'the \code{\link{xyz-class}} class. The \code{bse} class calculates
#'predicted values from the broken stick model fitted to the
#'values specified in the \code{data} argument of the
#'\code{new("bse", data)} function.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{call}:}{An object of class \code{call} that refers to an object
#'    with a pre-fitted broken stick model. Evaluation by \code{eval(call)} should
#'    yield an object of class \code{brokenstick} as created by
#'    \code{\link[brokenstick]{brokenstick}}.}
#'    \item{\code{found}:}{Logical indicating whether the broken stick model
#'    specified by \code{call} was actually found.}
#'    \item{\code{zscale}:}{A logical indicating whether the broken stick model
#'    models the outcome in the Z-scale (\code{zscale = TRUE}, default) or Y-scale
#'    (\code{zscale = FALSE}). At initialization, the \code{new()} function tries
#'    to infer and set this flag from the model specified by \code{call}.}
#'}
#'
#'@name bse-class
#'@rdname bse-class
#'@aliases bse-class
#'@author Stef van Buuren
#'@seealso \code{\link{xyz-class}}, \code{\link[brokenstick]{brokenstick}},
#'\code{\link[brokenstick]{predict.brokenstick}}
#'@keywords classes
#'@examples
#'library(donorloader)
#'smocc_bs <- load_data(dnr = "smocc_bs")
#'
#' # specify three height measures for child
#' child <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#' # we get no Z-score conversion because haven't told the child's sex
#' child
#'
#' boy <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "male")
#' girl <- new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "female")
#' boy
#' girl
#'
#' d1 <- new("bse", child)
#' d1
#'
#' # Same, but now for the girl
#' d2 <- new("bse", girl)
#' d2
#'
#'# Calculate predicted value for each x
#'d3 <- new("bse", new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4),
#' sex = "female"))
#'d3
#'
#'d4 <- new("bse", new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4),
#' sex = "female"), at = "knots")
#'d4
#'
#'# use transform_y function instead of reference call
#'# now, -0.126 is converted to 41.2 cm
#' d5 <- new("bse", girl, usetransform = TRUE, ga = 32, sex = "female")
#' d5
#' d5@transform
#'
#'@export
setClass("bse",
         contains = "xyz",
         slots = c(
           call = "language",
           found = "logical",
           zscale = "logical"
         ), prototype = prototype(
           call = quote(as.numeric(NULL)),
           found = FALSE,
           zscale = TRUE
         )
)

setMethod("initialize", "bse",
          function (.Object, data,
                    at = c("x", "knots"),
                    models = "smocc_bs",
                    call = quote(as.numeric(NULL)),
                    refcode = character(0),
                    pkg = "nlreferences",
                    usetransform = FALSE,
                    verbose = FALSE,
                    sex = NA_character_,
                    ga = NA_real_,
                    age = NA_real_,
                    digits = 3L,
                    ...) {
            at <- match.arg(at)

            # store flag
            slot(.Object, "usetransform") <- usetransform
            slot(.Object, "transform") <- "not set"

            # can't calculate if we have no data
            if (missing(data)) data <- new("xyz", ...)

            # do nothing if data slot is not of class 'xyz'
            if (!inherits(data, "xyz")) return(.Object)

            # copy names
            .Object@xname <- data@xname
            .Object@yname <- data@yname
            .Object@zname <- data@zname

            # inherit covariates value from data if not explicitly specified
            .Object@sex <- ifelse(missing(sex), data@sex, sex)
            .Object@ga  <- ifelse(missing(ga), data@ga, ga)
            .Object@age <- ifelse(missing(age), data@age, age)

            # direct call specification overides everything else
            if (!missing(call)) slot(.Object, "call") <- as.call(call)
            # else, create new call from models and yname arguments
            else {
              expr <- parse(text = paste0("load_data(dnr='", models,"', element='", data@yname, "')"))
              call <- call("eval", expr)
              slot(.Object, "call") <- call
            }

            # fetch the model
            model <- eval(call)
            .Object@found <- inherits(model, "brokenstick")

            # fill only if we have a model
            if (.Object@found) {

              # Was the model in the Z-score scale?
              # Not functional: we always model in the Z-score metric
              if (is.null(model$zmodel)) .Object@zscale <- TRUE
              else .Object@zscale <- model$zmodel

              # fill y and z
              if (at == "x") .Object@x <- data@x
              if (at == "knots") .Object@x <- as.numeric(get_knots(model))

              # direct refcode/pkg specification overrides everything else
              # else, create automatic refcode from the data
              if (!missing(refcode)) {
                .Object@refcode <- refcode
              } else {
                df <- data.frame()
                if (length(.Object@x)) {
                  df <- data.frame(xname = data@xname,
                                   yname = data@yname,
                                   x = .Object@x,
                                   sex = .Object@sex,
                                   age = .Object@age,
                                   ga = .Object@ga)
                }
                .Object@refcode <- nlreferences::set_refcodes(df)
              }
              .Object@pkg <- pkg

              if (.Object@zscale) {
                # transform z into y
                df <- data.frame(
                  age = data@x,
                  zname = data@z,
                  id = rep(1L, length(data@x)))
                colnames(df) <- c("age", data@zname, "id")
                if (nrow(df)) {
                  if (at == "x") .Object@z <- predict(model, df, shape = "vector", ...)
                  if (at == "knots") .Object@z <- predict(model, df, x = "knots", shape = "vector", ...)
                }
                else .Object@z <- numeric(0)
                if (length(.Object@z) == 0) {
                  .Object@x <- numeric(0)
                  .Object@y <- numeric(0)
                } else if (usetransform) {
                  slot(.Object, "transform") <- "transform2y()"
                  df <- data.frame(z = .Object@z,
                                   x = .Object@x,
                                   sex = .Object@sex,
                                   ga = .Object@ga)
                  names(df) <- c(paste0(.Object@yname, "_z"), "age", "sex", "ga")
                  .Object@y <-
                    nlreferences::transform2y(df, znames = .Object@zname,
                                              verbose = verbose)[[.Object@yname]]
                } else {
                  .Object@y <- centile::z2y(z = .Object@z,
                                            x = .Object@x,
                                            refcode = .Object@refcode,
                                            pkg = .Object@pkg,
                                            verbose = verbose,
                                            ...)
                }
              } else {
                # transform y into z
                df <- data.frame(
                  age = data@x,
                  yname = data@y,
                  id = rep(1L, length(data@x)))
                colnames(df) <- c("age", data@yname, "id")
                if (nrow(df)) {
                  if (at == "x") .Object@y <- predict(model, df, shape = "vector", ...)
                  if (at == "knots") .Object@y <- predict(model, df, x = "knots", shape = "vector", ...)
                }
                else .Object@y <- numeric(0)
                if (length(.Object@y) == 0) {
                  .Object@x <- numeric(0)
                  .Object@z <- numeric(0)
                } else if (usetransform) {
                  slot(.Object, "transform") <- "transform2z()"
                  df <- data.frame(y = .Object@y,
                                   x = .Object@x,
                                   sex = .Object@sex,
                                   ga = .Object@ga)
                  names(df) <- c(.Object@yname, "age", "sex", "ga")
                  slot(.Object, "z") <-
                    nlreferences::transform2z(df, ynames = .Object@yname,
                                              verbose = verbose)[[paste0(.Object@yname, "_z")]]
                } else {
                  .Object@z <- as.numeric(centile::y2z(y = .Object@y,
                                                       x = .Object@x,
                                                       refcode = .Object@refcode,
                                                       pkg = .Object@pkg,
                                                       verbose = verbose,
                                                       ...))
                }
              }
            }

            check <- validObject(.Object)
            return(.Object)
          }
)


setValidity("bse", function(object) {
  #if (!inherits(eval(object@call), "brokenstick"))
  #  return(paste0("eval(", object@call, ") is not an object of class 'brokenstick'"))
  return(TRUE)
})


setMethod("show",
          signature(object = "bse"),
          function (object) {
            if (!object@found) cat("Broken stick model not found.\n")
            else cat(paste0("donorloader::",
                            strsplit(as.character(object@call[[2]]), '\\[\\[\\"')[[1]][1],
                            "\n"))
            if (!length(object@x)) object@x <- NA_real_
            if (!length(object@y)) object@y <- NA_real_
            if (!length(object@z)) object@z <- NA_real_
            df <- data.frame(object@x, object@y, object@z)
            names(df) <- c(object@xname, object@yname, object@zname)
            print(df)
          }
)


#' as("bse", "data.frame")
#'
#' @name as
#' @family bse
setAs("bse", "data.frame", function(from) {
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
  }
)

#' @export
as.data.frame.bse <-
  function(x, row.names = NULL, optional = FALSE, ...) as(x, "data.frame")
