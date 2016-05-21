
#'Class for individual broken stick estimates
#'
#'The \code{bse} class stores three variables that result from predicting
#'a given broken stick model. The \code{bse} class extend
#'the \code{\link{xyz-class}} class. The \code{bse} class calculates
#'predicted values from the broken stick model fitted to the
#'values specified in the \code{data} argument of the \code{new("bse", data)} function.
#'
#'@section Slots:
#'  \describe{
#'    \item{\code{call}:}{An object of class \code{call} that refers to an object with a pre-fitted broken stick model. Evaluation by \code{eval(call)} should yield an object of class \code{brokenstick.export} as created by
#'    \code{\link[brokenstick]{export.brokenstick}}.}
#'    \item{\code{zscale}:}{A logical indicating whether the broken stick model models the outcome in the Z-scale (\code{zscale = TRUE}, default) or Y-scale (\code{zscale = FALSE}). At initialization, the \code{new()} function tries to infer and set this flag from the model specified by \code{call}.}
#'}
#'
#'@name bse-class
#'@rdname bse-class
#'@aliases bse-class
#'@author Stef van Buuren 2016
#'@seealso \code{\link{xyz-class}}, \code{\link[brokenstick]{export.brokenstick}}
#'@keywords classes
#'@examples
#'# specify three height measures
#'d <- new("bse", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#'# Note that Z-scores are added, relative to nl1997 reference for males
#'d
#'# Same, but now for a female
#'d <- new("bse", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4), sex = "female")
#'d
#'@export
#'@include class-xyz.R

setClass("bse",
         contains = "xyz",
         slots = c(
           call = "ANY",
           zscale = "logical"
         ), prototype = list(
           call = quote(as.numeric(NULL)),
           zscale = TRUE
         )
)

setMethod("initialize", "bse",
          function (.Object, data, type = "curve",
                    models = "bsmodels",
                    call = quote(as.numeric(NULL)),
                    ...) {

            # can't calculate if we have no data
            if (missing(data)) return(.Object)

            # do nothing if data slot is not of class 'xyz'
            if (!inherits(data, "xyz")) return(.Object)

            # copy names
            .Object@xname <- data@xname
            .Object@yname <- data@yname
            .Object@zname <- data@zname

            # direct call specification overides everything else
            if (!missing(call)) slot(.Object, "call") <- as.call(call)
            # else, create new call from models and yname arguments
            else {
              call <- call("[[", as.name(models), data@yname)
              slot(.Object, "call") <- call
            }

            # fetch the model
            model <- eval(call)

            # Was the model in the Z-score scale
            if (is.null(model$zmodel)) .Object@zscale <- TRUE
            else .Object@zscale <- model$zmodel

            # fill y and z
            if (type == "response") .Object@x <- data@x
            else .Object@x <- model$knots

            if (.Object@zscale) {
              .Object@z <- predict(object = model, y = data@z,
                                   age = data@x, type = type)
              .Object@y <- as.numeric(clopus::z2y(z = .Object@z,
                                                  x = .Object@x,
                                                  ref = eval(data@call)))
            }
            else {
              .Object@y <- predict(object = model, y = data@y,
                                   age = data@x, type = type)
              .Object@z <- as.numeric(clopus::y2z(y = .Object@y,
                                                  x = .Object@x,
                                                  ref = eval(data@call)))
            }

            return(.Object)
          }
)


setValidity("bse", function(object) {
  if (!inherits(eval(object@call), "brokenstick.export"))
    return(paste0("eval(", object@call, ") is not an object of class 'brokenstick.export'"))
  return(TRUE)
})

