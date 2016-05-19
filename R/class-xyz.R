#'Class for ages, measurements and standard deviation scores
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
#'    measurements \code{y} are made.}
#'    \item{\code{y}:}{Numeric vector of measurement of length \code{length(x)}.}
#'    \item{\code{z}:}{Numeric vector of Z-score of length \code{length(x)}.}
#'    \item{\code{xname}:}{A character scalar with the name of the \code{x}
#'    variable,usually something like \code{"age"}.}
#'    \item{\code{yname}:}{A character scalar with the name of the measurement.
#'    Currently restricted to one of \code{c("hgt", "wgt", "hdc")} for height (in cm),
#'    weight (in kg) or head circumference (in cm), respectively.}
#'    \item{\code{zname}:}{A character scalar with the name of the \code{z}
#'    variable. By default, is it equal to \code{paste0(yname, ".z")}.}
#'    \item{\code{call}:}{An object of class \code{call} that specifies the
#'    appropriate reference table from the \pkg{clopus} package. Use the function
#'    \code{\link[clopus]{create.reference.call}} to set \code{call} manually.
#'    Alternatively, specify one or more of \code{libname}, \code{prefix},
#'    \code{sex},
#'    \code{yname} and \code{sub} arguments, which are passed down to
#'    \code{create.reference.call()}. See below for examples.}
#'}
#'
#'@name xyz-class
#'@rdname xyz-class
#'@aliases yxz-class
#'@author Stef van Buuren 2016
#'@seealso \code{\link[clopus]{create.reference.call}}
#'@keywords classes
#'@examples
#'# specify three height measures
#'new("xyz", x = c(0, 0.2, 0.5), y = c(51.0, 54.1, 63.4))
#'@export

setClass("xyz",
         slots = c(
           x = "numeric",
           y = "numeric",
           z = "numeric",
           xname = "character",
           yname = "character",
           zname = "character",
           call = "ANY"
         ), prototype = list(
           call = quote(as.numeric(NULL))
         )
)

setMethod("initialize", "xyz",
          function (.Object, x, y, z,
                    xname = "age", yname = "hgt",
                    call = quote(as.numeric(NULL)),
                    ...) {

            if (!missing(x)) slot(.Object, "x") <- x
            if (!missing(y)) slot(.Object, "y") <- y
            if (!missing(z)) slot(.Object, "z") <- z

            .Object@xname <- as.character(xname[1])
            .Object@yname <- as.character(yname[1])
            if (length(.Object@zname) == 0) .Object@zname <- paste0(yname[1], ".z")

            # direct call specification overides everything else
            if (!missing(call)) slot(.Object, "call") <- as.call(call)
            # else, create new call from any ... arguments
            else {
              call <- clopus::create.reference.call(yname = yname, ...)
              slot(.Object, "call") <- call
            }

            # try to fill up z if there are y values
            if (length(.Object@z) == 0 && length(.Object@y) > 0)
              slot(.Object, "z") <-
              as.numeric(clopus::y2z(y = slot(.Object, "y"),
                                     x = slot(.Object, "x"),
                                     ref = eval(call)))

            # try to fill up y if there are z values
            if (length(.Object@y) == 0 && length(.Object@z) > 0)
              slot(.Object, "y") <-
              as.numeric(clopus::z2y(z = slot(.Object, "z"),
                                     x = slot(.Object, "x"),
                                     ref = eval(call)))
            return(.Object)
          }
)


setValidity("xyz", function(object) {
  if (length(object@x) > 0) {
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
  }
  return(TRUE)
})
