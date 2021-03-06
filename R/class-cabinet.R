#'@include class-individual.R
NULL

#'Cabinet class
#'
#'A collection of objects of class \code{individual} can be stored
#'into the class \code{cabinet} class.
#'The \code{cabinet} object can be generated by the \code{new("cabinet", ...)} and
#'\code{as(data, "cabinet")} functions.
#'The result is essentially a list of \code{individual} objects with an index.
#'Data of individual persons can be assessed by position by as list index
#'\code{`[[`} or by unique id by the vector index \code{`[`}.
#'@name cabinet-class
#'@rdname cabinet-class
#'@slot .Data A list of objects of class \code{individual}
#'@slot ids A number vector the indexes the (unique) individual \code{id}
#'@slot n Number of individuals
#'@slot readonly Logical indicating whether the cabinet is just for viewing
#'(\code{TRUE}) or for editing by the user (\code{FALSE}, the default)
#'@slot created Creation date
#'@slot updated Update date
#'@aliases cabinet-class
#'@author Stef van Buuren 2016
#'@seealso \code{\link[=individual-class]{individual}}
#'@keywords classes
#'@examples
#'# create cabinet with 1 empty record
#'z <- new("cabinet")
#'class(z)
#'slotNames(z)
#'slotNames(z[[1]])
#'
#'# create cabinet with three empty records
#'z <- new("cabinet", n = 3)
#'slotNames(z)
#'
#'# create cabinet with one individual, Sally
#'sally <- new("individual", name = "Sally", sex = "female", id = 22L)
#'z <- new("cabinet", sally)
#'length(z)
#'
#'# create cabinet with two individuals, Harry and Sally
#'harry <- new("individual", name = "Harry", id = as.integer(33))
#'z <- new("cabinet", list(harry = harry, sally = sally))
#'z[["harry"]]@name; z[["harry"]]@sex
#'z[["sally"]]@name; z[["sally"]]@sex
#'
#'\dontrun{
#'# convert all 1933 children of SMOCC in `individual` objects
#'library("donorloader")
#'smocc <- load_data(dnr = "smocc")
#'cab <- as(smocc, "cabinet")
#'smoccdemo <- cab[1:10]
#'
#'# same, but not relative to WHO references
#'cab.who <- list2cabinet(smocc, libname = "who", prefix = "who2011", sub = "")
#'}
#'@export
setClass("cabinet",
         contains = "list",
         representation(
           n       = "integer",
           ids     = "numeric",
           readonly = "logical",
           created = "Date",
           updated = "Date"
         ),
         prototype = prototype(
           list(new("individual")),
           n       = 1L,
           ids     = 0,
           readonly = FALSE,
           created = as.Date(Sys.Date()),
           updated = as.Date(Sys.Date())
         )
)

setMethod("initialize", "cabinet",
          function (.Object, data, n = 1, ...) {

            if (missing(data)) {
              # create list of n S4 objects of class individual
              .Object@.Data <- lapply(rep("individual", n), new)
              # set id to unique number
              for (i in 1:length(.Object@.Data)) .Object@.Data[[i]]@id <- i
              .Object@ids <- sapply(.Object, slot, "id")
              .Object@n <- length(.Object@.Data)
              return(.Object)
            }

            # if data is a list, check whether its elements are of class
            # individual, and
            if (is.list(data)) {
              # if all elements are individual, assign to .Data slot
              if (all(sapply(data, is.individual))) .Object@.Data <- data
              # set n to length of list
              .Object@ids <- sapply(.Object, slot, "id")
              .Object@n <- length(.Object@.Data)
              return(.Object)
            }

            # if data has class individual, just create cabinet with one
            if (is.individual(data)) {
              .Object@.Data <- list(data)
              .Object@ids <- sapply(.Object, slot, "id")
              .Object@n <- length(.Object@.Data)
              return(.Object)
            }
            stop("Cannot create cabinet")
          }
)


setValidity("cabinet", function(object) {
  if (any(duplicated(object@ids)))
    return(paste("Duplicated id's."))
  if (!all(sapply(object, is.individual)))
    return("Some list elements are not of S4 class 'individual'")
  return(TRUE)
})


#' as("list", "cabinet")
#'
#' as() reads the data structure as used in the
#' donordata package, and transforms it into a cabinet object.
#' @name as
#' @family cabinet
setAs("list", "cabinet", function(from) list2cabinet(from))

#' Convert donordata object to cabinet object
#'
#' This function calls the \code{donordata_to_individual} function to
#' transform data of each \code{id} into an object of \code{individual} S4 class,
#' and then collects all individuals into an object of \code{cabinet} S4 class.
#' @param from Longitudinal data in the \code{list} format as used by the \code{donordata} package
#' @param \dots Argument passed down to \code{donordata_to_individual}
#' @family cabinet
#'@author Stef van Buuren 2016
#'@seealso \code{\link{donordata_to_individual}}
#' @export
list2cabinet <- function(from, ...) {
  if (length(from) != 2) stop("Data type not from donordata")
  ids <- from[[2]]$id
  n <- length(ids)
  cab <- new("cabinet", n = length(ids))

  for (i in 1:length(cab))
    cab[[i]] <- donordata_to_individual(id = ids[i], ...)
  cab@ids <- sapply(cab, slot, "id")
  cab@n <- length(cab@.Data)

  validObject(cab)
  return(cab)
}

#' Is this object of class `cabinet`?
#'
#' @param x An object
#' @return A logical
#' @export
is.cabinet <- function(x)
{
  inherits(x,"cabinet")
}


#'Extract part of cabinet
#'
#'@param x An S4 object of class `cabinet`
#'@param i Indices specifying elements to extract.
#'@param j Not used
#'@param \dots Not used
#'@param drop Not used
setMethod(f = "[", signature(x = "cabinet", i = "ANY", j = "ANY"),
          function (x, i, j, ..., drop = FALSE) {
            cab <- new("cabinet", n = length(i))
            for (k in 1:length(cab)) {
              cab[[k]] <- x[[i[k]]]
            }
            cab@ids <- sapply(cab, slot, "id")
            validObject(cab)
            return(cab)
          }
)

#'Replace part of cabinet
#'
#'@param x An S4 object of class `cabinet`
#'@param i Indices specifying elements to replace
#'@param value An S4 object of class `cabinet` or `individual`
setMethod(f = "[<-", signature(x = "cabinet"),
          definition = function(x, i, value) {

            if (is.individual(value)) {
              x[[i]] <- value
              x@ids <- sapply(x, slot, "id")
              x@updated <- Sys.Date()
              validObject(x)
              return(x)
            }

            if (is.cabinet(value)) {
              if (length(value) != length(i)) stop("Length of arguments i and value do not match")
              for (k in 1:length(value)) x[[i[k]]] <- value[[k]]
              x@ids <- sapply(x, slot, "id")
              x@updated <- Sys.Date()
              validObject(x)
              return(x)
            }
            stop("Incompatible types")
          }
)



# setMethod("show",
#           signature(object = "cabinet"),
#           function (object)
#           {
#             print(cat("Object with", length(object), "individuals.\n"))
#           }
# )

