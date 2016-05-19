# class.r
# 10/02/2013

### SRF: classes for reading Simple Reference Format
library("clopus")

setClass("personID",
         slots = c(
           id    = "integer",
           name  = "character",
           dob   = "POSIXct"
         ),
         prototype = list(
           id    = as.integer(0),
           dob   = as.POSIXct(Sys.Date())
         )
)

setClass("personBG",
         slots = c(
           sex   = "character",
           ga    = "numeric",
           bw    = "numeric",
           mult  = "numeric",
           goodhealth = "logical",

           hgtm  = "numeric",
           wgtm  = "numeric",
           landm = "character",
           edum  = "character",
           agem  = "numeric",
           smo   = "logical",

           hgtf  = "numeric",
           wgtf  = "numeric",
           landf = "character",
           eduf  = "character"
         ), prototype = list(
           sex   = "male"
         )
)




setClass("personAN",
         slots = c(
           hgt = "xyz",
           wgt = "xyz",
           hdc = "xyz"
         )
)

setClass("person",
         contains = c("personID", "personBG", "personAN"),
         slots = c(
           test    = "integer"
         ),
         prototype = list(
           test = as.integer(0)
         )
)

# setGeneric("refresh", function(.Object, ...) {} )
#
# setMethod("refresh", "person",
#           function (.Object, ...) {
#
#             for (name in c("hgt", "wgt", "hdc")) {
#
#               x <- slot(slot(.Object, name), "x")
#               y <- slot(slot(.Object, name), "y")
#               z <- slot(slot(.Object, name), "z")
#
#               slot(slot(.Object, name), "xname") <- "age"
#               slot(slot(.Object, name), "yname") <- name
#               slot(slot(.Object, name), "zname") <- paste0(name, ".z")
#
#               # create new refcall if none is found
#               refcall <- slot(slot(.Object, name), "refcall")
#               if (length(refcall) == 0) {
#                 refcall <- clopus::create.reference.call(
#                   sex = .Object@sex, yname = name)
#                 slot(slot(.Object, name), "refcall") <- refcall
#               }
#
#               # try to fill up z if there are y values
#               if (length(z) == 0 && length(y) > 0)
#                 slot(slot(.Object, name), "z") <-
#                 clopus::y2z(y = slot(slot(.Object, name), "y"),
#                             x = slot(slot(.Object, name), "x"),
#                             ref = eval(refcall))
#
#               # try to fill up y if there are z values
#               if (length(y) == 0 && length(z) > 0)
#                 slot(slot(.Object, name), "y") <-
#                 clopus::z2y(z = slot(slot(.Object, name), "z"),
#                             x = slot(slot(.Object, name), "x"),
#                             ref = eval(refcall))
#             }
#             return(.Object)
#           }
# )
#


# initialize("personBS", function(.Object, bs, ...) {

# }

# matrix(0, nrow = n, ncol = 13 * 3,
#        dimnames = list(NULL,
#                        c(paste("hgt", 0:12, sep = ""),
#                          paste("wgt", 0:12, sep = ""),
#                          paste("hdc", 0:12, sep = ""))))
# )



# setMethod("refresh",
#           signature(object = "personAN"),
#           function (object, ...)
#           {
#             ref <- clopus::find.reference(libname = object@refname, select = TRUE)
#             object@hgt.z <- clopus::y2z(y = object@hgt, x = object@age,
#                                         ref = ref, yname = "hgt",
#                                         sex = object@sex, sub = "NL", drop = TRUE)
#             object@wgt.z <- clopus::y2z(y = object@wgt, x = object@age,
#                                         ref = ref, yname = "wgt",
#                                         sex = object@sex, sub = "NL", drop = TRUE)
#             object@hdc.z <- clopus::y2z(y = object@hdc, x = object@age,
#                                         ref = ref, yname = "hdc",
#                                         sex = object@sex, sub = "NL", drop = TRUE)
#             object
#           }
# )
#
#


#' bse <- matrix(0, nrow = n, ncol = 13 * 3,
#'               dimnames = list(NULL,
#'                               c(paste("hgt", 0:12, sep = ""),
#'                                 paste("wgt", 0:12, sep = ""),
#'                                 paste("hdc", 0:12, sep = ""))))
#' child <- data.frame(child, bse)
#' child$name <- as.character(child$name)
#'
#' time <- data.frame(
#'   src = rep("mykids", n),
#'   id = id,
#'   rec = 1,
#'   nrec = 1,
#'   dob = child$dob,
#'   dom = format(Sys.Date(),"%d-%m-%y"),
#'   age = NA,
#'   sex = child$sex,
#'   etn = child$landm,
#'   ga = child$ga,
#'   bw = child$bw,
#'   hgt = NA,
#'   wgt = NA,
#'   hdc = NA,
#'   breast = NA,
#'   mixed = NA,
#'   bottle = NA,
#'   hgt.z = NA,
#'   wgt.z = NA,
#'   hdc.z = NA,
#'   hgt.z.pt = NA,
#'   wgt.z.pt = NA,
#'   hdc.z.pt = NA)
#' time$hgt <- as.numeric(time$hgt)
#' time$wgt <- as.numeric(time$wgt)
#' time$hdc <- as.numeric(time$hdc)
#'
#' list(id = id, child = child, time = time)
#' }
#'
#'
#'
#' setClass("SRF",
#'          representation(
#'            info       = "SRFInfo",
#'            raw        = "data.frame",
#'            tables     = "list"
#'          ))
#'
#' setClass("SRFlist",
#'          contains   = "list",
#'          representation(
#'            names  = "character",
#'            index  = "data.frame"
#'          ))
#'
#' ## class reference for actual use
#'
#'
#' setClass("NO", representation(
#'   x = "numeric",
#'   mean = "numeric",
#'   sd = "numeric"
#' ))
#'
#' setClass("LMS", representation(
#'   x = "numeric",
#'   L = "numeric",
#'   M = "numeric",
#'   S = "numeric"
#' ))
#'
#' setClass("PCT", representation(
#'   x = "numeric",
#'   p = "matrix"  # matrix
#' ))
#'
#'
#' setClass("MEA", representation(
#'   x = "numeric",
#'   mean = "numeric"
#' ))
#'
#'
#' setClass("MP", representation(
#'   x = "numeric",
#'   mean = "numeric",
#'   p = "numeric"
#' ))
#'
#' setClass("BCCG", representation(
#'   x = "numeric",
#'   nu = "numeric",
#'   mu = "numeric",
#'   sigma = "numeric"
#' ))
#'
#' setClass("BCPE", representation(
#'   x = "numeric",
#'   nu = "numeric",
#'   mu = "numeric",
#'   sigma = "numeric",
#'   tau = "numeric"
#' ))
#'
#' setClass("BCT", representation(
#'   x = "numeric",
#'   nu = "numeric",
#'   mu = "numeric",
#'   sigma = "numeric",
#'   tau = "numeric"
#' ))
#'
#' #'ReferenceInfo class
#' #'
#' #'The \code{referenceInfo} object contains descriptive information about reference values.
#' #'The object is primarily used to define a slot in the \code{\link[=reference-class]{reference}}
#' #'object.
#' #'
#' #'@section Slots:
#' #'  \describe{
#' #'    \item{\code{n}:}{An integer defining the sequence number within the encapsulating
#' #'    \code{referenceList} object.}
#' #'    \item{\code{i}:}{An integer defining the study number within the encapsulating
#' #'    \code{referenceList} object.}
#' #'    \item{\code{j}:}{An integer defining the substudy number within the encapsulating
#' #'    \code{referenceList} object.}
#' #'    \item{\code{code}:}{A string with the study code. When the references were read by
#' #'    \code{import.references()}, then \code{code} corresponds to the file name
#' #'    containing the original reference values.}
#' #'    \item{\code{country}:}{The name of the country, coded as a two-letter or three-letter
#' #'    abbreviation according to \url{http://www.worldatlas.com/aatlas/ctycodes.htm}.}
#' #'    \item{\code{year}:}{The year to which the references apply. Preferably, this is the year
#' #'    in which the measurements were taken, but sometimes \code{year} refers to the publication year.}
#' #'    \item{\code{sex}:}{A string \code{"male"} or \code{"female"}.}
#' #'    \item{\code{sub}:}{A string indicating any subgroups, e.g. ethnic minorities or weeks of
#' #'    gestational age. The empty string \code{""} indicates no subgroup.}
#' #'    \item{\code{yname}:}{A three-letter string indicator the type of outcome measurement.
#' #'    The following are in use: \code{hgt} (height), \code{wgt} (body weight), \code{bmi} (body
#' #'    mass index), \code{hdc} (head circumference), \code{wfh} (weight for height),
#' #'    \code{hip} (hip circumference), \code{wst} (waist circumference),
#' #'    \code{whr} (waist/hip ratio), \code{lgl} (leg length),
#' #'    \code{sit} (sitting height) and \code{shh} (sitting height/height ratio).}
#' #'    \item{\code{dist}:}{A string coding the type of distribution of the references.
#' #'    The following are in use: \code{NO} (normal distribution), \code{LMS} (LMS model),
#' #'    \code{PCT} (percentiles, non-parametric), \code{MEA} (mean only),
#' #'    \code{MP} (mean-percentiles, non-parametric), \code{BCCG} (Box-Cox
#' #'    Cole-Green distribution), \code{BCPE} (Box-Cox Power Exponential) and
#' #'    \code{BCT} (Box-Cox T-distribution).}
#' #'    \item{\code{mnx}:}{The minimum value of the \code{x} in the references. This is
#' #'    calculated by \code{import.references()}.}
#' #'    \item{\code{mxx}:}{The maximum value of the \code{x} in the references. This is
#' #'    calculated by \code{import.references()}.}
#' #'    \item{\code{source}:}{A character variable containing an abbreviation of the literature
#' #'    reference from which the values were taken.}
#' #'    \item{\code{public}:}{A character variable containing the literature
#' #'    reference from which the values were taken.}
#' #'    \item{\code{remark}:}{A free text field with remarks.}
#' #'    \item{\code{date}:}{An object of class \code{POSIXct} with the date at which the
#' #'    reference values were created. This is automatically added when a new object of
#' #'    class \code{reference} is created.}
#' #'}
#' #'
#' #'@name referenceInfo-class
#' #'@rdname referenceInfo-class
#' #'@aliases referenceInfo-class
#' #'@author Stef van Buuren 2013
#' #'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}},
#' #'\code{\link[=referenceTable-class]{referenceTable}}
#' #'@keywords classes
#' #'@export
#' setClass("referenceInfo",
#'          representation(
#'            n         = "numeric",
#'            i         = "numeric",
#'            j         = "numeric",
#'            code      = "character",
#'            country   = "character",
#'            year      = "numeric",
#'            sex       = "character",
#'            sub       = "character",
#'            yname     = "character",
#'            dist      = "character",
#'            mnx       = "numeric",
#'            mxx       = "numeric",
#'            source    = "character",
#'            public    = "character",
#'            remark    = "character",
#'            date      = "POSIXct"),
#'          prototype = list(
#'            date      = Sys.time()
#'          )
#' )
#'
#'
#' #'ReferenceTable class
#' #'
#' #'The \code{referenceTable} object contains the reference values and the distribution type.
#' #'The \code{referenceTable} object is generated by bla bla.
#' #'
#' #'@section Slots:
#' #'  \describe{
#' #'    \item{\code{table}:}{A \code{list} of objects, typically sublist with elements
#' #'    named \code{x}, \code{mean}, \code{sd}, \code{L}, \code{M}, \code{S}, and so on.}
#' #'    \item{\code{dist}:}{A string identifying the name of the distribution of the values,
#' #'    usually \code{"NO"}, \code{"LMS"}, \code{"PCT"}, and so on.}
#' #'    }
#' #'
#' #'@name referenceTable-class
#' #'@rdname referenceTable-class
#' #'@aliases referenceTable-class
#' #'@author Stef van Buuren 2013
#' #'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}},
#' #'\code{\link[=referenceInfo-class]{referenceInfo}}
#' #'@keywords classes
#' #'@export
#' setClass("referenceTable",
#'          representation(
#'            table = "list",
#'            dist = "character"
#'          )
#' )
#'
#'
#' #'Reference class
#' #'
#' #'The \code{reference} object contains the reference values and descriptive information
#' #'about the values. The \code{reference} object is
#' #'generated by the \code{import.references()}, \code{find.reference()}
#' #'and \code{as.reference()} functions.
#' #'
#' #'@section Slots:
#' #'  \describe{
#' #'    \item{\code{table}:}{An object of class \code{referenceTable}}
#' #'    \item{\code{info}:}{An object of class \code{referenceInfo}}
#' #'    }
#' #'
#' #'@name reference-class
#' #'@rdname reference-class
#' #'@aliases reference-class
#' #'@author Stef van Buuren 2013
#' #'@seealso \code{\link{import.references}}, \code{\link[=referenceTable-class]{referenceTable}},
#' #'\code{\link[=referenceInfo-class]{referenceInfo}}
#' #'@keywords classes
#' #'@export
#' setClass("reference",
#'          representation(
#'            table = "referenceTable",
#'            info  = "referenceInfo"
#'          ))
#'
#'
#' #'ReferenceList class
#' #'
#' #'A collection of references can be stored as an object of the class \code{referenceList}.
#' #'The \code{referenceList} object can be generated by
#' #'by the \code{import.references()}, \code{find.reference()}
#' #'and \code{as.reference()} functions.
#' #'
#' #'@name referenceList-class
#' #'@rdname referenceList-class
#' #'@section Slots:
#' #'  \describe{
#' #'    \item{\code{.Data}:}{A list}
#' #'    \item{\code{names}:}{A vector of names used to label references}
#' #'    \item{\code{index}:}{A \code{data.frame} with administrative information.}
#' #'    }
#' #'@aliases referenceList-class
#' #'@author Stef van Buuren 2013
#' #'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}}
#' #'@keywords classes
#' #'@export
#' setClass("referenceList",
#'          contains = "list",
#'          representation(
#'            names  = "character",
#'            index  = "data.frame"
#'          ))
#'
#'
#' #' Is this object of class SRF (simple reference format)
#' #'
#' #' @aliases is.SRF
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.SRF <- function(x)
#' {
#'   inherits(x,"SRF")
#' }
#'
#' #' Is this object of class SRFlist (simple reference format list)
#' #'
#' #' @aliases is.SRFlist
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.SRFlist <- function(x)
#' {
#'   inherits(x,"SRFlist")
#' }
#'
#' #' Is this object of class reference
#' #'
#' #' @aliases is.reference
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.reference <- function(x)
#' {
#'   inherits(x,"reference")
#' }
#'
#' #' Is this object of class referenceList
#' #'
#' #' @aliases is.referenceList
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.referenceList <- function(x)
#' {
#'   inherits(x,"referenceList")
#' }
#'
#'
#' #' Is this object of class referenceTable
#' #'
#' #' @aliases is.referenceTable
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.referenceTable <- function(x)
#' {
#'   inherits(x,"referenceTable")
#' }
#'
#' #' Is this object of class referenceInfo
#' #'
#' #' @aliases is.referenceInfo
#' #' @param x An object
#' #' @return A logical
#' #' @export
#' is.referenceInfo <- function(x)
#' {
#'   inherits(x,"referenceInfo")
#' }
