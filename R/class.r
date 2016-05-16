# class.r
# 10/02/2013

### SRF: classes for reading Simple Reference Format 

setClass("SRFInfo", 
         representation(
             study       = "numeric",
             filename    = "character",
             code        = "character",
             country     = "character",
             year        = "numeric",
             sex         = "character",
             sub         = "character",
             weight      = "character",
             source      = "character",
             publication = "character",
             remark      = "character",
             mnx         = "numeric",
             mxx         = "numeric",
             varnames    = "character",
             missing     = "character"), 
         prototype = list(sub = "")
)

setClass("SRF", 
         representation(
             info       = "SRFInfo",
             raw        = "data.frame",
             tables     = "list"
         ))

setClass("SRFlist", 
         contains   = "list", 
         representation(
             names  = "character", 
             index  = "data.frame"
         ))

## class reference for actual use


setClass("NO", representation(
    x = "numeric",
    mean = "numeric",
    sd = "numeric"
))

setClass("LMS", representation(
    x = "numeric",
    L = "numeric",
    M = "numeric",
    S = "numeric"
    ))

setClass("PCT", representation(
    x = "numeric",
    p = "matrix"  # matrix
))


setClass("MEA", representation(
    x = "numeric",
    mean = "numeric"
))


setClass("MP", representation(
    x = "numeric",
    mean = "numeric",
    p = "numeric"
))

setClass("BCCG", representation(
    x = "numeric",
    nu = "numeric",
    mu = "numeric",
    sigma = "numeric"
))

setClass("BCPE", representation(
    x = "numeric",
    nu = "numeric",
    mu = "numeric",
    sigma = "numeric",
    tau = "numeric"
))

setClass("BCT", representation(
    x = "numeric",
    nu = "numeric",
    mu = "numeric",
    sigma = "numeric",
    tau = "numeric"
))

#'ReferenceInfo class
#'
#'The \code{referenceInfo} object contains descriptive information about reference values.
#'The object is primarily used to define a slot in the \code{\link[=reference-class]{reference}}
#'object.  
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{n}:}{An integer defining the sequence number within the encapsulating 
#'    \code{referenceList} object.}
#'    \item{\code{i}:}{An integer defining the study number within the encapsulating 
#'    \code{referenceList} object.}
#'    \item{\code{j}:}{An integer defining the substudy number within the encapsulating 
#'    \code{referenceList} object.}
#'    \item{\code{code}:}{A string with the study code. When the references were read by 
#'    \code{import.references()}, then \code{code} corresponds to the file name 
#'    containing the original reference values.}
#'    \item{\code{country}:}{The name of the country, coded as a two-letter or three-letter
#'    abbreviation according to \url{http://www.worldatlas.com/aatlas/ctycodes.htm}.}
#'    \item{\code{year}:}{The year to which the references apply. Preferably, this is the year 
#'    in which the measurements were taken, but sometimes \code{year} refers to the publication year.}
#'    \item{\code{sex}:}{A string \code{"male"} or \code{"female"}.}
#'    \item{\code{sub}:}{A string indicating any subgroups, e.g. ethnic minorities or weeks of 
#'    gestational age. The empty string \code{""} indicates no subgroup.}
#'    \item{\code{yname}:}{A three-letter string indicator the type of outcome measurement. 
#'    The following are in use: \code{hgt} (height), \code{wgt} (body weight), \code{bmi} (body
#'    mass index), \code{hdc} (head circumference), \code{wfh} (weight for height), 
#'    \code{hip} (hip circumference), \code{wst} (waist circumference), 
#'    \code{whr} (waist/hip ratio), \code{lgl} (leg length),
#'    \code{sit} (sitting height) and \code{shh} (sitting height/height ratio).}
#'    \item{\code{dist}:}{A string coding the type of distribution of the references.
#'    The following are in use: \code{NO} (normal distribution), \code{LMS} (LMS model),
#'    \code{PCT} (percentiles, non-parametric), \code{MEA} (mean only), 
#'    \code{MP} (mean-percentiles, non-parametric), \code{BCCG} (Box-Cox 
#'    Cole-Green distribution), \code{BCPE} (Box-Cox Power Exponential) and
#'    \code{BCT} (Box-Cox T-distribution).}
#'    \item{\code{mnx}:}{The minimum value of the \code{x} in the references. This is 
#'    calculated by \code{import.references()}.}
#'    \item{\code{mxx}:}{The maximum value of the \code{x} in the references. This is 
#'    calculated by \code{import.references()}.}
#'    \item{\code{source}:}{A character variable containing an abbreviation of the literature
#'    reference from which the values were taken.}
#'    \item{\code{public}:}{A character variable containing the literature
#'    reference from which the values were taken.}
#'    \item{\code{remark}:}{A free text field with remarks.}
#'    \item{\code{date}:}{An object of class \code{POSIXct} with the date at which the 
#'    reference values were created. This is automatically added when a new object of 
#'    class \code{reference} is created.}
#'}
#'    
#'@name referenceInfo-class
#'@rdname referenceInfo-class
#'@aliases referenceInfo-class
#'@author Stef van Buuren 2013
#'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}}, 
#'\code{\link[=referenceTable-class]{referenceTable}}
#'@keywords classes
#'@export
setClass("referenceInfo", 
         representation(
             n         = "numeric",
             i         = "numeric",
             j         = "numeric",
             code      = "character",
             country   = "character",
             year      = "numeric",
             sex       = "character",
             sub       = "character",
             yname     = "character",
             dist      = "character",
             mnx       = "numeric",
             mxx       = "numeric",
             source    = "character",
             public    = "character",
             remark    = "character",
             date      = "POSIXct"), 
         prototype = list(
             date      = Sys.time()
         )
)


#'ReferenceTable class
#'
#'The \code{referenceTable} object contains the reference values and the distribution type. 
#'The \code{referenceTable} object is generated by bla bla.  
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{table}:}{A \code{list} of objects, typically sublist with elements 
#'    named \code{x}, \code{mean}, \code{sd}, \code{L}, \code{M}, \code{S}, and so on.}
#'    \item{\code{dist}:}{A string identifying the name of the distribution of the values, 
#'    usually \code{"NO"}, \code{"LMS"}, \code{"PCT"}, and so on.}
#'    }
#' 
#'@name referenceTable-class
#'@rdname referenceTable-class
#'@aliases referenceTable-class
#'@author Stef van Buuren 2013
#'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}}, 
#'\code{\link[=referenceInfo-class]{referenceInfo}}
#'@keywords classes
#'@export
setClass("referenceTable", 
         representation(
             table = "list",
             dist = "character"
         )
)


#'Reference class
#'
#'The \code{reference} object contains the reference values and descriptive information 
#'about the values. The \code{reference} object is
#'generated by the \code{import.references()}, \code{find.reference()} 
#'and \code{as.reference()} functions. 
#'
#'@section Slots: 
#'  \describe{
#'    \item{\code{table}:}{An object of class \code{referenceTable}}
#'    \item{\code{info}:}{An object of class \code{referenceInfo}}
#'    }
#' 
#'@name reference-class
#'@rdname reference-class
#'@aliases reference-class
#'@author Stef van Buuren 2013
#'@seealso \code{\link{import.references}}, \code{\link[=referenceTable-class]{referenceTable}}, 
#'\code{\link[=referenceInfo-class]{referenceInfo}}
#'@keywords classes
#'@export
setClass("reference",
         representation(
             table = "referenceTable", 
             info  = "referenceInfo"
         ))


#'ReferenceList class
#'
#'A collection of references can be stored as an object of the class \code{referenceList}.
#'The \code{referenceList} object can be generated by 
#'by the \code{import.references()}, \code{find.reference()} 
#'and \code{as.reference()} functions. 
#'
#'@name referenceList-class
#'@rdname referenceList-class
#'@section Slots: 
#'  \describe{
#'    \item{\code{.Data}:}{A list}
#'    \item{\code{names}:}{A vector of names used to label references}
#'    \item{\code{index}:}{A \code{data.frame} with administrative information.}
#'    }
#'@aliases referenceList-class
#'@author Stef van Buuren 2013
#'@seealso \code{\link{import.references}}, \code{\link[=reference-class]{reference}}
#'@keywords classes
#'@export
setClass("referenceList",
         contains = "list",
         representation(
             names  = "character", 
             index  = "data.frame"
         ))


#' Is this object of class SRF (simple reference format)
#' 
#' @aliases is.SRF
#' @param x An object
#' @return A logical
#' @export
is.SRF <- function(x)
{
    inherits(x,"SRF")
}

#' Is this object of class SRFlist (simple reference format list)
#' 
#' @aliases is.SRFlist
#' @param x An object
#' @return A logical
#' @export
is.SRFlist <- function(x)
{
    inherits(x,"SRFlist")
}

#' Is this object of class reference
#' 
#' @aliases is.reference
#' @param x An object
#' @return A logical
#' @export
is.reference <- function(x)
{
    inherits(x,"reference")
}

#' Is this object of class referenceList
#' 
#' @aliases is.referenceList
#' @param x An object
#' @return A logical
#' @export
is.referenceList <- function(x)
{
    inherits(x,"referenceList")
}


#' Is this object of class referenceTable
#' 
#' @aliases is.referenceTable
#' @param x An object
#' @return A logical
#' @export
is.referenceTable <- function(x)
{
    inherits(x,"referenceTable")
}

#' Is this object of class referenceInfo
#' 
#' @aliases is.referenceInfo
#' @param x An object
#' @return A logical
#' @export
is.referenceInfo <- function(x)
{
    inherits(x,"referenceInfo")
}
