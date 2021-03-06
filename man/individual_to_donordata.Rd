% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/individual_to_donordata.R
\name{individual_to_donordata}
\alias{individual_to_donordata}
\title{Convert object of class 'individual' to donordata}
\usage{
individual_to_donordata(x, element = NULL)
}
\arguments{
\item{x}{An object of class \code{individual}.}

\item{element}{Requested list elements. Can be \code{"child"},
\code{"time"}. The default \code{NULL} returns a list of both
elements.}
}
\value{
If \code{elements == "list"}, the function returns
a list of \code{"child"} and \code{"time"}. The function
returns a \code{"tibble"} for the choices \code{"child"} and
\code{"time"}.
}
\description{
This function returns the slots \code{"id"}, \code{"child"} and
\code{"time"}, either separate or as a \code{list} from
the \code{individual} object. The code
assumes that slots \code{"id"}, \code{"child"} and \code{"time"}
are up-to-data and consistent with the other slots.
}
\examples{
library(donorloader)
smocc <- load_data(dnr = "smocc")
smocc_bs <- load_data(dnr = "smocc_bs")
p <- donordata_to_individual(dnr = "smocc", id = 10001)
p
q <- individual_to_donordata(p)
q
}
\seealso{
\code{\link{xyz-class}}, \code{\link{bse-class}}
}
\author{
Stef van Buuren 2019
}
