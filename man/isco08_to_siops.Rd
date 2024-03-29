% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isco08.R, R/isco88.R, R/isco68.R
\name{isco08_to_siops}
\alias{isco08_to_siops}
\alias{isco88_to_siops}
\alias{isco68_to_siops}
\title{Translate 4-digit ISCO08/ISCO88/ISCO68 to SIOPS}
\usage{
isco08_to_siops(x, to_factor = FALSE)

isco88_to_siops(x, to_factor = FALSE)

isco68_to_siops(x, to_factor = FALSE)
}
\arguments{
\item{x}{A character vector of 4-digit ISCO codes.}

\item{to_factor}{A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the sorted codes of SIOPS in \code{all_schemas$isco08_to_siops}, \code{all_schemas$isco88_to_siops} and \code{all_schemas$isco68_to_siops}.}
}
\value{
A character vector of SIOPS codes.
}
\description{
This function translates a vector of 4-digit ISCO08/ISCO88/ISCO68 codes to SIOPS codes using the translation tables stored in \code{all_schema$isco08_to_siops / all_schema$isco88_to_siops / all_schema$isco68_to_siops}.
}
\details{
This translation was taken from the \code{iscogen} Stata package. For more details, check out the package documentation and search for \code{ISCO08/ISCO88/ISCO88 -> SIOPS}.

Since \code{SIOPS} doesn't have any labels, the \code{labels} is not availabe in this function.

For more information on this class schema, please check the references below:
\itemize{
\item Ganzeboom, H.B.G., P.M. De Graaf, D.J. Treiman (1992) A Standard International Socio-Economic Index of Occupational Status. Social Science Research 21: 1-56.
\item Ganzeboom, H.B.G., D.J. Treiman (1996) Internationally Comparable Measures of Occupational Status for the 1988 International Standard Classification of Occupations. Social Science Research 25: 201-239.
}
}
\examples{
library(dplyr)

ess \%>\%
  transmute(
    isco08,
    isco88,
    isco68,
    siops_08 = isco08_to_siops(isco08),
    siops_88 = isco88_to_siops(isco88),
    siops_68 = isco68_to_siops(isco68)
  )

}
