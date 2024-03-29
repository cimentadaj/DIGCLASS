% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isco08.R
\name{isco08_two_to_esec}
\alias{isco08_two_to_esec}
\title{Translate 2-digit ISCO08 to ESEC}
\usage{
isco08_two_to_esec(
  x,
  is_supervisor,
  self_employed,
  n_employees,
  label = FALSE,
  to_factor = FALSE
)
}
\arguments{
\item{x}{A character vector of 2-digit ISCO codes. This should be the 4-digit equivalent so instead of 13, the code should be 1300, which is the 4-digit version of of the 2-digit ISCO.}

\item{is_supervisor}{A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).}

\item{self_employed}{A numeric vector indicating whether each individual is self-employed (1) or an employee (0).}

\item{n_employees}{A numeric vector indicating the number of employees under each respondent.}

\item{label}{A logical value indicating whether to return the labels of the translated ESEC codes (default is \code{FALSE}).}

\item{to_factor}{A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the labels for ESEC found in \code{all_labels} (default is \code{FALSE}).}
}
\description{
This function translates a vector of 2-digit ISCO08 codes to ESEC codes using the translation table stored in \code{all_schema$isco08_two_to_esec}.
}
\details{
This function will accept 2 digit codes as 4 digits. This means that if the 2-digit code is 13 then it should be 1300. All codes should be 4 digits, even though the code is represented as 2-digits (1300, 1400, etc..)

This is exactly the same as \code{DIGCLASS::isco08_to_esec} but for two digit ISCO.

For more information on this class schema, please check the references below:
\itemize{
\item Resource website of the European Socio-economic Classification (ESeC): \url{https://www.iser.essex.ac.uk/archives/esec}
\item Derivation material: \url{https://www.iser.essex.ac.uk/archives/esec/user-guide/derivation-material}
\item Rose, D. and Harrison, E. (2007) ‘The European Socio-economic Classification: A New Social Class Schema for European Research’, European Societies, 9, 3: 459-490. \url{https://doi.org/10.1080/14616690701336518}
\item Rose D, Harrison E (2010) Social Class in Europe. An Introduction to the European Socio-economic Classification. London: Routledge.
\item Wirth, H. (2023). EU-SILC Tools: European Socioeconomic Classification - ESeC88 and ESeC08. (GESIS Papers, 2023/01). Köln: GESIS - Leibniz-Institut für Sozialwissenschaften. \url{https://doi.org/10.21241/ssoar.83962}
}
}
\examples{
library(dplyr)

# convert to two digits
ess$isco08_two <- isco08_swap(ess$isco08, from = 4, to = 2)

ess \%>\%
  transmute(
    isco08_two,
    esec = isco08_two_to_esec(
      isco08_two,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    ),
    esec_label = isco08_two_to_esec(
      isco08_two,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    )
  )

}
