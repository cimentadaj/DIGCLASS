#' Translate ISCO88 to ISCO68
#'
#' This function translates a vector of ISCO08 codes to ISCO88 codes using the
#' translation table stored in the `all_schemas$isco08_to_isco88` data frame.
#'
#' @param x A character vector of ISCO08 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO88 codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO88 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO88 = isco08_to_isco88(isco08, label = TRUE))
#' ess %>% mutate(ISCO88 = isco08_to_isco88(isco08, label = FALSE))
#'
#' @export
isco88_to_isco68 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO68",
    translate_df = all_schemas$isco88_to_isco68,
    translate_label_df = all_labels$isco68,
    label = label
  )
}


#' Translate ISCO88 to ISCO08
#'
#' This function translates a vector of ISCO88 codes to ISCO08 codes using the
#' translation table stored in the `all_schemas$isco88_to_isco08` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO08 codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO08 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO08 = isco88_to_isco08(isco88, label = TRUE))
#' ess %>% mutate(ISCO08 = isco88_to_isco08(isco88, label = FALSE))
#'
#' @export
isco88_to_isco08 <- function(x, label = FALSE) {

  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO08",
    translate_df = all_schemas$isco88_to_isco08,
    translate_label_df = all_labels$isco08,
    label = label
  )
}

#' Translate ISCO88 to ISCO88COM
#'
#' This function translates a vector of ISCO88 codes to ISCO88COM codes using the
#' translation table stored in the `all_schemas$isco88_to_isco88com` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO88COM codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO88COM codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO88COM = isco88_to_isco88com(isco88, label = TRUE))
#' ess %>% mutate(ISCO88COM = isco88_to_isco88com(isco88, label = FALSE))
#'
#' @export
isco88_to_isco88com <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO88COM",
    translate_df = all_schemas$isco88_to_isco88com,
    translate_label_df = all_labels$isco88com,
    label = label
  )
}

#' Translate ISCO88 to ISEI
#'
#' This function translates a vector of ISCO88 codes to ISEI codes using the
#' translation table stored in the `all_schemas$isco88_to_isei` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#'
#' @return A numeric vector of ISEI codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISEI = isco88_to_isei(isco88))
#'
#' @export
isco88_to_isei <- function(x) {

  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISEI",
    translate_df = all_schemas$isco88_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}


#' Translate ISCO88 to SIOPS
#'
#' This function translates a vector of ISCO88 codes to SIOPS codes using the
#' translation table stored in the `all_schemas$isco88_to_siops` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#'
#' @return A character vector of SIOPS codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(SIOPS = isco88_to_siops(isco88))
#'
#' @export
isco88_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "SIOPS",
    translate_df = all_schemas$isco88_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}

#' Translate ISCO88 to MPS
#'
#' This function translates a vector of ISCO88 codes to MPS codes using the
#' translation table stored in the `all_schemas$isco88_to_mps` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#'
#' @return A character vector of MPS codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(MPS = isco88_to_mps(isco88))
#'
#' @export
isco88_to_mps <- function(x) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "MPS88",
    translate_df = all_schemas$isco88_to_mps,
    translate_label_df = NULL,
    label = FALSE
  )
}

#' Translate ISCO88 to EGP
#'
#' This function translates a vector of ISCO88 codes to EGP codes using the
#' translation table stored in the `all_schemas$isco88_to_egp` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the
#' translated EGP codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(EGP = isco88_to_egp(isco88, self_employed, emplno, label = TRUE))
#' ess %>% mutate(EGP = isco88_to_egp(isco88, self_employed, emplno, label = FALSE))
#'
#' @export
isco88_to_egp <- function(x, self_employed, n_employees, label = FALSE) {

  col_position <- dplyr::case_when(
    self_employed == 0 & n_employees == 0 ~ 2,
    self_employed == 0 & dplyr::between(n_employees, 1, 9) ~ 3,
    self_employed == 0 & n_employees >= 10 ~ 4,
    self_employed == 1 & n_employees == 0 ~ 5,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 6,
    self_employed == 1 & n_employees >= 10 ~ 7,
  )

  multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "EGP",
    translate_df = all_schemas$isco88_to_egp,
    translate_label_df = all_labels$egp,
    label = label
  )
}


#' Translate ISCO88 to EGP11
#'
#' This function translates a vector of ISCO88 codes to EGP11 codes using the
#' translation table stored in the `all_schemas$isco88_to_egp11` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the
#' translated EGP11 codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(EGP = isco88_to_egp11(isco88, self_employed, emplno, label = TRUE))
#' ess %>% mutate(EGP = isco88_to_egp11(isco88, self_employed, emplno, label = FALSE))
#'
#' @export
isco88_to_egp11 <- function(x, self_employed, n_employees, label = FALSE) {

  col_position <- dplyr::case_when(
    self_employed == 0 & n_employees == 0 ~ 2,
    self_employed == 0 & n_employees == 1 ~ 3,
    self_employed == 0 & dplyr::between(n_employees, 2, 9) ~ 4,
    self_employed == 0 & n_employees >= 10 ~ 5,
    self_employed == 1 & n_employees == 0 ~ 6,
    self_employed == 1 & n_employees == 1 ~ 7,
    self_employed == 1 & dplyr::between(n_employees, 2, 9) ~ 8,
    self_employed == 1 & n_employees >= 10 ~ 9,
  )

  multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "EGP",
    translate_df = all_schemas$isco88_to_egp11,
    translate_label_df = all_labels$egp11,
    label = label
  )
}

#' Translate ISCO88 to OESCH
#'
#' This function translates a vector of ISCO88 codes to OESCH codes using the
#' translation table stored in the `all_schemas$isco88_to_oesch` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the
#' translated OESCH codes (default is \code{FALSE}).
#'
#' @return A character vector of OESCH codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(EGP = isco88_to_oesch(isco88, self_employed, emplno, label = TRUE))
#' ess %>% mutate(EGP = isco88_to_oesch(isco88, self_employed, emplno, label = FALSE))
#'
#' @export
isco88_to_oesch <- function(x, self_employed, n_employees, label = FALSE) {

  col_position <- dplyr::case_when(
    self_employed == 0 ~ 2,
    self_employed == 1 & n_employees == 0 ~ 3,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 4,
    self_employed == 1 & n_employees >= 10 ~ 5,
  )

  multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "OESCH",
    translate_df = all_schemas$isco88_to_oesch,
    translate_label_df = all_labels$oesch,
    label = label
  )
}
