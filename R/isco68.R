#' Translate ISCO68 to ISCO88
#'
#' This function translates a vector of ISCO68 codes to ISCO88 codes using the
#' translation table stored in the `all_schemas$isco68_to_isco88` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO88 codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO88 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO88 = isco68_to_isco88(isco68, label = TRUE))
#' ess %>% mutate(ISCO88 = isco68_to_isco88(isco68, label = FALSE))
#'
#' @export
isco68_to_isco88 <- function(x, label = FALSE) {

  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO88",
    translate_df = all_schemas$isco68_to_isco88,
    translate_label_df = all_labels$isco88,
    label = label
  )
}

#' Translate ISCO68 to ISCO08
#'
#' This function translates a vector of ISCO68 codes to ISCO08 codes using the
#' translation table stored in the `all_schemas$isco68_to_isco08` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO08 codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO08 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO08 = isco68_to_isco08(isco68, label = TRUE))
#' ess %>% mutate(ISCO08 = isco68_to_isco08(isco68, label = FALSE))
#'
#' @export
isco68_to_isco08 <- function(x, label = FALSE) {

  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO08",
    translate_df = all_schemas$isco68_to_isco08,
    translate_label_df = all_labels$isco08,
    label = label
  )
}

#' Translate ISCO68 to ISEI
#'
#' This function translates a vector of ISCO68 codes to ISEI codes using the
#' translation table stored in the `all_schemas$isco68_to_isei` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#'
#' @return A character vector of ISEI codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISEI = isco68_to_isei(isco68))
#'
#' @export
isco68_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISEI",
    translate_df = all_schemas$isco68_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}

#' Translate ISCO68 to SIOPS
#'
#' This function translates a vector of ISCO68 codes to SIOPS codes using the
#' translation table stored in the `all_schemas$isco68_to_siops` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#'
#' @return A character vector of SIOPS codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(SIOPS = isco68_to_siops(isco68))
#'
#' @export
isco68_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "SIOPS",
    translate_df = all_schemas$isco68_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}

#' Translate ISCO68 to EGP
#'
#' This function translates a vector of ISCO68 codes to EGP codes using the
#' translation table stored in the `all_schemas$isco68_to_egp` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the translated EGP codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(EGP = isco68_to_egp(isco68, self_employed, emplno, label = TRUE))
#' ess %>% mutate(EGP = isco68_to_egp(isco68, self_employed, emplno, label = FALSE))
#'
#' @export
isco68_to_egp <- function(x, self_employed, n_employees, label = FALSE) {

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
    translate_df = all_schemas$isco68_to_egp,
    translate_label_df = all_labels$egp,
    label = label
  )
}


#' Translate ISCO68 to EGP11
#'
#' This function translates a vector of ISCO68 codes to EGP11 codes using the
#' translation table stored in the `all_schemas$isco68_to_egp11` data frame.
#'
#' @param x A character vector of ISCO68 codes.
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the translated EGP11 codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP11 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(EGP = isco68_to_egp11(isco68, self_employed, emplno, label = TRUE))
#' ess %>% mutate(EGP = isco68_to_egp11(isco68, self_employed, emplno, label = FALSE))
#'
#' @export
isco68_to_egp11 <- function(x, self_employed, n_employees, label = FALSE) {

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
    translate_df = all_schemas$isco68_to_egp11,
    translate_label_df = all_labels$egp11,
    label = label
  )
}
