#' Translate ISCO08 to ISCO88
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
isco08_to_isco88 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "ISCO88",
    translate_df = all_schemas$isco08_to_isco88,
    translate_label_df = all_labels$isco88,
    label = label
  )
}


#' Translate ISCO08 to ISEI
#'
#' This function translates a vector of ISCO08 codes to ISEI codes using the
#' translation table stored in the `all_schemas$isco08_to_isei` data frame.
#'
#' @param x A character vector of ISCO08 codes.
#'
#' @return A numeric vector of ISEI codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISEI = isco08_to_isei(isco08))
#'
#' @export
isco08_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "ISEI-08",
    translate_df = all_schemas$isco08_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}

#' Translate ISCO08 to SIOPS
#'
#' This function translates a vector of ISCO08 codes to SIOPS codes using the
#' translation table stored in the `all_schemas$isco08_to_siops` data frame.
#'
#' @param x A character vector of ISCO08 codes.
#'
#' @return A character vector of SIOPS codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(SIOPS = isco08_to_siops(isco08))
#'
#' @export
isco08_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "SIOPS-08",
    translate_df = all_schemas$isco08_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}


#' Translate ISCO08 to OESCH

#'
#' This function translates a vector of ISCO08 codes to OESCH codes using the
#' translation table stored in the `all_schemas$isco08_to_oesch` data frame,
#' with the column position determined by the values of \code{self_employed}
#' and \code{n_employees}.
#'
#' @param x A character vector of ISCO08 codes.
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
#' ess %>% mutate(OESCH = isco08_to_oesch(isco08, self_employed, emplno, label = TRUE))
#'
#' @export
isco08_to_oesch <- function(x, self_employed, n_employees, label = FALSE) {

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
    translate_df = all_schemas$isco08_to_oesch,
    translate_label_df = all_labels$oesch,
    label = label
  )
}
