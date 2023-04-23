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


#' Translate 3-digit ISCO08 to ESEC
#'
#' This function translates a vector of 3-digit ISCO08 codes to ESEC codes using the
#' translation table stored in the `all_schemas$isco08_to_esec` data frame.
#'
#' The ESEC translation is from ISCO08 to ESEC. This translation is borrowed from the `iscogen` Stata package. For more info, search for 'ISCO-08 -> ESEC' in the documentation of the `iscogen` package.
#'
#'
#' Contrary to ISCO88-ESEC, ISCO08 does not have a simplified method and the translation is done from ISCO08 and not ISCO08COM.
#'
#' @param x A character vector of 3-digit ISCO08 codes. Even though these should be 3-digit, instead of 130, the code should be 1300, which is the 3-digit version of ISCO.
#'
#' @param is_supervisor A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).
#'
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#'
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#'
#' @param label A logical value indicating whether to return the labels of the translated ESEC codes (default is \code{FALSE}).
#'
#' @return A character vector of ESEC codes.
#'
#' @examples
#' library(dplyr)
#'
#' # convert to three digits
#' ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)
#'
#' # Using the full method
#' ess %>%
#'   transmute(
#'     esec_label = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     ),
#'     esec = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     )
#'   )
#'
#' @export
isco08_to_esec <- function(x,
                           is_supervisor,
                           self_employed,
                           n_employees,
                           label = FALSE) {
  # TODO: this function should fail if `x` is not 3 digits (1310 instead of 131)
  col_position <- dplyr::case_when(
    # Is it an employee?
    self_employed == 0 & is_supervisor == 0 ~ 2,
    # Is it a supervisor of other people?
    self_employed == 0 & is_supervisor == 1 ~ 3,
    self_employed == 1 & n_employees == 0 ~ 4,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 5,
    self_employed == 1 & n_employees >= 10 ~ 6
  )

  res <- multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "ESEC",
    translate_df = all_schemas$isco08_to_esec,
    translate_label_df = all_labels$esec,
    label = label,
    digits = 4
  )

  res
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

#' Swap ISCO08 between 1, 2, 3 and 4 digit groups
#'
#' This function translates a vector of ISCO08 codes between different digits. For most surveys, this will be translating between the 1 digit occupations to more general groups, such as two digits, three digits and four digits.
#'
#' Note that to translate using `isco08_swap` you'll need to provide the `from` and `to` arguments. The first one specifies the current number of digits of the input variable. If your variable is 1 digit occupations, then `from` should be `1`. If you want to translate 1 digit occupations to three digits then the arguments should be `from = 1` and `to = 3`. See the argument description of `from` and `to` for all possible values. As well as examples on how this works
#'
#' @param x A character vector of ISCO08 codes.
#' @param from a numeric specifying the occupation digits of the input vector. Possible values are only 1, 2, 3 or 4.
#' @param to a numeric specifying the desired occupation digits. Possible values are only 1, 2, 3 or 4.
#'
#' @return A character vector of ISCO08 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(
#'   isco08_four_digits = isco08_swap(isco08, from = 4, to = 1),
#'   isco08_three_digits = isco08_swap(isco08, from = 4, to = 2),
#'   isco08_two_digits = isco08_swap(isco08, from = 4, to = 3),
#'   isco08_one_digits = isco08_swap(isco08, from = 4, to = 4)
#' )
#'
#' @export
isco08_swap <- function(x,
                        from,
                        to) {
  if (from < to) {
    stop("`from` should always be a bigger digit group than `to`.")
  }

  from <- as.character(from)
  to <- as.character(to)
  from <- match.arg(from, as.character(1:4))
  to <- match.arg(to, as.character(1:4))

  opts <- c("4" = "unit", "3" = "minor", "2" = "submajor", "1" = "major")

  from <- unname(opts[from])
  to <- unname(opts[to])

  if (from == to) {
    return(x)
  }

  common_translator(
    x,
    input_var = from,
    output_var = to,
    translate_df = all_schemas$isco08_hierarchy,
    translate_label_df = NULL,
    label = FALSE
  )
}
