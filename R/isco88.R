#' Translate ISCO88 to ISCO68
#'
#' This function translates a vector of ISCO88 codes to ISCO68 codes using the
#' translation table stored in the `all_schemas$isco88_to_isco68` data frame.
#'
#' @param x A character vector of ISCO88 codes.
#' @param label A logical value indicating whether to return the labels of the
#' translated ISCO88 codes (default is \code{FALSE}).
#'
#' @return A character vector of ISCO88 codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% mutate(ISCO88 = isco88_to_isco68(isco88, label = TRUE))
#' ess %>% mutate(ISCO88 = isco88_to_isco68(isco88, label = FALSE))
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

#' @rdname isco08_to_isei
#' @order 2
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


#' @rdname isco08_to_siops
#' @order 2
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


#' Translate ISCO88 to EGP-MP
#'
#' This function translates a vector of ISCO88 codes to EGP-MP codes.
#'
#' @details
#' EGP-MP is a class schema similar to EGP but reassigns managers and professionals (ISCO88 codes 1 and 2) to have both high/low managers and profesionals. Note that since this translation first converts ISCO88 to EGP and then applies the following logic to build EGP-MP:
#'
#' * All occupations with EGP digit 1 and ISCO 1-digit 0 or 1 or has subordinates, **is a high manager**
#' * All occupations with EGP digit 1 and is self-employed with more than 1 employee, **is a high manager**
#' * All occupations with EGP digit 1 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **high professional**
#'
#' * All occupations with EGP digit 2 and ISCO 1-digit 0 or 1 or has subordinates, is a **lower manager**
#' * All occupations with EGP digit 2 and is self-employed with more than 1 employee, is a **lower manager**
#' * All occupations with EGP digit 2 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **lower professional**
#'
#' All other EGP codes remain the same.
#'
#' This translation was created from the Stata do file shared by Oscar Smallenbroek called "ESEC-MP.do". For more info, please contact the author.
#'
#' @param x A character vector of ISCO88 codes.
#' @param is_supervisor A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the
#' translated EGP-MP codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP-MP codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco88,
#'     egp_mp = isco88_to_egp_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     egp_mp_label = isco88_to_egp_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
##'
#' @export
isco88_to_egp_mp <- function(x,
                             is_supervisor,
                             self_employed,
                             n_employees,
                             label = FALSE) {
  egp <- isco88_to_egp(
    x,
    self_employed,
    n_employees,
    label = FALSE
  )

  egp_mp <- managers_professionals_helper(
    x,
    egp,
    is_supervisor,
    self_employed,
    n_employees,
    label = label
  )

  egp_mp
}


#' Translate ISCO88 to EGP11-MP
#'
#' This function translates a vector of ISCO88 codes to EGP11-MP codes.
#'
#' @details
#' EGP11-MP is a class schema similar to EGP11 but reassigns managers and professionals (ISCO88 codes 1 and 2) to have both high/low managers and profesionals. Note that since this translation first converts ISCO88 to EGP11 and then applies the following logic to build EGP11-MP:
#'
#' * All occupations with EGP11 digit 1 and ISCO 1-digit 0 or 1 or has subordinates, **is a high manager**
#' * All occupations with EGP11 digit 1 and is self-employed with more than 1 employee, **is a high manager**
#' * All occupations with EGP11 digit 1 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **high professional**
#'
#' * All occupations with EGP11 digit 2 and ISCO 1-digit 0 or 1 or has subordinates, is a **lower manager**
#' * All occupations with EGP11 digit 2 and is self-employed with more than 1 employee, is a **lower manager**
#' * All occupations with EGP11 digit 2 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **lower professional**
#'
#' All other EGP11 codes remain the same.
#'
#' This translation was created from the Stata do file shared by Oscar Smallenbroek called "ESEC-MP.do". For more info, please contact the author.
#'
#' @param x A character vector of ISCO88 codes.
#' @param is_supervisor A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the
#' translated EGP11-MP codes (default is \code{FALSE}).
#'
#' @return A character vector of EGP11-MP codes.
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco88,
#'     egp_mp = isco88_to_egp11_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     egp_mp_label = isco88_to_egp11_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
#'
#' @export
isco88_to_egp11_mp <- function(x,
                               is_supervisor,
                               self_employed,
                               n_employees,
                               label = FALSE) {
  egp <- isco88_to_egp11(
    x,
    self_employed,
    n_employees,
    label = FALSE
  )

  egp_mp <- managers_professionals_helper(
    x,
    egp,
    is_supervisor,
    self_employed,
    n_employees,
    label = label
  )

  egp_mp
}


#'@rdname isco08_to_esec
#'@order 2
#' @export
isco88com_to_esec <- function(x,
                              is_supervisor,
                              self_employed,
                              n_employees,
                              full_method = TRUE,
                              label = FALSE) {
  # TODO: this function should fail if `x` is not 3 digits (1310 instead of 131)

  if (full_method) {
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
      translate_df = all_schemas$isco88com_to_esec_three,
      translate_label_df = all_labels$esec,
      label = label,
      digits = 4
    )
  } else {
    res <- common_translator(
      x,
      input_var = "ISCO88(3-digit)",
      output_var = "ESEC",
      translate_df = all_schemas$isco88com_to_esec_three,
      translate_label_df = all_labels$esec,
      label = label
    )
  }

  res
}


#' @rdname isco08_to_esec_mp
#' @order 2
#' @export
isco88com_to_esec_mp <- function(x,
                                 is_supervisor,
                                 self_employed,
                                 n_employees,
                                 full_method = TRUE,
                                 label = FALSE) {
  esec <- isco88com_to_esec(
    x,
    is_supervisor,
    self_employed,
    n_employees,
    full_method = full_method,
    label = FALSE
  )

  esec_mp <- managers_professionals_helper(
    x,
    esec,
    is_supervisor,
    self_employed,
    n_employees,
    label = label
  )

  esec_mp
}


#' @rdname isco08_to_msec
#' @order 2
#' @export
isco88com_to_msec <- function(x,
                              is_supervisor,
                              self_employed,
                              n_employees,
                              label = FALSE) {
  # TODO: this function should fail if `x` is not 3 digits (1310 instead of 131)
  col_position <- dplyr::case_when(
    self_employed == 1 & n_employees >= 10 ~ 2,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 3,
    self_employed == 1 & n_employees == 0 ~ 4,
    self_employed == 0 & is_supervisor == 1 ~ 5,
    self_employed == 0 & is_supervisor == 0 ~ 6,
  )

  res <- multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "MSEC",
    translate_df = all_schemas$isco88com_to_msec,
    translate_label_df = all_labels$msec,
    label = label,
    digits = 4
  )

  res
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


#' @rdname isco08_swap
#' @order 2
#' @export
isco88_swap <- function(x,
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
    translate_df = all_schemas$isco88_hierarchy,
    translate_label_df = NULL,
    label = FALSE
  )
}
