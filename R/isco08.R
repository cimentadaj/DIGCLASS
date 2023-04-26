#' `r rg_template_title("ISCO08", "ISCO88")`
#'
#' `r rg_template_intro("ISCO08", "ISCO88", "isco08_to_isco88")`
#'
#' @details`r rg_template_details_iscogen("ISCO08", "ISCO88")`
#'
#' This translation uses first mapping in case of duplicates (repeated mappings); this is consistent with the source because in SPSS later mappings are ignored. The source of the translation is `isco0888.sps` from \url{http://www.harryganzeboom.nl/isco08/}.
#'
#' @param x `r rg_template_arg_x("ISCO08")`
#' @param label `r rg_template_arg_label("ISCO88")`
#'
#' @return `r rg_template_return("ISCO88")`
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% transmute(
#'   isco08,
#'   isco88 = isco08_to_isco88(isco08, label = FALSE),
#'   isco88_label = isco08_to_isco88(isco08, label = TRUE)
#' )
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


#' `r rg_template_title("ISCO08", "ISEI")`
#'
#' `r rg_template_intro("ISCO08", "ISEI", "isco08_to_isei")`
#'
#' @details`r rg_template_details_iscogen("ISCO08", "ISEI")`
#'
#' Since `ISEI` doesn't have any labels, the `labels` is not availabe in this function. The source of the translation is `isco08_with_isei.pdf` from \url{http://www.harryganzeboom.nl/isco08/}.
#'
#' @param x `r rg_template_arg_x("ISCO08")`
#'
#' @return `r rg_template_return("ISEI")`
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% transmute(isco08, isei = isco08_to_isei(isco08))
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


#' `r rg_template_title("ISCO08", "SIOPS")`
#'
#' `r rg_template_intro("ISCO08", "SIOPS", "isco08_to_siops")`
#'
#' @details`r rg_template_details_iscogen("ISCO08", "SIOPS")`
#'
#' Since `SIOPS` doesn't have any labels, the `labels` is not availabe in this function. The source of the translation is `isqotrei08.sps` from \url{http://www.harryganzeboom.nl/isco08/}.
#'
#' @param x `r rg_template_arg_x("ISCO08")`
#'
#' @return `r rg_template_return("SIOPS")`
#'
#' @examples
#' library(dplyr)
#'
#' ess %>% transmute(isco08, SIOPS = isco08_to_siops(isco08))
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

#' Translate 3-digit ISCO08 to MSEC
#'
#' This function translates a vector of 3-digit ISCO08 codes to MSEC codes using the
#' translation table stored in the `all_schemas$isco08_to_msec` data frame.
#'
#' This translation was created from the document "Allocation rules of ISCO-08 and ISCO-88 (COM) 3-digit codes to ESEG-Revised" from Oscar Smallenbroek, Florian Hertel and Carlo Barone. For more info, please contact the authors. Although originally called ESEG-Revised, the class schema has been formally called MSEC.
#'
#' This function will accept 3-digit codes as 4 digits. This means that if the 3-digit code is 131 then it should be 1310. All codes should be 4 digits, even though the code is represented as 3 digits (1310, 1230, etc..)
#'
#' @param x A character vector of 3-digit ISCO08 codes. Even though these should be 3-digit, instead of 130, the code should be 1300, which is the 3-digit version of ISCO.
#' @param is_supervisor A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).
#' @param self_employed A numeric vector indicating whether each individual is self-employed (1) or not (0).
#' @param n_employees A numeric vector indicating the number of employees for each individual.
#' @param label A logical value indicating whether to return the labels of the translated MSEC codes (default is \code{FALSE}).
#'
#' @return A character vector of MSEC codes.
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
#'     msec_label = isco08_to_msec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     ),
#'     msec = isco08_to_msec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     )
#'   )
#'
#' @export
isco08_to_msec <- function(x,
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
    translate_df = all_schemas$isco08_to_msec,
    translate_label_df = all_labels$msec,
    label = label,
    digits = 4
  )

  res
}

#' `r rg_template_title("ISCO08", "microclass")`
#'
#' `r rg_template_intro("ISCO08", "microclass", "isco08_to_microclass")`
#'
#' @details This translation was created from the Excel file shared by Oscar Smallenbroek called "isco08 to micro with numeric labels.xlsx". For more info, please contact the author.
#'
#' @param x `r rg_template_arg_x("ISCO08")`
#' @param label `r rg_template_arg_label("microclass")`
#'
#' @return `r rg_template_return("microclass")`
#'
#' @examples
#' library(dplyr)
#'
#' # Using the full method
#' ess %>% transmute(
#'   isco08,
#'   microclasses = isco08_to_microclass(isco08),
#'   microclasses_label = isco08_to_microclass(isco08, label = TRUE)
#' )
#'
#' @export
isco08_to_microclass <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "microclass",
    translate_df = all_schemas$isco08_to_microclass,
    translate_label_df = all_labels$microclass,
    label = label
  )
}


#' `r rg_template_title("ISCO08", "ESEC", digit = 3)`
#'
#' `r rg_template_intro("ISCO08", "ESEC", "isco08_to_esec", digit = 3)`
#'
#' @details `r rg_template_details_iscogen("ISCO08", "ESEC")` `r rg_template_digits_warning(digit = 3)`
#'
#' Contrary to ISCO88COM-ESEC, ISCO08 does not have a simplified method and the translation is done from ISCO08 and not ISCO08COM.
#'
#' @param x `r rg_template_arg_x_digit("ISCO08", digit = 3)`
#' @param is_supervisor `r rg_template_arg_supervisor()`
#' @param self_employed `r rg_template_arg_selfemployed()`
#' @param n_employees `r rg_template_arg_nemployees()`
#' @param label `r rg_template_arg_label("ESEC")`
#'
#' @return `r rg_template_return("ESEC")`
#'
#' @examples
#' library(dplyr)
#'
#' # convert to three digits
#' ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)
#'
#' ess %>%
#'   transmute(
#'     isco08_three,
#'     esec = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     esec_label = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
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
    translate_df = all_schemas$isco08_to_esec_three,
    translate_label_df = all_labels$esec,
    label = label,
    digits = 4
  )

  res
}


#' `r rg_template_title("ISCO08", "ESEC-MP", digit = 3)`
#'
#' `r rg_template_intro("ISCO08", "ESEC-MP", "isco08_to_esec", digit = 3)` After translating to ESEC, this function reassigns managers and professionals (ISCO08 codes 1 and 2) to have both high/low managers and profesionals
#'
#' @details
#'
#' # TODO: After this is corrected in the code, correct it here and in all docs.
#' The `MP` in `ESEC-MP`stands for Managers and Professionals. The logic used to build this is like this:
#'
#' * All occupations with ESEC digit 1 and ISCO 1-digit 0 or 1 or has subordinates, **is a high manager**
#' * All occupations with ESEC digit 1 and is self-employed with more than 1 employee, **is a high manager**
#' * All occupations with ESEC digit 1 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **high professional**
#'
#' * All occupations with ESEC digit 2 and ISCO 1-digit 0 or 1 or has subordinates, is a **lower manager**
#' * All occupations with ESEC digit 2 and is self-employed with more than 1 employee, is a **lower manager**
#' * All occupations with ESEC digit 2 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **lower professional**
#'
#' This translation was created from the Stata do file shared by Oscar Smallenbroek called "ESEC-MP.do". For more info, please contact the author.
#'
#' `r rg_template_digits_warning(digit = 3)`
#'
#' Contrary to ISCO88COM-ESEC, ISCO08 does not have a simplified method and the translation is done from ISCO08 and not ISCO08COM.
#'
#' @param x `r rg_template_arg_x_digit("ISCO08", digit = 3)`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("ESEC-MP")`
#'
#' @return `r rg_template_return("ESEC-MP")`
#'
#' @examples
#' library(dplyr)
#'
#' # convert to three digits
#' ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)
#'
#' ess %>%
#'   transmute(
#'     isco08_three,
#'     esec = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     esec_label = isco08_to_esec(
#'       isco08_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
#' @export
isco08_to_esec_mp <- function(x,
                              is_supervisor,
                              self_employed,
                              n_employees,
                              label = FALSE) {
  esec <- isco08_to_esec(x, is_supervisor, self_employed, n_employees, label = FALSE)

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

#' `r rg_template_title("ISCO08", "ESEC", digit = 2)`
#'
#' `r rg_template_intro("ISCO08", "ESEC", "isco08_two_to_esec", digit = 2)`
#'
#' `r rg_template_digits_warning(digit = 2)`
#'
#' @param x `r rg_template_arg_x_digit("ISCO08", digit = 2)`
#'
#' @inheritParams isco08_to_esec
#'
#' @examples
#' library(dplyr)
#'
#' # convert to two digits
#' ess$isco08_two <- isco08_swap(ess$isco08, from = 4, to = 2)
#'
#' ess %>%
#'   transmute(
#'     isco08_two,
#'     esec = isco08_two_to_esec(
#'       isco08_two,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     esec_label = isco08_two_to_esec(
#'       isco08_two,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
#'
#' @export
isco08_two_to_esec <- function(x,
                               is_supervisor,
                               self_employed,
                               n_employees,
                               label = FALSE) {
  # TODO: this function should fail if `x` is not 2 digits (1300 instead of 13)
  col_position <- dplyr::case_when(
    self_employed == 1 & n_employees >= 10 ~ 1,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 2,
    self_employed == 1 & n_employees == 0 ~ 3,
    self_employed == 0 & is_supervisor == 1 ~ 4,
    self_employed == 0 & is_supervisor == 0 ~ 5
  )

  res <- multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "ESEC",
    translate_df = all_schemas$isco08_to_esec_two,
    translate_label_df = all_labels$esec,
    label = label,
    digits = 4
  )

  res
}



#' `r rg_template_title("ISCO08", "OESCH")`
#'
#' `r rg_template_intro("ISCO08", "OESCH", "isco08_to_oesch")`
#'
#' @param x `r rg_template_arg_x("ISCO08")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("OESCH")`
#' @return `r rg_template_return("OESCH")`
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco08,
#'     oesch = isco08_to_oesch(isco08, self_employed, emplno, label = FALSE),
#'     oesch_label = isco08_to_oesch(isco08, self_employed, emplno, label = TRUE)
#' )
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

#' Swap ISCO08/ISCO88/ISCO88 between 1, 2, 3 and 4 digit groups
#'
#' This function translates a vector of ISCO08/ISCO88/ISCO88 codes between different digits. For most surveys, this will be translating between the 4 digit occupations to more general groups, such as three , two and one digit groups.
#'
#' @details Note that to translate using `isco*_swap` you'll need to provide the `from` and `to` arguments. The first one specifies the current number of digits of the input variable. If your variable is 4-digit occupations, then `from` should be `4`. If you want to translate 4-digit occupations to 3-digits then the arguments should be `from = 4` and `to = 3`. See the argument description of `from` and `to` for all possible values. As well as examples on how this works.
#'
#' `r rg_template_digits_warning(digit = 4) %>% gsub("4-digit", "3-digit", .)`
#'
#' Note that translation can only be done from higher to smaller digits (4 to 3, 3 to 2, 3 to 1) and never the other way around (1 to 2, 2 to 3, 3 to 4).
#'
#' ISCO68 might return some NAs depending on the occupation code as it does not have 4 digits for the groups 0000 and 1000. Any translation from 4 digit codes to 1 digit codes within those groups will return an NA for those major groups. See the ILO website: \url{https://www.ilo.org/public/english/bureau/stat/isco/isco68/major.htm}.
#'
#'
#' @param x A character vector of 4-digit ISCO08/ISCO88/ISCO88 codes. By 4 digit it means that even though the function could be 3-digits (code 131 for example), the code should be 1310, which is the 4-digit version of ISCO.
#'
#' @param from a numeric specifying the occupation digits of the input vector. Possible values are only 1, 2, 3 or 4.
#' @param to a numeric specifying the desired occupation digits. Possible values are only 1, 2, 3 or 4.
#'
#' @return `r rg_template_return("ISCO08/ISCO88/ISCO88")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco08
#' ess %>%
#'   transmute(
#'     isco08,
#'     isco08_one = isco08_swap(isco08, from = 4, to = 1),
#'     isco08_two = isco08_swap(isco08, from = 4, to = 2),
#'     isco08_three = isco08_swap(isco08, from = 4, to = 3),
#'     isco08_four = isco08_swap(isco08, from = 4, to = 4)
#' )
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     isco88_one = isco88_swap(isco88, from = 4, to = 1),
#'     isco88_two = isco88_swap(isco88, from = 4, to = 2),
#'     isco88_three = isco88_swap(isco88, from = 4, to = 3),
#'     isco88_four = isco88_swap(isco88, from = 4, to = 4)
#' )
#'
#' # isco68
#' # Note that for certain four digit groups, isco68 does not have a
#' # major group (0000, 1000). That means that Some NAs might be present,
#' # such as for occupations that are between 1000 and 200. Remember to
#' # check well the result.
#' ess %>%
#'   transmute(
#'     isco68,
#'     isco68_one = isco68_swap(isco68, from = 4, to = 1),
#'     isco68_two = isco68_swap(isco68, from = 4, to = 2),
#'     isco68_three = isco68_swap(isco68, from = 4, to = 3),
#'     isco68_four = isco68_swap(isco68, from = 4, to = 4)
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
