#' `r rg_template_title("ISCO88", "ISCO68")`
#'
#' `r rg_template_intro("ISCO88", "ISCO68", "isco88_to_isco68")`
#'
#' @details`r rg_template_details_iscogen("ISCO88", "ISCO68")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("ISCO68")`
#'
#' @return `r rg_template_return("ISCO68")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     isco68 = isco88_to_isco68(isco88, label = FALSE),
#'     isco68_label = isco88_to_isco68(isco88, label = TRUE)
#'   )
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

#' `r rg_template_title("ISCO88/ISCO68", "ISCO08")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "ISCO08", c("isco88_to_isco08", "isco68_to_isco08"))`
#'
#' @details`r rg_template_details_iscogen("ISCO88/ISCO68", "ISCO08")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("ISCO08")`
#'
#' @return `r rg_template_return("ISCO08")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     isco08 = isco88_to_isco08(isco88, label = FALSE),
#'     isco08_label = isco88_to_isco08(isco88, label = TRUE)
#' )
#'
#' # isco68
#' ess %>%
#'   transmute(
#'     isco68,
#'     isco08 = isco68_to_isco08(isco68, label = FALSE),
#'     isco08_label = isco68_to_isco08(isco68, label = TRUE)
#' )
#'
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


#' `r rg_template_title("ISCO88", "ISCO88COM")`
#'
#' `r rg_template_intro("ISCO88", "ISCO88COM", "isco88_to_isco88com")`
#'
#' @details`r rg_template_details_iscogen("ISCO88", "ISCO88COM")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("ISCO88COM")`
#'
#' @return `r rg_template_return("ISCO88COM")`
#'

#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco88,
#'     isco88com = isco88_to_isco88com(isco88, label = FALSE),
#'     isco88com_label = isco88_to_isco88com(isco88, label = TRUE)
#' )
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

#' `r rg_template_title("ISCO88", "MPS")`
#'
#' `r rg_template_intro("ISCO88", "MPS", "isco88_to_mps")`
#'
#' @details`r rg_template_details_iscogen("ISCO88", "MPS")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#'
#' @return `r rg_template_return("MPS")`
#'
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco88,
#'     mps = isco88_to_mps(isco88),
#'   )
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

#' `r rg_template_title("ISCO88/ISCO68", "EGP")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP", c("isco88_to_egp", "isco68_to_egp"))`
#'
#' @details`r rg_template_details_iscogen("ISCO88/IS68", "EGP")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("EGP")`
#'
#' @return `r rg_template_return("EGP")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
#' ess %>% transmute(
#'   isco88,
#'   egp = isco88_to_egp(isco88, self_employed, emplno, label = FALSE),
#'   egp_label = isco88_to_egp(isco88, self_employed, emplno, label = TRUE)
#' )
#'
#' # isco68
#' ess %>% transmute(
#'   isco68,
#'   egp = isco68_to_egp(isco68, self_employed, emplno, label = FALSE),
#'   egp_label = isco68_to_egp(isco68, self_employed, emplno, label = TRUE)
#' )
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


#' `r rg_template_title("ISCO88/ISCO68", "EGP11")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP11", c("isco88_to_egp11", "isco68_to_egp11"))`
#'
#' @details`r rg_template_details_iscogen("ISCO88/ISCO68", "EGP11")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("EGP11")`
#'
#' @return `r rg_template_return("EGP11")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
#' ess %>% transmute(
#'   isco88,
#'   egp11 = isco88_to_egp11(isco88, self_employed, emplno, label = FALSE),
#'   egp11_label = isco88_to_egp11(isco88, self_employed, emplno, label = TRUE)
#' )
#'
#' # isco68
#' ess %>% transmute(
#'   isco68,
#'   egp11 = isco68_to_egp11(isco68, self_employed, emplno, label = FALSE),
#'   egp11_label = isco68_to_egp11(isco68, self_employed, emplno, label = TRUE)
#' )
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


#' `r rg_template_title("ISCO88/ISCO68", "EGP-MP")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP-MP", c("isco88_to_egp", "isco68_to_egp"))` After translating to EGP using these tables, this function reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals
#'
#' @details
#'
#' EGP-MP is a class schema similar to EGP but reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals.
#'
#' # TODO: After this is corrected in the code, correct it here and in all docs.
#' This schema is a slight variation of the original EGP and the logic used to build this is like this:
#'
#' * All occupations with EGP digit 1 and ISCO 1-digit 0 or 1 or has subordinates, **is a high manager**
#' * All occupations with EGP digit 1 and is self-employed with more than 1 employee, **is a high manager**
#' * All occupations with EGP digit 1 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **high professional**
#'
#' * All occupations with EGP digit 2 and ISCO 1-digit 0 or 1 or has subordinates, is a **lower manager**
#' * All occupations with EGP digit 2 and is self-employed with more than 1 employee, is a **lower manager**
#' * All occupations with EGP digit 2 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **lower professional**
#'
#' This translation was created from the Stata do file shared by Oscar Smallenbroek called "EGP-MP.do". For more info, please contact the author.
#'
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("EGP-MP")`
#'
#' @return `r rg_template_return("EGP-MP")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
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
#'
#' # isco68
#' ess %>%
#'   transmute(
#'     isco68,
#'     egp_mp = isco68_to_egp_mp(
#'       isco68,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     egp_mp_label = isco68_to_egp_mp(
#'       isco68,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
#'
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


#' `r rg_template_title("ISCO88/ISCO68", "EGP11-MP")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP11-MP", c("isco88_to_egp11", "isco68_to_egp11"))` After translating to EGP11 using these tables, this function reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals
#'
#' @details
#'
#' EGP11-MP is a class schema similar to EGP11 but reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals.
#'
#' # TODO: After this is corrected in the code, correct it here and in all docs.
#' This schema is a slight variation of the original EGP11 and the logic used to build this is like this:
#'
#' * All occupations with EGP11 digit 1 and ISCO 1-digit 0 or 1 or has subordinates, **is a high manager**
#' * All occupations with EGP11 digit 1 and is self-employed with more than 1 employee, **is a high manager**
#' * All occupations with EGP11 digit 1 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **high professional**
#'
#' * All occupations with EGP11 digit 2 and ISCO 1-digit 0 or 1 or has subordinates, is a **lower manager**
#' * All occupations with EGP11 digit 2 and is self-employed with more than 1 employee, is a **lower manager**
#' * All occupations with EGP11 digit 2 and has a 1-digit ISCO higher than 1 and is either an employee or a self-employed with no subordinates, is a **lower professional**
#'
#' This translation was created from the Stata do file shared by Oscar Smallenbroek called "EGP-MP.do". For more info, please contact the author.
#'
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("EGP11-MP")`
#'
#' @return `r rg_template_return("EGP11-MP")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     egp11_mp = isco88_to_egp11_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     egp11_mp_label = isco88_to_egp11_mp(
#'       isco88,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     )
#'   )
#'
#' # isco68
#' ess %>%
#'   transmute(
#'     isco68,
#'     egp11_mp = isco68_to_egp11_mp(
#'       isco68,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE
#'     ),
#'     egp11_mp_label = isco68_to_egp11_mp(
#'       isco68,
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




#' @rdname isco08_to_oesch
#' @order 2
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
