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
#'   )
#'
#' # isco68
#' ess %>%
#'   transmute(
#'     isco68,
#'     isco08 = isco68_to_isco08(isco68, label = FALSE),
#'     isco08_label = isco68_to_isco08(isco68, label = TRUE)
#'   )
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
#'   )
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

#' `r rg_template_title("ISCO88/ISCO68", "EGP11/EGP7/EGP5/EGP3")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP11/EGP7/EGP5/EGP3", c("isco88_to_egp11", "isco68_to_egp11", "egp11_to_egp7", "egp11_to_egp5", "egp11_to_egp3"))`
#'
#' @details This function works by first translating to EGP11 and then translating to other EGP variants, if the user has requested this through the `n_classes` argument.
#'
#' `r rg_template_details_iscogen("ISCO88/IS68", "EGP11")` For translations between EGP11 and EGP7/EGP5/EGP3, see the source of the `occupar` R package [here](https://github.com/DiogoFerrari/occupar/blob/7130d94438f1da2a4aac4731437991a8eea88436/R/occupar_occupation.R#L226-L288).
#'
#' For more details, users can see the translation used in this package in `all_schema$egp11_to_egp7`, `all_schema$egp11_to_egp5` and `all_schema$egp11_to_egp3`. Moreover, the labels used can be found in `all_labels$egp11`, `all_labels$egp7`, `all_labels$egp5` and `all_labels$egp3`.
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param n_classes a numeric value indicating the number of EGP classes to obtain. Default is 11 EGP classes. The possible values are 11 classes, 7 classes, 5 classes and 3 classes. For more information, see the details section.
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
#'   egp11 = isco88_to_egp(isco88, self_employed, emplno, label = FALSE),
#'   egp7 = isco88_to_egp(isco88, self_employed, emplno, n_classes = 7, label = FALSE),
#'   egp5 = isco88_to_egp(isco88, self_employed, emplno, n_classes = 5, label = FALSE),
#'   egp3 = isco88_to_egp(isco88, self_employed, emplno, n_classes = 3, label = FALSE),
#' )
#'
#' # isco68
#' ess %>% transmute(
#'   isco68,
#'   egp11 = isco68_to_egp(isco68, self_employed, emplno, label = FALSE),
#'   egp7 = isco68_to_egp(isco68, self_employed, emplno, n_classes = 7, label = FALSE),
#'   egp5 = isco68_to_egp(isco68, self_employed, emplno, n_classes = 5, label = FALSE),
#'   egp3 = isco68_to_egp(isco68, self_employed, emplno, n_classes = 3, label = FALSE)
#' )
#'
#' @export
isco88_to_egp <- function(x, self_employed, n_employees, n_classes = 11, label = FALSE) {
  stopifnot(n_classes %in% c(11, 7, 5, 3))
  stopifnot(length(n_classes) == 1)

  col_position <- dplyr::case_when(
    self_employed == 0 & n_employees == 0 ~ 2,
    self_employed == 0 & dplyr::between(n_employees, 1, 9) ~ 3,
    self_employed == 0 & n_employees >= 10 ~ 4,
    self_employed == 1 & n_employees == 0 ~ 5,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 6,
    self_employed == 1 & n_employees >= 10 ~ 7,
  )

  schema <- all_schemas$isco88_to_egp11
  input_var <- "EGP11"
  output_var <- paste0("EGP", n_classes)
  all_classes <-
    list(
      `7` = list(all_schemas$egp11_to_egp7, all_labels$egp7),
      `5` = list(all_schemas$egp11_to_egp5, all_labels$egp5),
      `3` = list(all_schemas$egp11_to_egp3, all_labels$egp3)
    )


  if (n_classes == 11) {
    egp11 <-
      multiple_cols_translator(
        x = x,
        col_position = col_position,
        output_var = "EGP11",
        translate_df = schema,
        translate_label_df = all_labels$egp11,
        label = label
      )

    return(egp11)
  } else {
    egp <- main_schema_to_others(
      x,
      col_position,
      n_classes,
      schema,
      input_var,
      output_var,
      all_classes,
      label
    )
    return(egp)
  }
}


#' `r rg_template_title("ISCO88/ISCO68", "EGP-MP")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP-MP", c("isco88_to_egp11", "isco68_to_egp11"))` After translating to EGP using these tables, this function reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals. Note that this function translates to EGP11 (not EGP7/EGP5/EGP3) and then reassigns categories to have both high/low managers and professionals.
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
#' **Note that this translation uses EGP11.**
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


#' @rdname isco08_to_esec
#' @order 2
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
isco88_to_oesch <- function(x, self_employed, n_employees, n_classes = 16, label = FALSE) {
  stopifnot(n_classes %in% c(16, 8, 5))
  stopifnot(length(n_classes) == 1)

  col_position <- dplyr::case_when(
    self_employed == 0 ~ 2,
    self_employed == 1 & n_employees == 0 ~ 3,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 4,
    self_employed == 1 & n_employees >= 10 ~ 5,
  )

  schema <- all_schemas$isco88_to_oesch16
  input_var <- "OESCH16"
  output_var <- paste0("OESCH", n_classes)

  all_classes <-
    list(
      `8` = list(all_schemas$oesch16_to_oesch8, all_labels$oesch8),
      `5` = list(all_schemas$oesch16_to_oesch5, all_labels$oesch5)
    )

  if (n_classes == 16) {
    oesch16 <-
      multiple_cols_translator(
        x = x,
        col_position = col_position,
        output_var = input_var,
        translate_df = schema,
        translate_label_df = all_labels$oesch16,
        label = label
      )

    return(oesch16)
  } else {
    oesch <- main_schema_to_others(
      x,
      col_position,
      n_classes,
      schema,
      input_var,
      output_var,
      all_classes,
      label
    )

    return(oesch)
  }
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
