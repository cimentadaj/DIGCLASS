#' `r rg_template_title("ISCO08/ISCO88COM", "ESEC", digit = 3)`
#'
#' `r rg_template_intro("ISCO08/ISCO08COM", "ESEC", c("isco08_to_esec", "isco88com_to_esec_three"), digit = 3)`
#'
#' @details `r rg_template_details_iscogen("ISCO08/ISCO88COM", "ESEC")`
#'
#' `r rg_template_digits_warning(digit = 3)`
#'
#' ISCO88COM has two types of translations: simple and full method. The full method uses information on whether the respondent is a supervisor, self-employed and the number of subordinates of the employee. In contrast, the simple method matches directly the ISCO code to an ESEC code.
#'
#' For more info, please see page 17 of the European Socio-economic Classification (ESeC) User Guide (2006) by Rode, D. and Harrison, E.
#'
#'
#' The translation for ISCO88 is done from ISCO88COM which is not ISCO88. If you have ISCO88, you can translate it to ISCO88COM using the function `DIGCLASS::isco88_to_isco88com` before translating to ESEC.
#'
#'
#' Contrary to ISCO88COM-ESEC, ISCO08 does not have a simplified method and the translation is done from ISCO08 directly to ESEC.
#'
#' @param x `r rg_template_arg_x_digit("ISCO", digit = 3)`
#' @param is_supervisor `r rg_template_arg_supervisor()`
#' @param self_employed `r rg_template_arg_selfemployed()`
#' @param n_employees `r rg_template_arg_nemployees()`
#' @param full_method a boolean on whether to apply the full method or the simple method.
#' @param label `r rg_template_arg_label("ESEC")`
#'
#' @return `r rg_template_return("ESEC")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # convert isco08 to three digits
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
#' # convert isco88 to three digits
#' ess$isco88com_three <- isco88_swap(ess$isco88com, from = 4, to = 3)
#'
#' # Using the full method
#' ess %>%
#'   transmute(
#'     isco88com_three,
#'     esec_label = isco88com_to_esec(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE,
#'       full_method = TRUE
#'     ),
#'     esec_no_label = isco88com_to_esec(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = FALSE,
#'       full_method = TRUE
#'     )
#'   )
#'
#' # Using the simple method
#' ess %>%
#'   transmute(
#'     isco88com_three,
#'     esec_simple = isco88com_to_esec(
#'       isco88com_three,
#'       label = FALSE,
#'       full_method = FALSE
#'     ),
#'     esec_simple_label = isco88com_to_esec(
#'       isco88com_three,
#'       label = TRUE,
#'       full_method = FALSE
#'     )
#'   )
#'
#' @export
isco08_to_esec <- function(x,
                           is_supervisor,
                           self_employed,
                           n_employees,
                           label = FALSE) {
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
    digits = 3
  )

  res
}

#' `r rg_template_title("ISCO08", "ESEC", digit = 2)`
#'
#' `r rg_template_intro("ISCO08", "ESEC", "isco08_two_to_esec", digit = 2)`
#'
#' `r rg_template_digits_warning(digit = 2)`
#'
#' This is exactly the same as `DIGCLASS::isco08_to_esec` but for two digit ISCO.
#'
#' @param x `r rg_template_arg_x_digit("ISCO", digit = 2)`
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
    digits = 2
  )

  res
}





#' `r rg_template_title("ISCO08/ISCO88COM", "ESEC-MP", digit = 3)`
#'
#' `r rg_template_intro("ISCO08/ISCO88COM", "ESEC-MP", c("isco08_to_esec", "isco88com_to_esec_three"), digit = 3)` After translating to ESEC using these tables, this function reassigns managers and professionals (ISCO08/ISCO88COM codes 1 and 2) to have both high/low managers and profesionals
#'
#' @details
#'
#' This function translates a vector of 3-digit ISCO08/ISCO88COM codes to ESEC-MP codes.
#'
#' ESEC-MP is a class schema similar to ESEC but reassigns managers and professionals (ISCO08/ISCO88COM codes 1 and 2) to have both high/low managers and profesionals. Similarly to `DIGCLASS::isco88com_to_esec`, `isco88com_to_esec_mp` allows to translate using the simple or full method. `isco08_to_esec_mp` does not allow to translate using two different methods and uses the full method by default.
#'
#' # TODO: After this is corrected in the code, correct it here and in all docs.
#' This schema is a slight variation of the original ESEC and the logic used to build this is like this:
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
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("ESEC-MP")`
#'
#' @return `r rg_template_return("ESEC-MP")`
#'
#' @order 1
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
#' # Convert isco88com to three digits
#' ess$isco88com_three <- isco88_swap(ess$isco88com, from = 4, to = 3)
#'
#' # Using the full method
#' ess %>%
#'   transmute(
#'     isco88com_three,
#'     esec = isco88com_to_esec_mp(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       full_method = TRUE,
#'       label = FALSE
#'     ),
#'     esec_label = isco88com_to_esec_mp(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       full_method = TRUE,
#'       label = TRUE
#'     )
#'   )
#'
#' # Using the simple method. For esec_mp
#' # we need all variables (is_supervisor, self_employed, etc..)
#' # because we need to assign the manager/professionals depending
#' # these variables.
#' ess %>%
#'   transmute(
#'     isco88com_three,
#'     esec_simple = isco88com_to_esec_mp(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       full_method = FALSE,
#'       label = FALSE
#'     ),
#'     esec_simple_label = isco88com_to_esec_mp(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       full_method = FALSE,
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




#' `r rg_template_title("ISCO08/ISCO68", "ISCO88")`
#'
#' `r rg_template_intro("ISCO08/ISCO68", "ISCO88", c("isco08_to_isco88", "isco68_to_isco88"))`
#'
#' @details`r rg_template_details_iscogen("ISCO08/ISCO68", "ISCO88")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("ISCO88")`
#'
#' @return `r rg_template_return("ISCO88")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # isco08
#' ess %>% transmute(
#'   isco08,
#'   isco88 = isco08_to_isco88(isco08, label = FALSE),
#'   isco88_label = isco08_to_isco88(isco08, label = TRUE)
#' )
#'
#' # isco68
#' ess %>% transmute(
#'   isco68,
#'   isco88 = isco68_to_isco88(isco68, label = FALSE),
#'   isco88_label = isco68_to_isco88(isco68, label = TRUE)
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


#' `r rg_template_title("ISCO08/ISCO88/ISCO68", "ISEI")`
#'
#' `r rg_template_intro("ISCO08/ISCO88/ISCO68", "ISEI", paste0("isco", c('08', '88', '68'), "_to_isei"))`
#'
#' @details`r rg_template_details_iscogen("ISCO08/ISCO88/ISCO88", "ISEI")`
#'
#' Since `ISEI` doesn't have any labels, the `labels` is not availabe in this function.
#'
#' @param x `r rg_template_arg_x("ISCO")`
#'
#' @return `r rg_template_return("ISEI")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco08,
#'     isco88,
#'     isco68,
#'     isei_08 = isco08_to_isei(isco08),
#'     isei_88 = isco88_to_isei(isco88),
#'     isei_68 = isco68_to_isei(isco68)
#'   )
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


#' `r rg_template_title("ISCO08/ISCO88/ISCO68", "SIOPS")`
#'
#' `r rg_template_intro("ISCO08/ISCO88/ISCO68", "SIOPS", paste0("isco", c('08', '88', '68'), "_to_siops"))`
#'
#' @details`r rg_template_details_iscogen("ISCO08/ISCO88/ISCO88", "SIOPS")`
#'
#' Since `SIOPS` doesn't have any labels, the `labels` is not availabe in this function.
#'
#' @param x `r rg_template_arg_x("ISCO")`
#'
#' @return `r rg_template_return("SIOPS")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' ess %>%
#'   transmute(
#'     isco08,
#'     isco88,
#'     isco68,
#'     siops_08 = isco08_to_siops(isco08),
#'     siops_88 = isco88_to_siops(isco88),
#'     siops_68 = isco68_to_siops(isco68)
#'   )
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

#' `r rg_template_title("ISCO08/ISCO88COM", "MSEC", digit = 3)`
#'
#'
#' `r rg_template_intro("ISCO08/ISCO88COM", "MSEC", paste0("isco", c('08', '88com'), "_to_msec"), digit = 3)`
#'
#' These translations were created from the document "Allocation rules of ISCO-08 and ISCO-88 (COM) 3-digit codes to ESEG-Revised" from Oscar Smallenbroek, Florian Hertel and Carlo Barone. For more info, please contact the authors. Although originally called ESEG-Revised, the class schema has been formally called MSEC.
#'
#' `r rg_template_digits_warning(digit = 3)`
#'
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("MSEC")`
#'
#' @return `r rg_template_return("MSEC")`
#'
#' @order 1
#'
#' @examples
#' library(dplyr)
#'
#' # convert to three digits
#' ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)
#' ess$isco88com_three <- isco88_swap(ess$isco88com, from = 4, to = 3)
#'
#' # isco08
#' ess %>%
#'   transmute(
#'     isco08_three,
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
#' # isco88com
#' ess %>%
#'   transmute(
#'     isco88com_three,
#'     msec_label = isco88com_to_msec(
#'       isco88com_three,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       label = TRUE
#'     ),
#'     msec = isco88com_to_msec(
#'       isco88com_three,
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
    digits = 3
  )

  res
}

#' `r rg_template_title("ISCO08", "microclass")`
#'
#' `r rg_template_intro("ISCO08", "microclass", "isco08_to_microclass")`
#'
#' @details This translation was created from the Excel file shared by Oscar Smallenbroek called "isco08 to micro with numeric labels.xlsx". For more info, please contact the author.
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("microclass")`
#'
#' @return `r rg_template_return("microclass")`
#'
#' @examples
#' library(dplyr)
#'
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




#' `r rg_template_title("ISCO08/ISCO88", "OESCH16/OESCH8/OESCH5")`
#'
#' `r rg_template_intro("ISCO08/ISCO88", "OESCH", c("isco08_to_oesch16", "isco88_to_oesch16", "oesch16_to_oesch8", "oesch16_to_oesch5"))`
#'
#' @details This function works by first translating to OESCH16 and then translating to other OESCH variants, if the user has requested this through the `n_classes` argument.
#'
#' `r rg_template_details_iscogen("ISCO88/IS68", "OESCH")` For translations between OESCH16 and OESCH8/OESCH5, see the source of the Stata package `oesch` [here](http://fmwww.bc.edu/repec/bocode/o/oesch.ado).
#'
#' For more details, users can see the translation used in this package in `all_schema$oesch16_to_oesch8` and `all_schema$oesch16_to_oesch5`. Moreover, the labels used can be found in `all_labels$oesch16`, `all_labels$oesch8` and `all_labels$oesch5`.
#
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param n_classes a numeric value indicating the number of OESCH classes to obtain. Default is 16 OESCH classes. The possible values are 16 classes, 8 classes and 5 classes. For more information, see the details section.
#' @param label `r rg_template_arg_label("OESCH")`
#' @return `r rg_template_return("OESCH")`
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
#'     oesch16 = isco08_to_oesch(isco08, self_employed, emplno, label = FALSE),
#'     oesch8 = isco08_to_oesch(isco08, self_employed, emplno, n_classes = 8, label = FALSE),
#'     oesch5 = isco08_to_oesch(isco08, self_employed, emplno, n_classes = 5, label = FALSE),
#'   )
#'
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     oesch16 = isco88_to_oesch(isco88, self_employed, emplno, label = FALSE),
#'     oesch8 = isco88_to_oesch(isco88, self_employed, emplno, n_classes = 8, label = FALSE),
#'     oesch5 = isco88_to_oesch(isco88, self_employed, emplno, n_classes = 5, label = FALSE),
#'   )
#'
#' @export
isco08_to_oesch <- function(x, self_employed, n_employees, n_classes = 16, label = FALSE) {
  stopifnot(n_classes %in% c(16, 8, 5))
  stopifnot(length(n_classes) == 1)

  col_position <- dplyr::case_when(
    self_employed == 0 ~ 2,
    self_employed == 1 & n_employees == 0 ~ 3,
    self_employed == 1 & dplyr::between(n_employees, 1, 9) ~ 4,
    self_employed == 1 & n_employees >= 10 ~ 5,
  )

  schema <- all_schemas$isco08_to_oesch16
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

#' Translates 2-digit ISCO08 to ESEG.
#'
#' This function translates a vector of 2-digit ISCO88COM codes to the ESEG class schema.
#'
#' @details The translation was implemented following the work of Kea Tijdens in the document "ESEG-2014 coding scheme + explanatory note". For more details, see the last table in the document [here](https://seriss.eu/wp-content/uploads/2016/12/SERISS-Deliverable-D8-13-ESeG-coding_submitted.pdf).
#'
#' @param x `r rg_template_arg_x_digit("ISCO", digit = 2)`
#'
#' @param work_status A numeric vector of values from 0 to 3 where `1 = self_employed`, `0 = employee` and `2 = non employed`.
#'
#' @param main_activity A numeric vector of values from 1 to 5 where `1 = respondent is working`, `2 = respondent is in education`, `3 = respondent is disabled `, `4 = respondent has no paid work (household work, taking care of children, etc..)` and `5 = respondent is retired`. For an example, see the variable `mainact` from the European Social Survey.
#'
#' @param age A numeric vector of ages of the respondent.
#'
#' @param type The type of translation to make. Possible values are "one-digit" and "two-digit". The "one-digit" translation returns a broad summary based translation of only 9 categories, whereas the "two-digit" translation returns a much bigger ESEG translation of more than 25 categories.
#'
#' @param label `r rg_template_arg_label("ESEG")`
#'
#' @examples
#' library(dplyr)
#'
#' # Convert isco08 to two digits
#' ess$isco08_two <- isco08_swap(ess$isco08, from = 4, to = 2)
#'
#' # Using the two-digit translation
#' ess %>%
#'   transmute(
#'     isco08_two,
#'     eseg = isco08_to_eseg(
#'       isco08_two,
#'       work_status,
#'       main_activity,
#'       agea,
#'       type = "two-digit"
#'     ),
#'     eseg_label = isco08_to_eseg(
#'       isco08_two,
#'       work_status,
#'       main_activity,
#'       agea,
#'       type = "two-digit",
#'       label = TRUE
#'     )
#'   )
#'
#' # Using the one-digit translation
#' ess %>%
#'   transmute(
#'     isco08_two,
#'     eseg = isco08_to_eseg(
#'       isco08_two,
#'       work_status,
#'       main_activity,
#'       agea,
#'       type = "one-digit"
#'     ),
#'     eseg_label = isco08_to_eseg(
#'       isco08_two,
#'       work_status,
#'       main_activity,
#'       agea,
#'       type = "one-digit",
#'       label = TRUE
#'     )
#'   )
#'
#' @export
isco08_to_eseg <- function(x,
                           work_status,
                           main_activity,
                           age,
                           type,
                           label = FALSE) {

  x <- repair_isco(x, digits = 4)
  count_digits(x, digits = 2)
  isco1 <- substr(x, 1, 1)
  isco2 <- substr(x, 1, 2)

  construct_eseg(
    isco1,
    isco2,
    work_status,
    main_activity,
    age,
    type = type,
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
#'   )
#'
#' # isco88
#' ess %>%
#'   transmute(
#'     isco88,
#'     isco88_one = isco88_swap(isco88, from = 4, to = 1),
#'     isco88_two = isco88_swap(isco88, from = 4, to = 2),
#'     isco88_three = isco88_swap(isco88, from = 4, to = 3),
#'     isco88_four = isco88_swap(isco88, from = 4, to = 4)
#'   )
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
#'   )
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
    label = FALSE,
  )
}
