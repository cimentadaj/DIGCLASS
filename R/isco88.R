#' `r rg_template_title("ISCO88", "ISCO68")`
#'
#' `r rg_template_intro("ISCO88", "ISCO68", "isco88_to_isco68")`
#'
#' @details`r rg_template_details_iscogen("ISCO88", "ISCO68")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param label `r rg_template_arg_label("ISCO68")`
#' @param to_factor `r rg_template_arg_factor("ISCO68")`
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
isco88_to_isco68 <- function(x, label = FALSE, to_factor = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO68",
    translate_df = all_schemas$isco88_to_isco68,
    translate_label_df = all_labels$isco68,
    check_isco = "isco88",
    label = label,
    to_factor = to_factor
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
#' @param to_factor `r rg_template_arg_factor("ISCO08")`
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
isco88_to_isco08 <- function(x, label = FALSE, to_factor = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO08",
    translate_df = all_schemas$isco88_to_isco08,
    translate_label_df = all_labels$isco08,
    check_isco = "isco88",
    label = label,
    to_factor = to_factor
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
#' @param to_factor `r rg_template_arg_factor("ISCO88COM")`
#'
#' @return `r rg_template_return("ISCO88COM")`
#'
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
isco88_to_isco88com <- function(x, label = FALSE, to_factor = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO88COM",
    translate_df = all_schemas$isco88_to_isco88com,
    translate_label_df = all_labels$isco88com,
    check_isco = "isco88",
    label = label,
    to_factor = to_factor
  )
}

#' @rdname isco08_to_isei
#' @order 2
#' @export
isco88_to_isei <- function(x, to_factor = FALSE) {

  translate_label_df <-
    dplyr::relocate(all_schemas$isco88_to_isei, 2, 1) %>%
    dplyr::arrange(dplyr::pick(dplyr::contains("ISEI")))

  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISEI",
    translate_df = all_schemas$isco88_to_isei,
    # Although this transformation does not allow labels, this is
    # just to be used for translating to a factor if requested.
    translate_label_df = translate_label_df,
    check_isco = "isco88",
    label = FALSE,
    to_factor = to_factor
  )
}


#' @rdname isco08_to_siops
#' @order 2
#' @export
isco88_to_siops <- function(x, to_factor = FALSE) {

  translate_label_df <-
    dplyr::relocate(all_schemas$isco88_to_siops, 2, 1) %>%
    dplyr::arrange(dplyr::pick(dplyr::contains("SIOPS")))

  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "SIOPS",
    translate_df = all_schemas$isco88_to_siops,
    # Although this transformation does not allow labels, this is
    # just to be used for translating to a factor if requested.
    translate_label_df = translate_label_df,
    check_isco = "isco88",
    label = FALSE,
    to_factor = to_factor
  )
}

#' `r rg_template_title("ISCO88", "MPS")`
#'
#' `r rg_template_intro("ISCO88", "MPS", "isco88_to_mps")`
#'
#' @details`r rg_template_details_iscogen("ISCO88", "MPS")`
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param to_factor A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the sorted codes of MPS in `all_schemas$isco88_to_mps`.
#
#'
#' @return `r rg_template_return("MPS")`
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
isco88_to_mps <- function(x, to_factor = FALSE) {
  translate_label_df <-
    dplyr::relocate(all_schemas$isco88_to_mps, 2, 1) %>%
    dplyr::arrange(dplyr::pick(dplyr::contains("MPS")))

  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "MPS88",
    translate_df = all_schemas$isco88_to_mps,
    # Although this transformation does not allow labels, this is
    # just to be used for translating to a factor if requested.
    translate_label_df = translate_label_df,
    check_isco = "isco88",
    label = FALSE,
    to_factor = to_factor
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
#' Users can see the translation used in this package in `all_schema$egp11_to_egp7`, `all_schema$egp11_to_egp5` and `all_schema$egp11_to_egp3`. Moreover, the labels used can be found in `all_labels$egp11`, `all_labels$egp7`, `all_labels$egp5` and `all_labels$egp3`.
#'
#' For more details on this class schema, please check the references below:
#'
#' * Erikson, R., Goldthorpe, J.H. and Portocarero, L (1979) Intergenerational Class Mobility inThree Western European Societies: England, France and Sweden’, British Journal of Sociology, 30.
#'
#' * Erikson, R., J.H. Goldthorpe, L. Portocarero (1983) Intergenerational Class Mobility and the Convergence Thesis: England, France and Sweden. British Journal of Sociology, 34(3): 303-343.
#'
#' * Erikson R, Goldthorpe JH (1992) The Constant Flux: A Study of Class Mobility in Industrial Societies. Oxford: Clarendon Press.
#'
#' * Goldthorpe class scheme. Oxford Reference. Retrieved 19 May. 2023, from [https://www.oxfordreference.com/view/10.1093/oi/authority.20110803095858703](https://www.oxfordreference.com/view/10.1093/oi/authority.20110803095858703).
#'
#' * Goldthorpe JH (2007) Social class and the differentiation of employment contracts. In: Goldthorpe JH (ed.) On Sociology: Volume Two. Stanford: Stanford University Press,101–124.
#'
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param n_classes a numeric value indicating the number of EGP classes to obtain. Default is 11 EGP classes. The possible values are 11 classes, 7 classes, 5 classes and 3 classes. For more information, see the details section.
#' @param label `r rg_template_arg_label("EGP")`
#' @param to_factor `r rg_template_arg_factor("EGP")`
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
isco88_to_egp <- function(x, self_employed, n_employees, n_classes = 11, label = FALSE, to_factor = FALSE) {
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
        check_isco = "isco88",
        label = label,
        to_factor = to_factor
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
      check_isco = "isco88",
      label,
      to_factor = to_factor
    )
    return(egp)
  }
}


#' `r rg_template_title("ISCO88/ISCO68", "EGP-MP")`
#'
#' `r rg_template_intro("ISCO88/ISCO68", "EGP-MP", c("isco88_to_egp11", "isco68_to_egp11"))` After translating to EGP using these tables, this function reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals. Note that this function translates to EGP11 (not EGP7/EGP5/EGP3) and then reassigns categories to have both high/low managers and professionals. **Note that this translation uses EGP11.**
#'
#' @details
#'
#' EGP-MP is a class schema similar to EGP but reassigns managers and professionals (ISCO88/ISCO68 codes 1 and 2) to have both high/low managers and profesionals.
#'
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
#' For more information on this class schema, please checkthe references below:
#
#' * Smallenbroek O, Hertel F, Barone C (2022) Measuring class hierarchies in post-industrial societies: a criterion and construct validation of EGP and ESEC across 31 countries. Sociological Methods &amp; Research. Epub ahead of print 11 November. [https://doi.org/10.1177/00491241221134522](https://doi.org/10.1177/00491241221134522)
#
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @inheritParams isco08_to_esec
#' @param label `r rg_template_arg_label("EGP-MP")`
#' @param to_factor A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the sorted codes of EGP-MP which can be found in the source code of each function.
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
                             label = FALSE,
                             to_factor = FALSE) {
  egp <- isco88_to_egp(
    x,
    self_employed,
    n_employees,
    label = FALSE
  )

  lookup_egp <- stats::setNames(as.character(5:13), as.character(3:11))
  labs <- c(
    "Ia Higher Managers",
    "Ib Lower Managers",
    "IIa Higher Professionals",
    "IIb Lower Professionals",
    "IIIa Routine Nonmanual",
    "IIIb Lower Sales-Service",
    "IVa Self-employed with employees",
    "IVb Self-employed with no employees",
    "V Manual Supervisors",
    "VI Skilled Worker",
    "VIIa Unskilled Worker",
    "VIIb Farm Labor",
    "IVc Self-employed Farmer"
  )

  labs <- stats::setNames(labs, as.character(1:13))

  egp_mp <- managers_professionals_helper(
    x,
    egp,
    is_supervisor,
    self_employed,
    n_employees,
    lookup_labels = lookup_egp,
    schema_labels = labs,
    label = label,
    to_factor = to_factor
  )

  egp_mp
}

#' @rdname isco08_to_ipics
#' @order 2
#' @export
isco88_to_ipics <- function(x, self_employed, n_employees, label = FALSE, to_factor = FALSE) {
  col_position <- dplyr::case_when(
    self_employed == 0 & n_employees == 0 ~ 2,
    self_employed == 0 & dplyr::between(n_employees, 1, 10) ~ 3,
    self_employed == 1 & n_employees == 0 ~ 4,
    self_employed == 1 & dplyr::between(n_employees, 1, 10) ~ 5,
    self_employed == 1 & n_employees > 11 ~ 6
  )

  res <- multiple_cols_translator(
    x = x,
    col_position = col_position,
    output_var = "IPICS",
    translate_df = all_schemas$isco88_to_ipics,
    translate_label_df = all_labels$ipics,
    check_isco = "isco88",
    label = label,
    digits = 4,
    to_factor = to_factor
  )
}


#' @rdname isco08_to_esec
#' @order 2
#' @export
isco88com_to_esec <- function(x,
                              is_supervisor,
                              self_employed,
                              n_employees,
                              full_method = TRUE,
                              label = FALSE,
                              to_factor = FALSE) {
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
      check_isco = "isco88com",
      digits = 3,
      to_factor = to_factor
    )
  } else {
    res <- common_translator(
      x,
      input_var = "ISCO88(3-digit)",
      output_var = "ESEC",
      translate_df = all_schemas$isco88com_to_esec_three,
      translate_label_df = all_labels$esec,
      label = label,
      check_isco = "isco88com",
      digits = 3,
      to_factor = to_factor
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
                                 label = FALSE,
                                 to_factor = FALSE) {
  esec <- isco88com_to_esec(
    x = x,
    is_supervisor = is_supervisor,
    self_employed = self_employed,
    n_employees = n_employees,
    full_method = full_method,
    label = FALSE
  )

  labs <- c(
    "1" = "Higher Manager",
    "2" = "Higher Professional",
    "3" = "Lower Manager",
    "4" = "Lower Professional",
    "5" = "Higher-grade White-collar",
    "6" = "Self-employed and Small Employer",
    "7" = "Self-employed and Small Employer agriculture",
    "8" = "Higher-grade Blue-collar",
    "9" = "Lower-grade White-collar",
    "10" = "Lower-grade Blue-collar",
    "11" = "Routine"
  )

  # Since we're replacing esec 1 and esec 2 for 1, 2, 3 and 4, we need
  # to replace esec 3:9 to be now 5:11 such that the labels of esec_mp
  # match.
  lookup_esec <- stats::setNames(as.character(5:11), as.character(3:9))
  esec_mp <- managers_professionals_helper(
    x = x,
    esec = esec,
    is_supervisor = is_supervisor,
    n_employees = n_employees,
    self_employed = self_employed,
    lookup_labels = lookup_esec,
    schema_labels = labs,
    label = label,
    to_factor = to_factor
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
                              label = FALSE,
                              to_factor = FALSE) {



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
    check_isco = "isco88com",
    digits = 3,
    to_factor = to_factor
  )

  res
}




#' @rdname isco08_to_oesch
#' @order 2
#' @export
isco88_to_oesch <- function(x, self_employed, n_employees, n_classes = 16, label = FALSE, to_factor = FALSE) {
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
        check_isco = "isco88",
        label = label,
        to_factor = to_factor
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
      check_isco = "isco88",
      label,
      to_factor = to_factor
    )

    return(oesch)
  }
}

#' Translates 4-digit ISCO88COM to WRIGHT.
#'
#' This function translates a vector of 4-digit ISCO88COM codes to the E.O Wright class schema.
#'
#' @details The translation implemented in this function was originally developed by Erik Olin Wright. There are three possible types of translations: the "simple" version, the "decision-making" version and the "power-class" version. This translation was implemented following the SPSS script from Håkon Leiulfsrud and Heidi Jensberg. For more information, please contact the authors.
#'
#' For more information on this class schema, please check the references below:
#'
#' * Leiulfsrud, H., I. Bison &amp; H. Jensberg (2005) Social Class in Europe: European Social Survey 2002/3. Official ESS Report. NTNU Social Research in Trondheim & Department of Social Research, Trento University. [https://www.europeansocialsurvey.org/docs/methodology/ESS1_social_class.pdf](https://www.europeansocialsurvey.org/docs/methodology/ESS1_social_class.pdf)
#'
#' * Leiulfsrud, H., Bison, I. and Solheim, E. (2010), Social Class in Europe II: The European Social Survey 2002–2008, Trondheim: NTNU. [https://www.researchgate.net/publication/317624268_Social_Class_in_Europe_II](https://www.researchgate.net/publication/317624268_Social_Class_in_Europe_II)
#'
#' * Wright, E. (1978): Class, Crises and the State. London: New Left Books.
#'
#' * Wright, E. O. (1985): Classes. London: New Left Books.
#'
#' * Wright, E. O. (1997): Class Counts: Comparative studies in class analyses. Cambridge: Cambridge University Press.
#'
#' * Wright, E. O. (2005) Approaches to Class Analysis. Cambridge: Cambridge University Press.
#'
#' @param x `r rg_template_arg_x_digit("ISCO")`
#'
#' @inheritParams isco08_to_esec
#'
#' @param control_work A likert-scale type question from 0 to 10 where 0 is whether an individual has no control over their work/organisation decisions and 10 is complete control over work/organization decisions. For an example, see the variable `iorgact` in the European Social Survey.
#' @param control_daily A likert-scale type question from 1 to 4 where 1 means complete control to decide how their own daily work is/was organised and 4 means no control to decide how their own daily work is/was organised. For an example, see the variable `orgwrk` in the European Social Survey. Another example is recoding the variable `wkdcorga` from the European Social Survey such that 8-10 is 1, 5-7 is 2, 2-4 is 3 and 0-1 is 4.
#'
#' @param type The type of translation to make. Possible values are "simple", "decision-making" and "power-class".
#' @param label `r rg_template_arg_label("WRIGHT")`
#'
#' @param to_factor A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the sorted codes of WRIGHT which can be found in the source code of each function.
#'
#' @examples
#' library(dplyr)
#'
#' # E.O Wright - Simple translation
#' ess %>%
#'   transmute(
#'     isco88com,
#'     wr_simple = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "simple"
#'     ),
#'     wr_simple_label = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "simple",
#'       label = TRUE
#'     )
#'   )
#'
#' # E.O Wright - Decision-making translation
#' ess %>%
#'   transmute(
#'     isco88com,
#'     wr_decision = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "decision-making"
#'     ),
#'     wr_decision_label = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "decision-making",
#'       label = TRUE
#'     )
#'   )
#'
#' # E.O Wright - Power-class translation
#' ess %>%
#'   transmute(
#'     isco88com,
#'     wr_power = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "power-class"
#'     ),
#'     wr_power_label = isco88com_to_wright(
#'       isco88com,
#'       is_supervisor,
#'       self_employed,
#'       emplno,
#'       control_work,
#'       control_daily,
#'       type = "power-class",
#'       label = TRUE
#'     )
#'   )
#'
#' @export
isco88com_to_wright <- function(x,
                                is_supervisor,
                                self_employed,
                                n_employees,
                                control_work,
                                control_daily,
                                type,
                                label = FALSE,
                                to_factor = FALSE) {
  x <- repair_isco(x)

  # TODO: in case we want to count the exact number of digits, uncomment.
  # this was commented because we want to allow flexibility. users with
  # 3-digits should be able to translate even if they get NAs
  ## count_digits(x, digits = 4)
  check_isco(x, check_isco = "isco88com")

  construct_wright(
    x,
    is_supervisor,
    self_employed,
    n_employees,
    control_work,
    control_daily,
    type = type,
    label = label,
    to_factor = to_factor
  )
}

#' `r rg_template_title("ISCO88", "ORDC")`
#'
#' `r rg_template_intro("ISCO88", "ORDC", "isco88_to_ordc")` ORDC stands for Oslo Register Data Class.
#'
#' @details The translation implemented in this function comes from the tables found in [https://journals.sagepub.com/doi/suppl/10.1177/00031224211020012/suppl_file/sj-pdf-1-asr-10.1177_00031224211020012.pdf](https://journals.sagepub.com/doi/suppl/10.1177/00031224211020012/suppl_file/sj-pdf-1-asr-10.1177_00031224211020012.pdf). That table is the appendix of the paper "Wealth Accumulation and Opportunity Hoarding: Class-Origin Wealth Gaps over a Quarter of a Century in a Scandinavian Country" from Nordli and Toft.
#'
#' If `income` is specified, occupations in the economic upper class, the economic upper-middle class and the economic lower-middle class are grouped according to their income. The top 10 percent are assigned to the upper class, the next 40 percent to the upper-middle class, and the lowest 50 percent of earners to the lower-middle class. If income is not specified, a direct match to the ORDC class schema is performed.
#'
#' For more information on this class schema, please check the references below:
#'
#' * Hansen, M. N., & Toft, M. (2021). Wealth Accumulation and Opportunity Hoarding: Class-Origin Wealth Gaps over a Quarter of a Century in a Scandinavian Country. American Sociological Review, 86(4), 603–638. [https://doi.org/10.1177/00031224211020012](https://doi.org/10.1177/00031224211020012)
#'
#' @param x `r rg_template_arg_x("ISCO")`
#' @param income A numeric vector with the income corresponding to each respondent. See the details section for more information on how this is used.
#' @param label `r rg_template_arg_label("ORDC")`
#' @param to_factor `r rg_template_arg_factor("ORDC")`
#'
#' @return `r rg_template_return("ORDC")`
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
#'     ordc = isco88_to_ordc(isco88, label = FALSE),
#'     ordc_label = isco88_to_ordc(isco88, label = TRUE)
#'   )
#'
#' @export
isco88_to_ordc <- function(x, income = NULL, label = FALSE, to_factor = FALSE) {
  x <- common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ORDC",
    translate_df = all_schemas$isco88_to_ordc,
    translate_label_df = all_labels$ordc,
    check_isco = "isco88",
    label = label,
    to_factor = to_factor
  )

  if (!is.null(income)) {
    if (label) {
      equal_to <- "Upper-middle class: economic"
      assign_to1 <- "Lower-middle class: economic"
      assign_to2 <- "Upper class: class: economi"
    } else {
      equal_to <- 6
      assign_to1 <- 9
      assign_to2 <- 3
    }

    # Define the cut-off values
    lower <- stats::quantile(income, 0.5, na.rm = TRUE)
    upper <- stats::quantile(income, 0.9, na.rm = TRUE)

    # Adjust economic classes
    x <- dplyr::case_when(
      !is.na(income) & income <= lower & x == equal_to ~ assign_to1,
      !is.na(income) & income >= upper & x == equal_to ~ assign_to2,
      TRUE ~ x
    )

  }

  x
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
    check_isco = "isco88",
    label = FALSE
  )
}
