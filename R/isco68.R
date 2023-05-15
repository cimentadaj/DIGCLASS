#' @rdname isco08_to_isco88
#' @order 2
#' @export
isco68_to_isco88 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO88",
    translate_df = all_schemas$isco68_to_isco88,
    translate_label_df = all_labels$isco88,
    check_isco = "isco68",
    label = label
  )
}

#' @rdname isco88_to_isco08
#' @order 2
#' @export
isco68_to_isco08 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO08",
    translate_df = all_schemas$isco68_to_isco08,
    translate_label_df = all_labels$isco08,
    check_isco = "isco68",
    label = label
  )
}

#' @rdname isco08_to_isei
#' @order 3
#' @export
isco68_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISEI",
    translate_df = all_schemas$isco68_to_isei,
    translate_label_df = NULL,
    check_isco = "isco68",
    label = FALSE
  )
}

#' @rdname isco08_to_siops
#' @order 3
#' @export
isco68_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "SIOPS",
    translate_df = all_schemas$isco68_to_siops,
    translate_label_df = NULL,
    check_isco = "isco68",
    label = FALSE
  )
}

#' @rdname isco88_to_egp
#' @order 2
#' @export
isco68_to_egp <- function(x, self_employed, n_employees, n_classes = 11, label = FALSE) {
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

  schema <- all_schemas$isco68_to_egp11
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
        check_isco = "isco68",
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
      label,
      check_isco = "isco68"
    )

    return(egp)
  }
}

#' @rdname isco88_to_egp_mp
#' @order 2
#' @export
isco68_to_egp_mp <- function(x,
                             is_supervisor,
                             self_employed,
                             n_employees,
                             label = FALSE) {
  egp <- isco68_to_egp(
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
    label = label
  )

  egp_mp
}



#' @rdname isco08_swap
#' @order 3
#' @export
isco68_swap <- function(x,
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
    translate_df = all_schemas$isco68_hierarchy,
    translate_label_df = NULL,
    check_isco = "isco68",
    label = FALSE
  )
}
