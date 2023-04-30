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
    label = FALSE
  )
}

#' @rdname isco88_to_egp
#' @order 2
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


#' @rdname isco88_to_egp11
#' @order 2
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


#' @rdname isco88_to_egp11_mp
#' @order 2
#' @export
isco68_to_egp11_mp <- function(x,
                               is_supervisor,
                               self_employed,
                               n_employees,
                               label = FALSE) {
  egp <- isco68_to_egp11(
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
    label = FALSE
  )
}
