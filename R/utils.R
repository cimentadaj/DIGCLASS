common_translator <- function(x, input_var, output_var, translate_df, translate_label_df, label, digits = 4) {
  x <- repair_isco(x, digits = digits)

  res <-
    tibble::tibble(x = as.character(x)) %>%
    dplyr::left_join(translate_df, by = c("x" = input_var))

  if (label) {
    no_labs <- c("isei", "siops", "mps88", "iseisps")
    if (tolower(output_var) %in% no_labs) {
      stop(
        "Labels not available for these schemas: ",
        paste(no_labs, collapse = ", "), ". Set `label` to `FALSE` to translate this schema without labels."
      )
    }

    res <-
      tibble::tibble(x_label = res[[output_var]]) %>%
      dplyr::left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
  } else {
    transformed <- res[[output_var]]
  }

  transformed
}

multiple_cols_translator <- function(x, col_position, output_var, translate_df, translate_label_df, label, digits = 4) {
  x <- repair_isco(x, digits = digits)

  class_match <- match(x, translate_df[[1]])
  matrix_translate_df <- as.matrix(translate_df)
  transformed <- matrix_translate_df[cbind(class_match, col_position)]

  if (label) {
    res <-
      tibble::tibble(x_label = transformed) %>%
      dplyr::left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
  } else {
    transformed <- transformed
  }

  transformed
}

managers_professionals_helper <- function(x, esec, is_supervisor, n_employees, self_employed, label) {
  # TODO: Since this function does not have an excel, I've coded the "rules" manually
  # but ideally we want to move all of this into social_classes.txt such that
  # all schemas are contained in a single file.
  one_digit_isco <- as.numeric(substr(x, 1, 1))
  has_subordinates <- (is_supervisor | n_employees > 0)
  esec_first <- esec == 1
  esec_second <- esec == 2
  isco_first <- one_digit_isco == 1
  isco_zero <- one_digit_isco == 0
  large_employees <- self_employed == 1 & n_employees >= 10
  small_employees <- self_employed == 1 & dplyr::between(n_employees, 1, 9)
  self_employed_no_employees <- self_employed == 1 & n_employees < 1
  supervisor <- self_employed == 0 & n_employees > 0
  employee <- self_employed == 0 & n_employees == 0

  higher_managers_1 <- esec_first & (isco_zero | isco_first | has_subordinates)
  higher_managers_2 <- esec_first & (large_employees | small_employees)
  higher_professional <- esec_first & one_digit_isco > 1 & (self_employed_no_employees | employee)
  lower_managers_1 <- esec_second & (isco_zero | isco_first | has_subordinates)
  lower_managers_2 <- esec_second & (large_employees | small_employees)
  lower_professional <- esec_second & one_digit_isco > 1 & (self_employed_no_employees | employee)

  mp <- dplyr::case_when(
    higher_managers_1 | higher_managers_2 ~ "1",
    higher_professional ~ "2",
    lower_managers_1 | lower_managers_2 ~ "3",
    lower_professional ~ "4",
    TRUE ~ esec
  )

  if (label) {
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

    mp <- labs[mp]
  }

  mp
}

rg_template_title <- function(from, to, digit = 4) {
  glue::glue("Translate {digit}-digit {from} to {to}")
}

rg_template_intro <- function(from, to, translate_df, digit = 4) {
  glue::glue("This function translates a vector of {digit}-digit {from} codes to {to} codes using the translation table stored in the `all_schema${translate_df}` data frame.")
}

rg_template_details_iscogen <- function(from, to) {
  glue::glue("This translation was taken from the `iscogen` Stata package. For more details, check out the package documentation and search for `{from} -> {to}`.")
}

slot_digits <- function(digit = 3) {
  chr_dig <- as.character(digit)
  x <- list("4" = c(131, 1310, 10), "3" = c(131, 1310, 10), "2" = c(13, 1300, 100), "1" = c(1, 1000, 1000))
  chosen_x <- x[[chr_dig]]

  chosen_x
}

rg_template_digits_warning <- function(digit = 3) {

  chosen_x <- slot_digits(digit = digit)

  # IMPORTANTE: Do not change how some {digit} have {digit}- and other are just {digit} followed by a space
  # That's because in `isco_swap` I replace `{digit}-` for another digit in the docs.
  glue::glue("This function will accept {digit} digit codes as 4 digits. This means that if the {digit}-digit code is {chosen_x[1]} then it should be {chosen_x[2]}. All codes should be 4 digits, even though the code is represented as {digit}-digits ({chosen_x[2]}, {chosen_x[2] + chosen_x[3]}, etc..)")
}

rg_template_arg_x <- function(from, digit = 4) {
  glue::glue("A character vector of {digit}-digit {from} codes.")
}

rg_template_arg_x_digit <- function(from, digit = 4) {

  chosen_x <- slot_digits(digit = digit)
  x <- rg_template_arg_x(from, digit = digit)
  glue::glue("{x} Even though these should be {digit}-digit, instead of {chosen_x[1]}, the code should be {chosen_x[2]}, which is the {digit}-digit version of ISCO.")
}


rg_template_arg_label <- function(to) {
  glue::glue("A logical value indicating whether to return the labels of the translated {to} codes (default is \\code{{FALSE}}).")
}

rg_template_arg_supervisor <- function() {
  glue::glue("A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).")
}


rg_template_arg_selfemployed <- function() {
  glue::glue("A numeric vector indicating whether each individual is self-employed (1) or not (0).")
}

rg_template_arg_nemployees <- function() {
  glue::glue("A numeric vector indicating the number of employees for each individual.")
}


rg_template_return <- function(to) {
  glue::glue("A character vector of {to} codes.")

}

utils::globalVariables(c("all_schemas", "all_labels"))
