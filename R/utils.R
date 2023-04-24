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


utils::globalVariables(c("all_schemas", "all_labels"))
