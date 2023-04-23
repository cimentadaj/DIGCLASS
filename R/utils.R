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


utils::globalVariables(c("all_schemas", "all_labels"))
