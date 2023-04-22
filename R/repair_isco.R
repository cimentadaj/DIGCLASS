#' Repair an ISCO variable
#'
#' ISCO variables need have certain properties. In particular, they must contain occupations of a single digit. This means that all ocupations need to be either 4 digits, 3 digits, 2 digits or 1 digit. The ISCO variable can't have 3 digits and 2 digits at the same time.
#'
#' When reading datasets, it might happen that the ISCO column is read as a numeric column and occupations such as `0140` are converted to `140`. What this function does is a bit of sanity check:
#'
#' * Checks if the provided ISCO variable is a character vector, and if not, warns the user that numeric ISCO variables may contain lost data.
#' * Checks that all occupations have the same number of digits and warns if not
#' * Converts all occupations with digits less than \code{occ_digits} to have the same number of digits by appending `0` from the left until all have the same number of digits.
#'
#' @param x ISCO variable to repair
#' @param occ_digits The baseline digits that the function should expect. This is whether the variable is 4/3/2/1 digits.
#'
#' @return Repaired ISCO variable
#'
#' @examples
#'
#' repair_isco(c("1234", "5678", "9012"))
#'
#' \dontrun{
#' repair_isco(c("123", "5678", "012"))
#' }
#'
#' @export
repair_isco <- function(x, occ_digits = 4) {
  if (is.numeric(x)) {
    cli::cli_alert_warning("ISCO variable is not a character. Beware that numeric ISCO variables possibly contain lost data. See https://cimentadaj.github.io/socialclasses/articles/repairing_isco_input.html for more details. Converting to a character vector.")
    x <- as.character(x)
  }

  x_clean <- x[!is.na(x)]
  x_nchar <- nchar(x_clean)
  unique_x <- unique(x_nchar)

  if (any(unique_x > occ_digits)) {
    bigger_digits <- unique_x[unique_x > occ_digits]
    bigger_occs <- unique(x_clean[nchar(x_clean) %in% bigger_digits])
    cli::cli_abort(c(
      "x" = "ISCO variable has occupations with {bigger_digits} digits. The column needs to be normalized to have occupation digits that are {occ_digits} digits or less. Check out these values:",
      "x" = paste0("x Occupation `", bigger_occs, "`")
    ))
  }

  if (any(unique_x < occ_digits)) {
    cli::cli_alert_info("ISCO variable has occupations with digits less than {occ_digits}. Converting to {occ_digits} digits.")

    new_x <- pad(x_clean, occ_digits)
    original_vals <- unique(x_clean[nchar(x_clean) < occ_digits])
    converted_vals <- pad(original_vals, occ_digits)
    converted_list <- paste0("Converted `", original_vals, "` to `", converted_vals, "`")
    cli::cli_ul(converted_list)
    x[!is.na(x)] <- new_x
  }

  x
}

pad <- function(x, digits) paste0(strrep("0", digits - nchar(x)), x)
