#' Repair an ISCO variable
#'
#'
#' ISCO variables need have certain properties. In particular, they must contain occupations of a single digit. This means that all ocupations need to be either 4 digits, 3 digits, 2 digits or 1 digit. The ISCO variable can't have 3 digits and 2 digits at the same time.
#'
#' When reading datasets, it might happen that the ISCO column is read as a numeric column and occupations such as `0140` are converted to `140`. It is impossible for `DIGCLASS` to determine whether this class is actually `0140` or `1400`. what this function does is a bit of sanity check:
#'
#'
#' * Checks if the provided ISCO variable is a character vector, and if not, warns the user that numeric ISCO variables may contain lost data.
#' * Checks that all occupations have the same number of digits, and if not, raises an error.
#'
#'
#'
#' @param x ISCO variable to repair
#' @return Repaired ISCO variable
#' @examples
#'
#' repair_isco(c("1234", "5678", "9012"))
#'
#' \dontrun{
#' repair_isco(c("123", "5678", "012"))
#' }
#'
#' @export
repair_isco <- function(x) {
  if (is.numeric(x)) {
    cli::cli_alert_warning("ISCO variable is not a character. Beware that numeric ISCO variables possibly contain lost data. See https://cimentadaj.github.io/socialclasses/articles/repairing_isco_input.html for more details. Converting to a character vector.")
    x <- as.character(x)
  }

  x_nchar <- nchar(x)
  x_nchar <- x_nchar[!is.na(x)]
  unique_x <- unique(x_nchar)

  if (length(unique_x) != 1) {
    cli::cli_abort("ISCO variable has occupations with {unique_x} digits. The column needs to be normalized to have only a single digit.")
    return(x)
  }

  cli::cli_alert_success("ISCO variable ready for analysis")
  x
}
