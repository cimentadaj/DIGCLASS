#' Fill ISCO codes with trailing zeros
#'
#' This function converts ISCO codes of any digit level to a specified target 
#' digit format by appending trailing zeros. This is useful for preparing codes 
#' for functions that expect specific digit lengths, such as `isco*_to_oep()` 
#' which expects 4-digit input.
#'
#' @details 
#' The function automatically detects the digit level of input codes and fills 
#' them to the target format by appending trailing zeros:
#' * 1-digit codes (e.g., "6") become "6000" (for digits=4)
#' * 2-digit codes (e.g., "61") become "6100" (for digits=4)
#' * 3-digit codes (e.g., "611") become "6110" (for digits=4)
#' * Codes already at target length remain unchanged
#'
#' This follows the same logic as CROSSWALK/ISCOGEN in Stata. Note that this 
#' function only expands codes (adds zeros) - to reduce digit levels, use 
#' `isco*_swap()` instead.
#'
#' @param x A character or numeric vector of ISCO codes
#' @param digits Target number of digits (1, 2, 3, or 4). Default is 4.
#' @param isco_type Optional. Specify "isco08", "isco88", or "isco68" for validation
#'
#' @return A character vector of ISCO codes at the specified digit level
#'
#' @examples
#' # Fill 1-digit codes to 4-digit (default)
#' isco_fill(c("1", "2", "6"))  # Returns: c("1000", "2000", "6000")
#'
#' # Fill 2-digit codes to 4-digit  
#' isco_fill(c("11", "21", "61"))  # Returns: c("1100", "2100", "6100")
#'
#' # Fill 1-digit codes to 3-digit
#' isco_fill(c("1", "2", "6"), digits = 3)  # Returns: c("100", "200", "600")
#'
#' # Fill 2-digit codes to 3-digit
#' isco_fill(c("11", "21", "61"), digits = 3)  # Returns: c("110", "210", "610")
#'
#' # 4-digit codes unchanged when digits=4
#' isco_fill(c("1111", "2111", "6111"))  # Returns: unchanged
#'
#' # Typical workflow for OEP calculation
#' \dontrun{
#' library(dplyr)
#' your_data %>%
#'   mutate(
#'     isco08_4digit = isco_fill(isco08_2digit),  # Fill to 4-digit
#'     oep = isco08_to_oep(isco08_4digit)
#'   )
#' }
#'
#' @seealso [repair_isco()] for fixing malformed ISCO codes, [isco08_swap()] for converting between digit levels
#' @export
isco_fill <- function(x, digits = 4, isco_type = NULL) {
  # Input validation
  if (!digits %in% 1:4) {
    cli::cli_abort("`digits` must be 1, 2, 3, or 4")
  }
  
  if (is.numeric(x)) {
    cli::cli_alert_warning("Converting numeric ISCO to character...")
    x <- as.character(x)
  }
  
  # Handle edge cases: empty or all-NA input
  if (length(x) == 0) {
    return(x)
  }
  
  x_clean <- x[!is.na(x)]
  if (length(x_clean) == 0) {
    return(x)  # Handle all-NA case
  }
  
  # Detect current digit level automatically
  digit_lengths <- nchar(x_clean)
  
  # Validate consistent digit length
  if (length(unique(digit_lengths)) > 1) {
    cli::cli_abort("Mixed digit lengths detected. Please ensure all codes have same digit level.")
  }
  
  current_digits <- unique(digit_lengths)[1]
  
  # Check if already correct length
  if (current_digits == digits) {
    return(x)
  }
  
  # Check if trying to compress (not expand)
  if (current_digits > digits) {
    cli::cli_abort("Cannot reduce {current_digits}-digit codes to {digits}-digit. Use isco*_swap() instead.")
  }
  
  # Validate against ISCO type if provided
  if (!is.null(isco_type)) {
    check_isco(x, isco_type)
  }
  
  # Fill with zeros to reach target digits
  zeros_needed <- digits - current_digits
  x[!is.na(x)] <- paste0(x[!is.na(x)], strrep("0", zeros_needed))
  
  cli::cli_alert_info("Filled {current_digits}-digit ISCO codes to {digits}-digit format")
  
  x
}