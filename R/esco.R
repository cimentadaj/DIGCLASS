#' `r rg_template_title("ESCO", "ISCO08")`
#'
#' `r rg_template_intro("ESCO", "ISCO08", c("esco_to_isco08"))`
#'
#' This translation converts the ESCO classification of the European Commission to ISCO08. This translations uses ESCO v1.1.1 and was downloaded from the dataset [https://esco.ec.europa.eu/en/use-esco/download](https://esco.ec.europa.eu/en/use-esco/download).
#'
#' For more information on this class schema, please check the references below:
#'
#' * The ESCO Classification: [https://esco.ec.europa.eu/en/classification](https://esco.ec.europa.eu/en/classification)
#'
#' @param x `r rg_template_arg_x("ESCO")`
#' @param label `r rg_template_arg_label("ISCO08")`
#' @param to_factor `r rg_template_arg_factor("ISCO08")`
#'
#' @return `r rg_template_return("ISCO08")`
#'
#' @order 1
#'
#' @examples
#'
#' esco_to_isco08(c("11401", "1101", "11301"), label = FALSE)
#'
#' esco_to_isco08(c("11401", "1101", "11301"), label = TRUE)
#'
#' esco_to_isco08(c("11401", "1101", "11301"), label = TRUE, to_factor = TRUE)
#'
#' @export
esco_to_isco08 <- function(x, label = FALSE, to_factor = FALSE) {
  common_translator(
    x,
    input_var = "ESCO",
    output_var = "ISCO08",
    translate_df = all_schemas$esco_to_isco08,
    translate_label_df = all_labels$isco08,
    check_isco = NULL,
    label = label,
    to_factor = to_factor,
    repair_isco = FALSE,
    digits = NULL,
    is_isco = FALSE
  )
}
