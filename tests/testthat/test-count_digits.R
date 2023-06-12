## library(testthat)

## all_vals <- expand.grid(digits = 1:4, to = 1:4)

## for (dig in seq_len(nrow(all_vals))) {

##   # Loop through the combinations and create tests dynamically
##   to <- all_vals$to[dig]
##   digits <- all_vals$digits[dig]

##   error <- if ((to == digits)) FALSE else TRUE
##   combination <- list(
##     isco_swap = isco88_swap(ess$isco88, 4, to), digits = digits, expect_error = error
##   )

##   test_name <- glue::glue("Count digits {to} {digits}")

##   test_that(test_name, {
##     if (combination$expect_error) {
##       expect_error(count_digits(combination$isco_swap, digits = combination$digits))
##     } else {
##       expect_no_error(count_digits(combination$isco_swap, digits = combination$digits))
##     }
##   })
## }
