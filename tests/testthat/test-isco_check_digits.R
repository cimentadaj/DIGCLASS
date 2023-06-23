
# TODO: in case we want to count the exact number of digits, uncomment.
# this was commented because we want to allow flexibility. users with
# 3-digits should be able to translate even if they get NAs

## library(dplyr)

## # Isco08
## funcs <- list(
##   isco08_to_esec,
##   isco08_two_to_esec,
##   isco08_to_esec_mp,
##   isco08_to_msec
## )

## digits <- c(3, 2, 3, 3, 3)

## ## for (i in seq_along(funcs)) {
##   test_that("checks fails due to other digits isco08", {
##     expect_error(
##       ess %>%
##         mutate(
##           res = funcs[[i]](
##             isco08,
##             is_supervisor,
##             self_employed,
##             emplno,
##             label = FALSE
##           )
##         ),
##       regexp = glue::glue("! `x` might not be a {digits[[i]]}-digit ISCO vector. Convert to {digits[[i]]} digits using `isco08/88/68_swap`.")
##     )
##   })
## }

## # Isco88
## funcs <- list(
##   isco88com_to_esec,
##   isco88com_to_esec_mp,
##   isco88com_to_msec
## )

## digits <- c(3, 3, 3)

## for (i in seq_along(funcs)) {
##   test_that("checks fails due to other digits isco88", {
##     expect_error(
##       ess %>%
##         mutate(
##           res = funcs[[i]](
##             isco88,
##             is_supervisor,
##             self_employed,
##             emplno,
##             label = FALSE
##           )
##         ),
##       regexp = glue::glue("! `x` might not be a {digits[[i]]}-digit ISCO vector. Convert to {digits[[i]]} digits using `isco08/88/68_swap`.")
##     )
##   })
## }
