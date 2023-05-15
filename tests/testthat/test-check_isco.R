library(dplyr)

all_iscos <-
  list(
    "isco08" = ess$isco08,
    "isco88" = ess$isco88,
    "isco88com" = ess$isco88com,
    "isco68" = ess$isco68
  )

for (i in seq_along(all_iscos)) {
  nm <- names(all_iscos)[i]
  vec <- all_iscos[[i]]
  test_that(glue::glue("check_isco measures {nm} correctly"), {
    expect_invisible(check_isco(vec, nm))
  })
}

res <- expand.grid(input = names(all_iscos), expected = names(all_iscos), stringsAsFactors = FALSE)
res <- res %>% filter(input != expected) %>% arrange(input)

# isco88com is known to be similart to isco88
res <- res %>% filter(!(input == "isco88com" & expected == "isco88"))

for (i in seq_len(nrow(res))) {
  nm <- res$input[[i]]
  vec <- all_iscos[[nm]]
  expected <- res$expected[[i]]

  test_that(glue::glue("check_isco warns when input is {nm} but expected {expected}"), {
    expect_message(check_isco(vec, expected))
  })
}
