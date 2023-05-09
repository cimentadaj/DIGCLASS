library(dplyr)

all_vals <- expand.grid(digits = c(16, 8, 5), label = c(TRUE, FALSE))
funs <- list(isco08_to_oesch, isco88_to_oesch)
names <- c("isco08", "isco88")

for (f in seq_along(funs)) {
  for (i in seq_len(nrow(all_vals))) {
    digits <- all_vals$digits[i]
    label <- all_vals$label[i]
    fun <- funs[[f]]
    name <- names[[f]]

    test_that(glue::glue("{name}_to_oesch transforms to {digits} with label {label}"), {
      # isco08
      res <- ess %>%
        transmute(
          oesch = fun(!!ensym(name), self_employed, emplno, n_classes = digits, label = label),
        ) %>%
        distinct(oesch)

      res <- res[!is.na(res)]

      expect_equal(length(res), digits)
    })
  }
}

all_vals <- expand.grid(digits = c(11, 7, 5, 3), label = c(TRUE, FALSE))
funs <- list(isco88_to_egp, isco68_to_egp)
names <- c("isco88", "isco68")

for (f in seq_along(funs)) {
  for (i in seq_len(nrow(all_vals))) {
    digits <- all_vals$digits[i]
    label <- all_vals$label[i]
    fun <- funs[[f]]
    name <- names[[f]]

    # isco68 - 11 digits doesn't have enough values in ESS to have all
    # 11 labels, instead it has 10 so I jump that test.
    if (name != "isco68" & digits != 11) {
      test_that(glue::glue("{name}_to_egp transforms to {digits} with label {label}"), {
        # isco08
        res <- ess %>%
          transmute(
            egp = fun(!!ensym(name), self_employed, emplno, n_classes = digits, label = label),
          ) %>%
          distinct(egp)

        res <- res[!is.na(res)]

        expect_equal(length(res), digits)
      })
    }
  }
}
