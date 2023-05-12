library(dplyr)

test_that("egp_mp translation works as expected", {

  # isco88
  res <-
    ess %>%
    transmute(
      isco88,
      egp_mp = isco88_to_egp_mp(
        isco88,
        is_supervisor,
        self_employed,
        emplno,
        label = FALSE
      ),
      egp_mp_label = isco88_to_egp_mp(
        isco88,
        is_supervisor,
        self_employed,
        emplno,
        label = TRUE
      )
    )

  expect_true(is.character(res$egp_mp))
  expect_true(is.character(res$egp_mp_label))
  expect_true(length(unique(res$egp_mp)) == 14)
  expect_true(length(unique(res$egp_mp_label)) == 14)

  res <-
    ess %>%
    transmute(
      isco68,
      egp_mp = isco68_to_egp_mp(
        isco68,
        is_supervisor,
        self_employed,
        emplno,
        label = FALSE
      ),
      egp_mp_label = isco68_to_egp_mp(
        isco68,
        is_supervisor,
        self_employed,
        emplno,
        label = TRUE
      )
    )

  expect_true(is.character(res$egp_mp))
  expect_true(is.character(res$egp_mp_label))
  expect_true(length(unique(res$egp_mp)) == 13)
  expect_true(length(unique(res$egp_mp_label)) == 13)
})
