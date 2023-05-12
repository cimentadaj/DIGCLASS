library(dplyr)

test_that("esec_mp translation works as expected", {

  ess$isco08_three <- isco08_swap(ess$isco88, from = 4, to = 3)

  res <-
    ess %>%
    transmute(
      esec_mp_label = isco08_to_esec_mp(
        isco08_three,
        is_supervisor,
        self_employed,
        emplno,
        label = TRUE
      ),
      esec_mp = isco08_to_esec_mp(
        isco08_three,
        is_supervisor,
        self_employed,
        emplno,
        label = FALSE
      )
    )

  expect_true(is.character(res$esec_mp))
  expect_true(is.character(res$esec_mp_label))
  expect_true(length(unique(res$esec_mp)) == 12)
  expect_true(length(unique(res$esec_mp_label)) == 12)

  ess$isco88com_three <- isco88_swap(ess$isco88com, from = 4, to = 3)

  # Using the full method
  res <-
    ess %>%
    transmute(
      isco88com_three,
      esec_mp = isco88com_to_esec_mp(
        isco88com_three,
        is_supervisor,
        self_employed,
        emplno,
        full_method = TRUE,
        label = FALSE
      ),
      esec_mp_label = isco88com_to_esec_mp(
        isco88com_three,
        is_supervisor,
        self_employed,
        emplno,
        full_method = TRUE,
        label = TRUE
      )
    )

  expect_true(is.character(res$esec_mp))
  expect_true(is.character(res$esec_mp_label))
  expect_true(length(unique(res$esec_mp)) == 12)
  expect_true(length(unique(res$esec_mp_label)) == 12)


  # Using the simple method
  res <-
    ess %>%
    transmute(
      isco88com_three,
      esec_mp = isco88com_to_esec_mp(
        isco88com_three,
        is_supervisor,
        self_employed,
        emplno,
        full_method = FALSE,
        label = FALSE
      ),
      esec_mp_label = isco88com_to_esec_mp(
        isco88com_three,
        is_supervisor,
        self_employed,
        emplno,
        full_method = FALSE,
        label = TRUE
      )
    )

  expect_true(is.character(res$esec_mp))
  expect_true(is.character(res$esec_mp_label))
  expect_true(length(unique(res$esec_mp)) == 12)
  expect_true(length(unique(res$esec_mp_label)) == 12)
})
