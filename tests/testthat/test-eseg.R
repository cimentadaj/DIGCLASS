library(dplyr)

test_that("tests eseg works corrects", {
  ess$isco08_two <- isco08_swap(ess$isco08, from = 4, to = 2)

  res <-
    ess %>%
    mutate(
      eseg = isco08_to_eseg(
        isco08_two,
        work_status,
        main_activity,
        agea,
        type = "two-digit"
      ),
      eseg_label = isco08_to_eseg(
        isco08_two,
        work_status,
        main_activity,
        agea,
        type = "two-digit",
        label = TRUE
      )
    )

  expect_true(is.character(res$eseg))
  expect_true(is.character(res$eseg_label))
  expect_true(length(unique(res$eseg)) == 33)
  expect_true(length(unique(res$eseg_label)) == 33)

  res <-
    ess %>%
    mutate(
      eseg = isco08_to_eseg(
        isco08_two,
        work_status,
        main_activity,
        agea,
        type = "one-digit"
      ),
      eseg_label = isco08_to_eseg(
        isco08_two,
        work_status,
        main_activity,
        agea,
        type = "one-digit",
        label = TRUE
      )
    )

  expect_true(is.character(res$eseg))
  expect_true(is.character(res$eseg_label))
  expect_true(length(unique(res$eseg)) == 9)
  expect_true(length(unique(res$eseg_label)) == 9)

  expect_error(
    mutate(
      ess,
      eseg = isco08_to_eseg(
        isco08,
        work_status,
        main_activity,
        agea,
        type = "one-digit"
      )
    )
  )

})
