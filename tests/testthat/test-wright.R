test_that("e.o wright translation works as expected", {
  # E.O Wright - Simple translation
  res <-
    ess %>%
    transmute(
      isco88com,
      wr_simple = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "simple"
      ),
      wr_simple_label = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "simple",
        label = TRUE
      )
    )

  expect_true(length(unique(res$wr_simple)) == 9)
  expect_true(length(unique(res$wr_simple_label)) == 9)

  # E.O Wright - Decision-making translation
  res <-
    ess %>%
    transmute(
      isco88com,
      wr_decision = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "decision-making"
      ),
      wr_decision_label = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "decision-making",
        label = TRUE
      )
    )

  expect_true(length(unique(res$wr_decision)) == 10)
  expect_true(length(unique(res$wr_decision_label)) == 10)

  # E.O Wright - Power-class translation
  res <- ess %>%
    transmute(
      isco88com,
      wr_power = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "power-class"
      ),
      wr_power_label = isco88com_to_wright(
        isco88com,
        is_supervisor,
        self_employed,
        emplno,
        control_work,
        control_daily,
        type = "power-class",
        label = TRUE
      )
    )

  expect_true(length(unique(res$wr_power)) == 10)
  expect_true(length(unique(res$wr_power_label)) == 10)
})
