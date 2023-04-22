library(testthat)

# create some sample data for testing
translate_df <- tibble::tibble(
  x = c("fooo", "barr", "bazz"),
  output_var = c(1, 2, 3)
)
translate_label_df <- tibble::tibble(
  output_var = c(1, 2, 3),
  label = c("A", "B", "C")
)

# write the unit tests
test_that("common_translator() works correctly", {
  # test with label = TRUE
  res1 <- common_translator("fooo", "x", "output_var", translate_df, translate_label_df, label = TRUE)
  expect_equal(res1, "A")

  # test with label = FALSE
  res2 <- common_translator("barr", "x", "output_var", translate_df, translate_label_df, label = FALSE)
  expect_equal(res2, 2)

  # test for error when label is requested for a schema that doesn't have labels available
  expect_error(common_translator("bazz", "x", "isei", translate_df, translate_label_df, label = TRUE))
})
