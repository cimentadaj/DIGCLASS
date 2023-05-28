# create some sample data for testing
translate_df <- tibble::tibble(
  col1 = c("quxx", "barr", "bazz"),
  col2 = c(1, 2, 3),
  col3 = c(4, 5, 6)
)
translate_label_df <- tibble::tibble(
  output_var = c(1, 2, 3),
  label = c("A", "B", "C")
)

# write the unit tests
test_that("multiple_cols_translator() works correctly", {
  ## # test with label = TRUE
  ## res1 <- multiple_cols_translator("qux", 1, "col2", translate_df, translate_label_df, label = TRUE)
  ## expect_equal(res1, "A")

  # test with label = FALSE
  res2 <- multiple_cols_translator("barr", 2, "col3", translate_df, translate_label_df, label = FALSE)
  expect_equal(res2, "2")

  res3 <- multiple_cols_translator("barr", 2, "col3", translate_df, translate_label_df, label = FALSE, to_factor = TRUE)
  expect_s3_class(res3, c("factor", "ordered"))

  # test for error when trying to translate a value that's not in the translation table
  expect_error(multiple_cols_translator("fooo", 1, "col2", translate_df, translate_label_df, label = TRUE))

  # test for error when trying to translate a value for a column that doesn't exist
  expect_error(multiple_cols_translator("quxx", 4, "col4", translate_df, translate_label_df, label = TRUE))

  # test for error when trying to translate a value with a label for a schema where labels are not available
  expect_error(multiple_cols_translator("quxx", 1, "isei", translate_df, translate_label_df, label = TRUE))
})
