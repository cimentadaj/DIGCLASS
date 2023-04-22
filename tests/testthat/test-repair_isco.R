library(testthat)

# Test for numeric input
test_that("numeric input is converted to character", {
  x <- 1234
  repaired_x <- repair_isco(x)
  expect_type(repaired_x, "character")
})

# Test for ISCO variable with occupations of less than 4 digits
test_that("occupations with less than 4 digits are padded", {
  x <- c("123", "4567", "89", NA, "1234")
  repaired_x <- repair_isco(x, occ_digits = 4)
  expect_equal(repaired_x, c("0123", "4567", "0089", NA, "1234"))
})

# Test for ISCO variable with occupations of more than 4 digits
test_that("occupations with more than 4 digits raise an error", {
  x <- c("123", "45678", "910", NA, "1234")
  expect_error(repair_isco(x, occ_digits = 4))
})

# Test for ISCO variable with no missing values
test_that("ISCO variable with no missing values is returned as is", {
  x <- c("1234", "5678", "9101")
  repaired_x <- repair_isco(x, occ_digits = 4)
  expect_equal(repaired_x, x)
})
