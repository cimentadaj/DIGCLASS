library(testthat)
library(DIGCLASS)

test_that("isco_fill works correctly with default digits=4", {
  # Test 1-digit filling to 4-digit
  expect_equal(isco_fill(c("1", "2", "6")), c("1000", "2000", "6000"))
  
  # Test 2-digit filling to 4-digit
  expect_equal(isco_fill(c("11", "21", "61")), c("1100", "2100", "6100"))
  
  # Test 3-digit filling to 4-digit
  expect_equal(isco_fill(c("111", "211", "611")), c("1110", "2110", "6110"))
  
  # Test 4-digit unchanged
  input_4d <- c("1111", "2111", "6111")
  expect_equal(isco_fill(input_4d), input_4d)
})

test_that("isco_fill works with custom digits parameter", {
  # Test 1-digit to 3-digit
  expect_equal(isco_fill(c("1", "2", "6"), digits = 3), c("100", "200", "600"))
  
  # Test 2-digit to 3-digit
  expect_equal(isco_fill(c("11", "21", "61"), digits = 3), c("110", "210", "610"))
  
  # Test 1-digit to 2-digit
  expect_equal(isco_fill(c("1", "2", "6"), digits = 2), c("10", "20", "60"))
  
  # Test 3-digit unchanged when digits=3
  input_3d <- c("111", "211", "611")
  expect_equal(isco_fill(input_3d, digits = 3), input_3d)
  
  # Test 2-digit unchanged when digits=2
  input_2d <- c("11", "21", "61")
  expect_equal(isco_fill(input_2d, digits = 2), input_2d)
  
  # Test 1-digit unchanged when digits=1
  input_1d <- c("1", "2", "6")
  expect_equal(isco_fill(input_1d, digits = 1), input_1d)
})

test_that("isco_fill handles edge cases", {
  # Test numeric input
  expect_equal(isco_fill(c(1, 2, 6)), c("1000", "2000", "6000"))
  
  # Test NA handling
  expect_equal(isco_fill(c("1", NA, "6")), c("1000", NA, "6000"))
  
  # Test all-NA input
  expect_equal(isco_fill(c(NA, NA)), c(NA, NA))
  
  # Test empty input
  expect_equal(isco_fill(character(0)), character(0))
  
  # Test single element
  expect_equal(isco_fill("1"), "1000")
  expect_equal(isco_fill("11"), "1100")
  expect_equal(isco_fill("111"), "1110")
  expect_equal(isco_fill("1111"), "1111")
})

test_that("isco_fill error handling works correctly", {
  # Test mixed digit lengths error
  expect_error(isco_fill(c("1", "11", "111")), "Mixed digit lengths")
  
  # Test invalid digits parameter
  expect_error(isco_fill(c("1", "2"), digits = 5), "`digits` must be 1, 2, 3, or 4")
  expect_error(isco_fill(c("1", "2"), digits = 0), "`digits` must be 1, 2, 3, or 4")
  expect_error(isco_fill(c("1", "2"), digits = -1), "`digits` must be 1, 2, 3, or 4")
  
  # Test trying to compress codes
  expect_error(isco_fill(c("1111", "2111"), digits = 3), "Cannot reduce.*Use isco.*_swap")
  expect_error(isco_fill(c("111", "211"), digits = 2), "Cannot reduce.*Use isco.*_swap")
  expect_error(isco_fill(c("11", "21"), digits = 1), "Cannot reduce.*Use isco.*_swap")
})

test_that("isco_fill works with isco_type validation", {
  # Test with valid isco_type - should work without error
  expect_no_error(isco_fill(c("11", "21"), isco_type = "isco08"))
  
  # Test with potentially invalid codes for given isco_type 
  # (check_isco will show message but not error)
  expect_message(isco_fill(c("99", "98"), isco_type = "isco08"))
})

test_that("isco_fill provides informative messages", {
  # Test that informative message is shown when filling
  expect_message(isco_fill(c("1", "2")), "Filled 1-digit ISCO codes to 4-digit format")
  expect_message(isco_fill(c("11", "21")), "Filled 2-digit ISCO codes to 4-digit format")
  expect_message(isco_fill(c("111", "211")), "Filled 3-digit ISCO codes to 4-digit format")
  
  # Test custom digits messages
  expect_message(isco_fill(c("1", "2"), digits = 3), "Filled 1-digit ISCO codes to 3-digit format")
  expect_message(isco_fill(c("11", "21"), digits = 3), "Filled 2-digit ISCO codes to 3-digit format")
  
  # Test that no message is shown when no change needed
  expect_no_message(isco_fill(c("1111", "2111")))
})

test_that("isco_fill handles special ISCO codes correctly", {
  # Test with codes that start with 0 (common edge case)
  expect_equal(isco_fill(c("01", "02")), c("0100", "0200"))
  expect_equal(isco_fill(c("001", "002")), c("0010", "0020"))
  
  # Test military codes (0000 series)
  expect_equal(isco_fill(c("0", "1")), c("0000", "1000"))
})

test_that("isco_fill works with realistic ISCO examples", {
  # Test with realistic ISCO08 examples
  isco_1d <- c("1", "2", "3", "6")  # Managers, Professionals, Technicians, Agriculture
  expected_4d <- c("1000", "2000", "3000", "6000")
  expect_equal(isco_fill(isco_1d), expected_4d)
  
  isco_2d <- c("11", "21", "31", "61")  # Chief executives, Science professionals, etc.
  expected_4d_from_2d <- c("1100", "2100", "3100", "6100")
  expect_equal(isco_fill(isco_2d), expected_4d_from_2d)
  
  # Test with leading zeros
  isco_with_zeros <- c("01", "02", "03")
  expected_with_zeros <- c("0100", "0200", "0300")
  expect_equal(isco_fill(isco_with_zeros), expected_with_zeros)
})