library(testthat)
library(glue)

# Define function that runs the tests
test_isco_swap <- function(swap_function, input) {

  # Create sample input vectors for testing
  input_vector_1 <- ess[[input]]

  # Define test cases
  test_that(glue::glue("Testing {deparse(substitute(swap_function))} function"), {

    ## expect_error(
    ##   swap_function(input_vector_1, from = 1, to = 3)
    ## )

    # Test with different from and to values
    output_vector_1 <- swap_function(input_vector_1, from = 4, to = 1)

    # Check if input and output vectors are different
    expect_false(identical(input_vector_1, output_vector_1))

    # isco68 does not have a 0000 major isco code. when converting
    # from 0100 to a 1 digit equivalent, we get NA and get different
    # result
    if (input != "isco68") {
      # Check if no NAs were introduced in the output vector
      expect_equal(sum(is.na(output_vector_1)), sum(is.na(input_vector_1)))
    }

    expect_true(all(input_vector_1 >= output_vector_1, na.rm = TRUE))

    # test with different from and to values
    output_vector_2 <- swap_function(input_vector_1, from = 4, to = 2)

    # Check if input and output vectors are different
    expect_false(identical(input_vector_1, output_vector_2))

    expect_true(all(input_vector_1 >= output_vector_2, na.rm = TRUE))

    # Here I don't check that NAs are the same between the two vectors
    # because for isco68, due to some major groups not being available (0000, 1000)
    # NAs might increase.

    # test with different from and to values
    output_vector_3 <- swap_function(input_vector_1, from = 4, to = 3)

    expect_true(all(input_vector_1 >= output_vector_3, na.rm = TRUE))

    # Check if input and output vectors are different
    expect_false(identical(input_vector_1, output_vector_3))

    # Check if no NAs were introduced in the output vector
    expect_equal(sum(is.na(output_vector_3)), sum(is.na(input_vector_1)))

    # Test with same from and to values
    output_vector_4 <- swap_function(input_vector_1, from = 4, to = 4)

    # Check if input and output vectors are the same
    expect_true(identical(input_vector_1, output_vector_4))

  })
}

# Example usage: testing isco08_swap function
test_isco_swap(isco08_swap, "isco08")
test_isco_swap(isco88_swap, "isco88")
test_isco_swap(isco68_swap, "isco68")
