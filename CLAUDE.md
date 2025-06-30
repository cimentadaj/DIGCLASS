# DIGCLASS Development Guide

This document provides essential guidelines for developers working on the DIGCLASS package. It covers codebase structure, conventions, and best practices to help you contribute effectively.

## 📁 Codebase Overview

### Core Architecture
- **Translation focus**: The package translates between ISCO (International Standard Classification of Occupations) schemas and various occupational class schemas
- **Data-driven**: Most translations are stored in `all_schemas` and `all_labels` data objects
- **Utility-first**: Common operations are abstracted into reusable utility functions

### Key Files & Directories
```
R/
├── isco08.R          # ISCO08 translation functions
├── isco88.R          # ISCO88 translation functions  
├── isco68.R          # ISCO68 translation functions
├── repair_isco.R     # ISCO data validation/repair
├── isco_fill.R       # ISCO zero-padding utilities
├── utils.R           # Core utility functions
├── all_schemas.R     # Translation data documentation
└── all_labels.R      # Label data documentation

tests/testthat/       # Comprehensive test suite
vignettes/            # User documentation and examples
_pkgdown.yml          # Website configuration
```

## 🎯 Coding Conventions

### Function Naming
- **Translation functions**: `isco{XX}_to_{schema}()` (e.g., `isco08_to_esec()`)
- **Utility functions**: `isco{XX}_{action}()` (e.g., `isco08_swap()`, `isco_fill()`)
- **Internal helpers**: descriptive names (e.g., `check_isco()`, `repair_isco()`)

### Documentation Standards
- **Extensive roxygen2**: Use template functions from `utils.R` for consistency
- **Real examples**: Include practical examples with actual ISCO codes
- **Cross-references**: Link related functions with `@seealso`
- **Template usage**: Follow patterns like `rg_template_title()`, `rg_template_intro()`

### Parameter Conventions
- `x`: Input ISCO vector (always first parameter)
- `label`: Logical for returning labels vs codes (default `FALSE`)
- `to_factor`: Logical for factor output (default `FALSE`)
- `isco_type`: Optional ISCO validation ("isco08", "isco88", "isco68")
- `digits`: Number of digits for ISCO operations (default `4`)

## 🔧 Core Utility Functions

### Essential Functions to Know
```r
# Input validation and repair
repair_isco(x, digits = 4)          # Fix malformed ISCO codes
check_isco(x, check_isco)           # Validate ISCO codes
count_digits(x, digits)             # Count digit patterns

# Translation engines  
common_translator(...)              # Single-column translations
multiple_cols_translator(...)       # Multi-column translations (e.g., ESEC)

# Data transformation
isco_fill(x, digits = 4)           # Pad with trailing zeros
isco08_swap(x, from, to)           # Convert between digit levels
```

### Translation Patterns
1. **Simple translations**: Use `common_translator()` - direct code-to-code mapping
2. **Complex translations**: Use `multiple_cols_translator()` - requires additional variables (employment status, etc.)
3. **Always validate**: Call `repair_isco()` and `check_isco()` on inputs

## 🧪 Testing Strategy

### Test Organization
- **One test file per R file**: `test-{filename}.R`
- **Comprehensive coverage**: Test edge cases, errors, and realistic scenarios
- **Follow patterns**: Use existing test files as templates (especially `test-isco_swap.R`)

### Key Test Cases
```r
# Standard test patterns
test_that("function_name works correctly", {
  # Basic functionality
  expect_equal(func(input), expected_output)
  
  # Edge cases
  expect_equal(func(c("1", NA, "2")), c("1000", NA, "2000"))
  expect_equal(func(character(0)), character(0))
  
  # Error handling
  expect_error(func(invalid_input), "expected error message")
  
  # Messages/warnings
  expect_message(func(input), "expected message")
})
```

## 📚 Adding New Functions

### Step-by-Step Process
1. **Choose appropriate R file** or create new one following naming conventions
2. **Write function** following parameter conventions and validation patterns
3. **Add comprehensive documentation** using roxygen2 templates
4. **Create thorough tests** covering all functionality and edge cases
5. **Update vignettes** if function adds new workflow capabilities
6. **Update `_pkgdown.yml`** to include function in appropriate section
7. **Run quality checks**: `devtools::check()` must pass with 0 errors/warnings

### Function Template
```r
#' Brief function description
#'
#' Detailed description with context and use cases.
#'
#' @param x A character vector of ISCO codes
#' @param other_param Description of parameter
#'
#' @return Description of return value
#'
#' @examples
#' # Simple example
#' function_name(c("1111", "2222"))
#'
#' @seealso [related_function()] for related functionality
#' @export
function_name <- function(x, other_param = default) {
  # Input validation
  if (condition) cli::cli_abort("Error message")
  
  # Core logic using utility functions
  result <- common_translator(x, ...)
  
  result
}
```

## 📖 Documentation & Vignettes

### Vignette Guidelines
- **translating_between_hierarchies.Rmd**: ISCO digit conversion and utility functions
- **repairing_isco_input.Rmd**: Data cleaning and validation
- **case-study-*.Rmd**: Real-world application examples

### pkgdown Configuration
- **Add new functions** to appropriate section in `_pkgdown.yml`
- **Group by purpose**: Translation functions, utility functions, etc.
- **Test locally**: Run `pkgdown::build_site()` before pushing

## ⚡ Performance Considerations

- **Use vectorized operations**: Avoid loops where possible
- **Leverage existing utilities**: Don't reinvent validation/translation logic
- **Cache lookups**: Use `all_schemas` and `all_labels` efficiently
- **Test with large datasets**: Ensure functions scale appropriately

## 🚨 Common Pitfalls

1. **Mixed digit lengths**: Always validate consistent ISCO digit levels
2. **Missing validation**: Use `repair_isco()` and `check_isco()` consistently  
3. **Inconsistent documentation**: Follow existing roxygen2 patterns exactly
4. **Missing pkgdown entries**: Add new functions to `_pkgdown.yml`
5. **Insufficient testing**: Test edge cases, NAs, empty inputs, and error conditions

## 🛠 Development Commands

```bash
# Essential development workflow
devtools::load_all()        # Load package for testing
devtools::test()           # Run test suite
devtools::check()          # Full package validation
devtools::document()       # Update documentation
pkgdown::build_site()      # Build documentation website

# Quality assurance
lintr::lint_package()      # Code style checking
goodpractice::gp()         # Best practices check
```

## 🎯 Package Philosophy

- **User-friendly**: Functions should "just work" with minimal setup
- **Stata compatibility**: Mirror CROSSWALK/ISCOGEN behavior where applicable
- **Comprehensive**: Cover edge cases and provide helpful error messages
- **Consistent**: Follow established patterns for predictable API
- **Well-tested**: Extensive test coverage ensures reliability

## 📞 Getting Help

- **Existing code**: Use similar functions as templates
- **Test files**: Show comprehensive usage patterns
- **Vignettes**: Demonstrate real-world workflows
- **Issue tracker**: For bugs and feature requests

Remember: When in doubt, follow existing patterns in the codebase. Consistency is key to maintaining a high-quality package!