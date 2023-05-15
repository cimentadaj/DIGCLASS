#' An example data frame with data from the European Social Survey, round 6.
#'
#' Just an example data frame to show how the package can be used to translate
#' between schemas.
#'
#' The codebook for the columns is:
#'
#' * `isco68`: The ISCO68 class schema in 4-digits.
#' * `isco88`: The ISCO88 class schema in 4-digits.
#' * `isco88com`: The ISCO88COM class schema in 4-digits.
#' * `isco08`: The ISCO08 class schema in 4-digits.
#' * `emplno`: Number of subordinates, if there are any. If 0 employees, the number is 0 and not an `NA`, as it should be in your data for using `DIGCLASS`.
#'
#' * `self_employed`: A numeric vector indicating whether each individual is self-employed (1) or an employee (0).
#'
#' * `is_supervisor`: A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).
#'
#' * `control_work`: A likert-scale type question from 0 to 10 where 0 is whether an individual has no control over their work/organisation decisions and 10 is complete control over work/organization decisions. For an example, see the variable `iorgact` in the European Social Survey.
#'
#' * `control_daily`: A likert-scale type question from 1 to 4 where 1 means complete control to decide how their own daily work is/was organised and 4 means no control to decide how their own daily work is/was organised. For an example, see the variable `orgwrk` in the European Social Survey. Another example is recoding the variable `wkdcorga` from the European Social Survey such that 8-10 is 1, 5-7 is 2, 2-4 is 3 and 0-1 is 4.
#'
#'* `work_status`: This is the same as `self_employed` but has non-employed respondents coded as `2`. The unique values are `1 = self_employed`, `0 = employee` and `2 = non employed`. This variable was constructed using the variable `emplrel` and `mainact` for the unemployed.
#'
#'* `main_activity`: A numeric vector where `1 = respondent is working`, `2 = respondent is in education`, `3 = respondent is disabled `, `4 = respondent has no paid work (household work, taking care of children, etc..)` and `5 = respondent is retired`. This variable can be recoded using the `mainact` variable from the European Social Survey.
#'
#' @name ess
#' @format Data Frame
NULL
