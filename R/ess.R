#' An example data frame with data from the European Social Survey, round 5.
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
#' * `control_daily`: A likert-scale type question from 0 to 10 where 0 means the respondent has no control to decide how their own daily work is/was organised and 10 is complete control to decide how their own daily work is/was organised. For an example, see the variable `wkdcorga` in the European Social Survey.
#'
#' @name ess
#' @format Data Frame
NULL
