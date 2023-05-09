## code to prepare `ESS` dataset goes here
library(haven)
library(dplyr)
library(DIGCLASS)

issp <-
  # This is just the 2020 data for the ISSP 2020 Environment IV
  read_dta("~/Downloads/class_schema/ZA7650_v1-0-0.dta") %>%
  select(ISCO08, EMPREL, NSUP, WRKSUP) %>%
  rename_all(tolower)

usethis::use_data(issp, overwrite = TRUE)
