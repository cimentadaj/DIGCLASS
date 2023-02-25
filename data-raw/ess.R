## code to prepare `ESS` dataset goes here
library(readr)
library(dplyr)
library(DIGCLASS)

ess <-
  # This is just the fith round of the ESS
  read_csv("~/Downloads/ESS4e04_5.csv") %>%
  mutate(self_employed = ifelse(emplrel == 2, 1, 0)) %>%
  select(iscoco, emplno, self_employed) %>%
  rename(isco88 = iscoco) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    isco68 = isco88_to_isco68(isco88),
    isco88 = if_else(isco88 >= 66666, NA, isco88),
    isco08 = isco68_to_isco08(isco68)
  ) %>%
  relocate(isco68, isco88, isco08, everything())

usethis::use_data(ess, overwrite = TRUE)
