## code to prepare `ESS` dataset goes here
library(readr)
library(dplyr)
library(DIGCLASS)

ess <-
  # This is just the fith round of the ESS
  read_csv("~/Downloads/ESS4e04_5.csv", col_types = list(.default = col_character())) %>%
  mutate(self_employed = ifelse(emplrel == "2", 1, 0)) %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(emplno = as.numeric(emplno)) %>%
  rename(isco88 = iscoco)

ess$isco88[nchar(ess$isco88) > 4] <- NA

ess <-
  ess %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    isco68 = isco88_to_isco68(isco88),
    isco08 = isco68_to_isco08(isco68)
  ) %>%
  relocate(isco68, isco88, isco08, everything())

usethis::use_data(ess, overwrite = TRUE)
