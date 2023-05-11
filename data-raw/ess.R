## code to prepare `ESS` dataset goes here
library(readr)
library(dplyr)
library(DIGCLASS)

ess <-
  # This is just the fith round of the ESS
  read_csv("~/Downloads/ESS4e04_5.csv", col_types = list(.default = col_character())) %>%
  mutate(
    self_employed = ifelse(emplrel == "2", 1, 0),
    is_supervisor = ifelse(jbspv == "1", 1, 0)
  ) %>%
  mutate_at(c("wkdcorga", "iorgact"), as.numeric) %>%
  transmute(
    iscoco,
    emplno,
    self_employed,
    is_supervisor,
    control_work = iorgact,
    control_daily = wkdcorga,
    control_daily = case_when(
      control_daily >= 8 & control_daily <= 10 ~ 1,
      control_daily >= 5 & control_daily <= 7 ~ 2,
      control_daily >= 2 & control_daily <= 4 ~ 3,
      control_daily >= 0 & control_daily <= 1 ~ 4,
      TRUE ~ NA
    )
  ) %>%
  mutate(emplno = as.numeric(emplno)) %>%
  rename(isco88 = iscoco)

ess$isco88[nchar(ess$isco88) > 4] <- NA
ess$isco88 <- repair_isco(ess$isco88)

ess <-
  ess %>%
  mutate_at(c("control_work", "control_daily"), ~ if_else(.x > 4, NA, .x)) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    isco68 = isco88_to_isco68(isco88),
    isco08 = isco68_to_isco08(isco68),
    isco88com = isco88_to_isco88com(isco88)
  ) %>%
  relocate(isco68, isco88, isco88com, isco08, everything())

usethis::use_data(ess, overwrite = TRUE)
