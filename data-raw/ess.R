## code to prepare `ESS` dataset goes here
library(readr)
library(dplyr)
library(DIGCLASS)

ess <-
  # This is just the fith round of the ESS
  read_csv("~/Downloads/ESS6e02_5.csv", col_types = list(.default = col_character()))

ess <-
  ess %>%
  mutate(
    self_employed = ifelse(emplrel == "2", 1, 0),
    is_supervisor = ifelse(jbspv == "1", 1, 0)
  ) %>%
  mutate_at(c("wkdcorga", "iorgact", "agea"), as.numeric) %>%
  transmute(
    isco08,
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
    ),
    work_status = case_when(
      emplrel == "1" ~ 0,
      emplrel == "2" ~ 1,
      emplrel == "3" ~ 1,
      # Not employed as 2
      mainact %in% c("3", "4") ~ 2,
      TRUE ~ NA
    ),
    ## main_activity: 1: working, 2: student, 3: disabled, 4: no paid work, 5: retired
    main_activity = case_when(
      mainact %in% "1" ~ 1,
      mainact %in% "2" ~ 2,
      mainact %in% "5" ~ 3,
      mainact %in% "8" ~ 4,
      mainact %in% "6" ~ 5,
      TRUE ~ NA
    ),
    agea = ifelse(agea > 500, NA, agea)
  ) %>%
  mutate(emplno = as.numeric(emplno))
  ## rename(isco88 = iscoco)

ess$isco08[nchar(ess$isco08) > 4] <- NA
ess$isco08 <- repair_isco(ess$isco08)

ess <-
  ess %>%
  filter(!is.na(isco08)) %>%
  mutate_at(c("control_work", "control_daily"), ~ if_else(.x > 4, NA, .x)) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    isco88 = isco08_to_isco88(isco08),
    isco68 = isco88_to_isco68(isco88),
    isco88com = isco88_to_isco88com(isco88)
  ) %>%
  relocate(isco68, isco88, isco88com, isco08, everything())

usethis::use_data(ess, overwrite = TRUE)
