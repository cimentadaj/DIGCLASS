library(DIGCLASS)
library(tidyverse)

setwd(here::here())

isco68 <- read_csv("data-raw/social_classes/labels/isco68.csv") %>% arrange(ISCO68)
isco88 <- read_csv("data-raw/social_classes/labels/isco88.csv") %>% arrange(ISCO88)
isco08 <- read_csv("data-raw/social_classes/labels/isco08.csv") %>% arrange(ISCO08)

# ISCO68
army <- nchar(isco68$ISCO68) == 5

isco68_full <-
  isco68 %>%
  filter(!army) %>%
  mutate(ISCO68 = as.numeric(ISCO68)) %>%
  complete(ISCO68 = full_seq(ISCO68, 1)) %>%
  mutate(ISCO68 = DIGCLASS:::pad(ISCO68, 4)) %>%
  bind_rows(isco68[army, ]) %>%
  mutate(
    major = if_else(str_sub(ISCO68, start = -3, end = -1) == "000", ISCO68, NA),
    submajor = if_else(str_sub(ISCO68, start = -2, end = -1) == "00", ISCO68, NA),
    minor = if_else(str_sub(ISCO68, start = -1, end = -1) == "0", ISCO68, NA),
    unit = ISCO68
  ) %>%
  fill(major, submajor, minor) %>%
  select(-`ISCO68-label-E`) %>%
  fill(major, submajor, minor) %>%
  arrange(ISCO68)

write_csv(isco68_full, "data-raw/social_classes/translation/isco68_hierarchy.csv")

# ISCO88
isco88_full <-
  isco88 %>%
  mutate(ISCO88 = as.numeric(ISCO88)) %>%
  complete(ISCO88 = full_seq(ISCO88, 1)) %>%
  mutate(
    ISCO88 = repair_isco(ISCO88),
    major = if_else(str_sub(ISCO88, start = -3, end = -1) == "000", ISCO88, NA),
    submajor = if_else(str_sub(ISCO88, start = -2, end = -1) == "00", ISCO88, NA),
    minor = if_else(str_sub(ISCO88, start = -1, end = -1) == "0", ISCO88, NA),
    unit = ISCO88
  ) %>%
  select(-`ISCO88-label-E`) %>%
  fill(major, submajor, minor) %>%
  arrange(ISCO88)

write_csv(isco88_full, "data-raw/social_classes/translation/isco88_hierarchy.csv")

isco08_full <-
  isco08 %>%
  mutate(ISCO08 = as.numeric(ISCO08)) %>%
  complete(ISCO08 = full_seq(ISCO08, 1)) %>%
  mutate(
    ISCO08 = repair_isco(ISCO08),
    major = if_else(str_sub(ISCO08, start = -3, end = -1) == "000", ISCO08, NA),
    submajor = if_else(str_sub(ISCO08, start = -2, end = -1) == "00", ISCO08, NA),
    minor = if_else(str_sub(ISCO08, start = -1, end = -1) == "0", ISCO08, NA),
    unit = ISCO08
  ) %>%
  select(-`ISCO08-label-E`) %>%
  fill(major, submajor, minor) %>%
  arrange(ISCO08)

write_csv(isco08_full, "data-raw/social_classes/translation/isco08_hierarchy.csv")
