library(DIGCLASS)
library(tidyverse)

isco68 <- read_csv("data-raw/social_classes/labels/isco68.csv")

isco68_full <-
  isco68 %>%
  mutate(
    major = if_else(str_sub(ISCO68, start = -3, end = -1) == "000", ISCO68, NA),
    submajor = if_else(str_sub(ISCO68, start = -2, end = -1) == "00", ISCO68, NA),
    minor = if_else(str_sub(ISCO68, start = -1, end = -1) == "0", ISCO68, NA),
    unit = ISCO68
  ) %>%
  fill(major, submajor, minor, unit)

write_csv(isco68_full, "data-raw/social_classes/translation/isco68_hierarchy.csv")

isco88 <- read_csv("data-raw/social_classes/labels/isco88.csv")

isco88_full <-
  isco88 %>%
  mutate(
    major = if_else(str_sub(ISCO88, start = -3, end = -1) == "000", ISCO88, NA),
    submajor = if_else(str_sub(ISCO88, start = -2, end = -1) == "00", ISCO88, NA),
    minor = if_else(str_sub(ISCO88, start = -1, end = -1) == "0", ISCO88, NA),
    unit = ISCO88
    ) %>%
  fill(major, submajor, minor)

write_csv(isco88_full, "data-raw/social_classes/translation/isco88_hierarchy.csv")

isco08 <- read_csv("data-raw/social_classes/labels/isco08.csv")

isco08_full <-
  isco08 %>%
  mutate(
    major = if_else(str_sub(ISCO08, start = -3, end = -1) == "000", ISCO08, NA),
    submajor = if_else(str_sub(ISCO08, start = -2, end = -1) == "00", ISCO08, NA),
    minor = if_else(str_sub(ISCO08, start = -1, end = -1) == "0", ISCO08, NA),
    unit = ISCO08
  ) %>%
  fill(major, submajor, minor)

write_csv(isco08_full, "data-raw/social_classes/translation/isco08_hierarchy.csv")
