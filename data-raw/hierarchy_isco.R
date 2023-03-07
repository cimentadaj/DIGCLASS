library(DIGCLASS)
library(tidyverse)

isco68 <- read_csv("social_classes/labels/isco68.csv")

isco68 %>%
  mutate(
    major = if_else(str_sub(ISCO68, start = -3, end = -1) == "000", ISCO68, NA),
    submajor = if_else(str_sub(ISCO68, start = -2, end = -1) == "00", ISCO68, NA),
    minor = if_else(str_sub(ISCO68, start = -1, end = -1) == "0", ISCO68, NA),
    subminor = ISCO68
  ) %>%
  fill(major, submajor, minor) %>%
  filter(
    submajor != minor, minor != subminor
  )


isco88 <- read_csv("social_classes/labels/isco88.csv")


isco88 %>%
  mutate(
    major = if_else(str_sub(ISCO88, start = -3, end = -1) == "000", ISCO88, NA),
    submajor = if_else(str_sub(ISCO88, start = -2, end = -1) == "00", ISCO88, NA),
    minor = if_else(str_sub(ISCO88, start = -1, end = -1) == "0", ISCO88, NA),
    subminor = ISCO88
    ) %>%
  fill(major, submajor, minor) %>%
  filter(
    major != submajor, submajor != minor, minor != subminor
  )


isco08 <- read_csv("social_classes/labels/isco08.csv")

isco08 %>%
  mutate(
    major = if_else(str_sub(ISCO08, start = -3, end = -1) == "000", ISCO08, NA),
    submajor = if_else(str_sub(ISCO08, start = -2, end = -1) == "00", ISCO08, NA),
    minor = if_else(str_sub(ISCO08, start = -1, end = -1) == "0", ISCO08, NA),
    subminor = ISCO08
  ) %>%
  fill(major, submajor, minor) %>%
  filter(
    major != submajor, submajor != minor, minor != subminor
  )

