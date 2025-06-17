library(readr)
library(purrr)
library(dplyr)

setwd(here::here())

all_translation <- list.files("data-raw/social_classes/translation", pattern = "csv", full.names = TRUE)

all_schemas <- map(all_translation, ~ {
  .x %>%
    read_csv(col_types = list(.default = col_character())) %>%
    mutate_all(~ if_else(.x == ".", NA_character_, .x))
})

names(all_schemas) <- gsub(".csv", "", basename(all_translation))

all_labels_csv <- list.files("data-raw/social_classes/labels", pattern = "csv", full.names = TRUE)

# Ignore problems(), this is simply due to msec having a ',' but we wrap it into
# "" so the labels are correct
all_labels <- map(all_labels_csv, ~ {
  .x %>%
    read_csv(col_types = list(.default = col_character())) %>%
    mutate_all(~ if_else(.x == ".", NA_character_, .x))
})

names(all_labels) <- gsub(".csv", "", basename(all_labels_csv))

usethis::use_data(all_schemas, overwrite = TRUE)
usethis::use_data(all_labels, overwrite = TRUE)
