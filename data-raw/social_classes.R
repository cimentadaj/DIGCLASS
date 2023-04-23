library(readr)

setwd(here::here())

all_translation <- list.files("data-raw/social_classes/translation", pattern = "csv", full.names = TRUE)
all_schemas <- lapply(all_translation, read_csv, col_types = list(.default = col_character()))
names(all_schemas) <- gsub(".csv", "", basename(all_translation))

all_labels_csv <- list.files("data-raw/social_classes/labels", pattern = "csv", full.names = TRUE)
all_labels <- lapply(all_labels_csv, read_csv, col_types = list(.default = col_character()))
names(all_labels) <- gsub(".csv", "", basename(all_labels_csv))

usethis::use_data(all_schemas, overwrite = TRUE)
usethis::use_data(all_labels, overwrite = TRUE)
