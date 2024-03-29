library(readr)
library(stringr)
library(tibble)
library(dplyr)

extract_df <- function(schema_txt, pattern_split = "\\s") {
  name_schema <-
    schema_txt[1] %>%
    str_replace_all("% CODELIST-|% LABELLIST-", "") %>%
    str_replace_all("^(.*?)\\-(.*)$", "\\1_to_\\2") %>%
    str_replace_all("-", "_")

  variable_start <- schema_txt %>%
    str_detect("variables") %>%
    which()

  variable_end <- which(!str_detect(schema_txt, "%"))[1] - 1
  variables_txt <- paste(schema_txt[variable_start:variable_end], collapse = " ")

  variables_txt <-
    variables_txt %>%
    str_replace_all("%|variables:|, .+$", "") %>%
    str_trim() %>%
    str_split(" ") %>%
    .[[1]]

  variables_txt <- variables_txt[variables_txt != ""]

  schema_txt <- schema_txt[!str_detect(schema_txt, "^%")]
  schema_txt <- str_split(schema_txt, pattern = pattern_split, simplify = TRUE)
  schema_txt <- as_tibble(schema_txt)
  colnames(schema_txt) <- variables_txt

  list(name = name_schema, schema_df = schema_txt)
}

extract_df_label <- function(schema_txt, pattern_split = "\\s") {

  name_schema <-
    schema_txt[1] %>%
    str_replace_all("% CODELIST-|% LABELLIST-", "") %>%
    str_replace_all("^(.*?)\\-(.*)$", "\\1_to_\\2") %>%
    str_replace_all("-", "_")

  variable_start <- schema_txt %>%
    str_detect("variables") %>%
    which()

  variable_end <- which(!str_detect(schema_txt, "%"))[1] - 1
  variables_txt <- paste(schema_txt[variable_start:variable_end], collapse = " ")

  variables_txt <-
    variables_txt %>%
    str_replace_all("%|variables:|, .+$", "") %>%
    str_trim() %>%
    str_split(" ") %>%
    .[[1]]

  variables_txt <- variables_txt[variables_txt != ""]

  schema_txt <- schema_txt[!str_detect(schema_txt, "^%")]
  schema_txt <- str_split(schema_txt, pattern = pattern_split, simplify = TRUE, n = 2)
  schema_txt <- as_tibble(schema_txt) %>% mutate_all(str_trim)
  colnames(schema_txt) <- variables_txt

  list(name = name_schema, schema_df = schema_txt)
}

extract_label <- function(schema_txt, pattern = "\\s") {
  label <-
    schema_txt %>%
    ## str_replace_all(" & ", "@") %>%
    ## str_replace_all("([A-Z]) ([A-Z])", "\\1#\\2") %>%
    str_replace_all('\\"', "'") %>%
    extract_df_label(pattern)

  label_df <- label$schema_df
  label_df[[2]] <-
    label_df[[2]] %>%
    str_replace_all("@", " & ") %>%
    str_replace_all("#", " ")

  list(name = label$name, label_df = label_df)
}

setwd(here::here("data-raw/social_classes/"))
schema <- read_lines("social_classes.txt")

labels_section <- which(str_detect(schema, "LABELLIST"))[1]
schema_filtered <- schema[1:(labels_section - 1)]

## Conversaion of schemas
begin <- which(str_detect(schema_filtered, "% CODELIST"))
end <- which(str_detect(schema_filtered, "% END"))

all_schemas <- list()

for (i in seq_along(begin)) {
  schema_ch <- schema_filtered[begin[i]:end[i]]
  schema_ch  <- extract_df(schema_ch)
  all_schemas[[schema_ch$name]] <- schema_ch$schema_df
  write_csv(schema_ch$schema_df, paste0("translation/", schema_ch$name, ".csv"))
}

# TODO: you fixed all of these things manually:

# isco88_to_siopts on ISCO-88 == 6154 has leading zeroes that mess it up
# isco08_to_siops on ISCO-08 == 6300 has leading zeroes that messes it up

# isco08_to_esec  and isco88_to_esec have the digits in three digits but without a trailing zero (only 131 instead of 1310). I add that trailing zero manually.

# isco08_to_esec labels, you added a new column to be able to match ESEC-simplified

# isco08_to_esec has comments on last three digits. What should you do with it?


## Schema labels
schema_label <- schema[labels_section:length(schema)]

begin <- which(str_detect(schema_label, "% LABELLIST"))
end <- which(str_detect(schema_label, "% END"))

all_labels <- list()

for (i in seq_along(begin)) {
  labels_ch <- schema_label[begin[i]:end[i]]
  cleaned_labels <- extract_label(labels_ch)
  all_labels[[cleaned_labels$name]] <- cleaned_labels$label_df
  write_csv(cleaned_labels$label_df, paste0("labels/", cleaned_labels$name, ".csv"))
}
