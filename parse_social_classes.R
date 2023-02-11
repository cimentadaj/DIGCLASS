library(readr)
library(stringr)
library(tibble)
library(dplyr)

extract_df <- function(schema_txt, pattern_split = "\\s") {
  name_schema <-
    schema_txt[1] %>%
    str_replace_all("% CODELIST-|% LABELLIST-", "") %>%
    str_replace_all("-", "_to_")

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
    str_replace_all("-", "_to_")

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
}

# isco88_to_siopts on ISCO-88 == 6154 has leading zeroes that mess it up
# isco08_to_siops on ISCO-08 == 6300 has leading zeroes that messes it up
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
}

library(tidyverse)
ess <- read_csv("~/Downloads/ESS4e04_5.csv")

common_translator <- function(x, input_var, output_var, translate_df, translate_label_df, label) {
  res <-
    tibble(x = as.character(x)) %>%
    left_join(translate_df, by = c("x" = input_var))


  if (label) {
    no_labs <- c("isei", "siops", "mps88", "iseisps")
    if (tolower(output_var) %in% no_labs) {
      stop(
        "Labels not available for these schemas: ",
        paste(no_labs, collapse = ", "), ". Set `label` to `FALSE` to translate this schema without labels."
      )
    }

    res <-
      tibble(x_label = res[[output_var]]) %>%
      left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
  } else {
    transformed <- as.numeric(res[[output_var]])
  }

  transformed
}

isco68_to_isco88 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO88",
    translate_df = all_schemas$isco68_to_isco88,
    translate_label_df = all_labels$isco88,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isco88(iscoco, label = FALSE))

isco88_to_isco68 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO68",
    translate_df = all_schemas$isco88_to_isco68,
    translate_label_df = all_labels$isco68,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco68(iscoco, label = FALSE))


isco68_to_isco08 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISCO08",
    translate_df = all_schemas$isco68_to_isco08,
    translate_label_df = all_labels$isco08,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isco08(iscoco))

isco88_to_isco08 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO08",
    translate_df = all_schemas$isco88_to_isco08,
    translate_label_df = all_labels$isco08,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco08(iscoco, label = TRUE))


isco08_to_isco88 <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "ISCO88",
    translate_df = all_schemas$isco08_to_isco88,
    translate_label_df = all_labels$isco88,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_isco88(iscoco, label = TRUE))

isco88_to_isco88com <- function(x, label = FALSE) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISCO88COM",
    translate_df = all_schemas$isco88_to_isco88com,
    translate_label_df = all_labels$isco88com,
    label = label
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco88com(iscoco, label = TRUE))

isco68_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "ISEI",
    translate_df = all_schemas$isco68_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isei(iscoco))


isco68_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO68",
    output_var = "SIOPS",
    translate_df = all_schemas$isco68_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_siops(iscoco))

isco88_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "ISEI",
    translate_df = all_schemas$isco88_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isei(iscoco))

isco88_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "SIOPS",
    translate_df = all_schemas$isco88_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_siops(iscoco))

isco88_to_mps <- function(x) {
  common_translator(
    x,
    input_var = "ISCO88",
    output_var = "MPS88",
    translate_df = all_schemas$isco88_to_mps,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_mps(iscoco))


isco08_to_isei <- function(x) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "ISEI-08",
    translate_df = all_schemas$isco08_to_isei,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_isei(iscoco))

isco08_to_siops <- function(x) {
  common_translator(
    x,
    input_var = "ISCO08",
    output_var = "SIOPS-08",
    translate_df = all_schemas$isco08_to_siops,
    translate_label_df = NULL,
    label = FALSE
  )
}

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_siops(iscoco))

