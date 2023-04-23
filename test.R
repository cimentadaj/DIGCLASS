library(dplyr)
library(stringr)

ess %>%
  mutate(
    isco88_three = isco88_swap(isco88, from = 4, to = 3),
    esec_full_no_label = isco88com_to_esec(isco88_three, is_supervisor, self_employed, emplno, full_method = TRUE, label = TRUE),
    esec_simple = isco88com_to_esec(isco88_three, is_supervisor, self_employed, emplno, full_method = FALSE, label = TRUE)
  ) %>%
  select(isco88_three, starts_with("esec"))
