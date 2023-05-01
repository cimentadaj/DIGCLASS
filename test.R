library(dplyr)
library(stringr)

## ess %>%
##   mutate(
##     isco88_three = isco88_swap(isco88, from = 4, to = 3),
##     esec_full_no_label = isco88com_to_esec(isco88_three, is_supervisor, self_employed, emplno, full_method = TRUE, label = TRUE),
##     esec_simple = isco88com_to_esec(isco88_three, is_supervisor, self_employed, emplno, full_method = FALSE, label = TRUE)
##   ) %>%
##   select(isco88_three, starts_with("esec"))


## ess %>%
##   mutate(
##     isco08_three = isco08_swap(isco08, from = 4, to = 3),
##     esec_full_no_label = isco08_to_esec(
##       isco08_three,
##       is_supervisor,
##       self_employed,
##       emplno,
##       label = TRUE
##     ),
##     esec = isco08_to_esec(
##       isco08_three,
##       is_supervisor,
##       self_employed,
##       emplno,
##       label = FALSE
##     )
##   ) %>%
##   select(isco08_three, starts_with("esec"))


# convert to three digits
ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)

# Using the full method
ess %>%
  transmute(
    msec_label = isco08_to_msec(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    msec = isco08_to_msec(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    )
  )



## Evaluat ISCO68 to EPG/EPG11
fun <- isco68_to_egp_mp

ess %>%
  transmute(
    isco68,
    egp = isco68_to_egp(
      isco68,
      self_employed,
      emplno,
      label = FALSE
    ),
    egp_label = isco68_to_egp(
      isco68,
      self_employed,
      emplno,
      label = TRUE
    ),
    egp_mp = fun(
      isco68,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    ),
    egp_mp_label = fun(
      isco68,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    )
  ) %>%
  count(egp, egp_label, egp_mp_label) %>%
  arrange(as.numeric(egp)) %>%
  select(-n)


library(dplyr)

ess %>%
  transmute(
    isco88,
    egp = isco88_to_egp(isco88, self_employed, emplno, label = FALSE),
    egp_labels = isco88_to_egp(isco88, self_employed, emplno, label = TRUE),
    egp_mp = isco88_to_egp_mp(isco88, is_supervisor, self_employed, emplno, label = TRUE)
  ) %>%
  count(egp, egp_labels, egp_mp) %>%
  arrange(as.numeric(egp))

## Evaluat ISCO88COM to ESEC
library(dplyr)

# convert to three digits
ess$isco88_three <- isco88_swap(ess$isco88, from = 4, to = 3)

ess %>%
  transmute(
    esec = isco88com_to_esec(
      isco88_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    ),
    esec_label = isco88com_to_esec(
      isco88_three,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    esec_mp_label = isco88com_to_esec_mp(
      isco88_three,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    esec_mp = isco88com_to_esec_mp(
      isco88_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    )
  ) %>%
  count(esec, esec_mp) %>%
  arrange(as.numeric(esec)) %>%

## Evaluat ISCO08 to ESEC
library(dplyr)

# convert to three digits
ess$isco08_three <- isco08_swap(ess$isco88, from = 4, to = 3)

ess %>%
  transmute(
    esec = isco08_to_esec(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    ),
    esec_label = isco08_to_esec(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    esec_mp_label = isco08_to_esec_mp(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    esec_mp = isco08_to_esec_mp(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    )
  ) %>%
  count(esec, esec_mp_label) %>%
  arrange(as.numeric(esec))


res <- readxl::read_excel("/home/jorge/Downloads/class_schema/Carlos_ClassSchemes/ESeC (ISCO 88-08)/Euresec matrix (3 digits + 2 digits, full).xlsx", skip = 2, sheet = 3)

zero <- str_detect(res$Code, "^0")

res <-
  res %>%
  mutate(
    Code = case_when(
      zero ~ paste0(Code, "0"),
      TRUE ~ paste0(Code, "00")
    )
  ) %>%
  select(-Description)


formatted_output <- sprintf("%s %s %s %s %s %s\n", res[[1]], res[[2]], res[[3]], res[[4]], res[[5]], res[[6]])
cat(formatted_output)

library(dplyr)

# convert to two digits
ess$isco08_two <- isco08_swap(ess$isco08, from = 4, to = 2)

n_c <- 3

# isco88
ess %>% transmute(
  isco88,
  egp = isco88_to_egp(isco88, self_employed, emplno, n_classes = n_c, label = FALSE),
  egp_label = isco88_to_egp(isco88, self_employed, emplno, n_classes = n_c, label = TRUE)
)

# isco68
ess %>% transmute(
  isco68,
  egp = isco68_to_egp(isco68, self_employed, emplno, label = FALSE),
  egp_label = isco68_to_egp(isco68, self_employed, emplno, label = TRUE)
)

ess %>%
  transmute(
    esec_label = isco08_two_to_esec(
      isco08_two,
      is_supervisor,
      self_employed,
      emplno,
      label = TRUE
    ),
    esec = isco08_two_to_esec(
      isco08_two,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    )
  )


library(dplyr)

# convert to three digits
ess$isco08_three <- isco08_swap(ess$isco08, from = 4, to = 3)

esec_trans <-
  ess %>%
  transmute(
    isco08_three,
    is_supervisor,
    self_employed,
    emplno,
    esec = isco08_to_esec(
      isco08_three,
      is_supervisor,
      self_employed,
      emplno,
      label = FALSE
    )
  )


## code to generate microclass
res <- read_excel("~/Downloads/isco08 to micro with numeric labels.xls") %>% drop_na()
res$isco08 <- repair_isco(res$isco08)

tpa <-
  res %>%
  ## rename(microclass = micro_numeric) %>%
  distinct(micro_numeric, micro_label) %>%
  arrange(micro_numeric) %>%
  drop_na() %>%
  mutate(micro = as.character(micro_numeric))

left_join(tp, tpa, by = "micro")

formatted_output <- sprintf("%s %s\n", tp[[1]], tp[[2]])
cat(formatted_output)
