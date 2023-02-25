
ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isco88(iscoco, label = FALSE))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco68(iscoco, label = FALSE))


ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isco08(iscoco))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco08(iscoco, label = TRUE))


ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_isco88(iscoco, label = TRUE))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isco88com(iscoco, label = TRUE))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_isei(iscoco))


ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco68_to_siops(iscoco))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_isei(iscoco))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_siops(iscoco))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco88_to_mps(iscoco))


ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_isei(iscoco))

ess %>%
  select(iscoco) %>%
  mutate(isco08 = isco08_to_siops(iscoco))

ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    egp = isco68_to_egp(iscoco, self_employed, emplno, label = TRUE)
  )


ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    egp = isco68_to_egp11(iscoco, self_employed, emplno, label = FALSE)
  )

ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    egp = isco88_to_egp(iscoco, self_employed, emplno, label = FALSE)
  )


ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    egp = isco88_to_egp11(iscoco, self_employed, emplno, label = FALSE)
  )

ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    oesch = isco88_to_oesch(iscoco, self_employed, emplno, label = FALSE)
  )


ess %>%
  select(iscoco, emplno, self_employed) %>%
  mutate(
    emplno = if_else(emplno > 10000, 0, emplno),
    oesch = isco08_to_oesch(iscoco, self_employed, emplno, label = FALSE)
  )

