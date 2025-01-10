common_translator <- function(x, input_var, output_var, translate_df, translate_label_df, label, check_isco = NULL, digits = 4, repair_isco = TRUE, to_factor = FALSE, is_isco = TRUE) {

  if (repair_isco) {
    # All checks must being by whether the function has 4 digits (regardless of it's 1300 or 13111)
    x <- repair_isco(x, digits = 4)
  }

  if (is_isco) {
    # If it's ESCO we skip this check

    # TODO: in case we want to count the exact number of digits, uncomment.
    # this was commented because we want to allow flexibility. users with
    # 3-digits should be able to translate even if they get NAs
    ## count_digits(x, digits = digits)
    check_isco(x, check_isco)
  }

  res <-
    tibble::tibble(x = as.character(x)) %>%
    dplyr::left_join(translate_df, by = c("x" = input_var), multiple = "first")

  if (label) {
    no_labs <- c("isei", "siops", "mps88", "iseisps")
    if (tolower(output_var) %in% no_labs) {
      stop(
        "Labels not available for these schemas: ",
        paste(no_labs, collapse = ", "), ". Set `label` to `FALSE` to translate this schema without labels."
      )
    }

    res <-
      tibble::tibble(x_label = res[[output_var]]) %>%
      dplyr::left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
    if (to_factor) {
      transformed <- factor(transformed, levels = unique(translate_label_df[[2]]), ordered = TRUE)
    }
  } else {
    transformed <- res[[output_var]]
    if (to_factor) {
      transformed <- factor(transformed, levels = unique(translate_label_df[[1]]), ordered = TRUE)
    }
  }

  transformed
}

check_isco <- function(x, check_isco) {
  if (!is.null(check_isco)) {
    check_isco <- match.arg(check_isco, c("isco08", "isco68", "isco88", "isco88com"))

    lookup_check <- list(
      `isco08` = all_labels$isco08[[1]],
      `isco68` = all_labels$isco68[[1]],
      `isco88` = all_labels$isco88[[1]],
      `isco88com` = all_labels$isco88com[[1]]
    )

    x_clean <- x[!is.na(x)]
    chosen_isco <- lookup_check[[as.character(check_isco)]]

    if (!all(x_clean %in% chosen_isco)) {
      unavailable_vals <- setdiff(unique(x_clean), chosen_isco)
      cli::cli_alert_warning(c(
        "x" = "`x` might not be {toupper(check_isco)}. These values do not belong to {toupper(check_isco)}:\n",
        "x" = paste0("x Occupation `", unavailable_vals, "`\n")
        )
      )
    }
  }
}

count_digits <- function(x, digits) {
  all_digits <- c(
    `1` = 3,
    `2` = 2,
    `3` = 1,
    `4` = 0
  )

  # Exclude 0100, 0000, etc..
  # because they mess up the counting of 0's. Even a 4 digit can
  # contain 0100 and technically contains three zeroes
  begins_zero <- substr(x, 1, 1) == 0
  x <- x[!begins_zero]
  x <- x[!is.na(x)]

  counts <- table(sapply(strsplit(x, ""), function(code) sum(code == "0"))) / length(x)
  # Add missing numbers as names with value 0
  missing_numbers <- as.character(setdiff(unname(all_digits), names(counts)))
  counts[missing_numbers] <- 0

  if (digits == 4) {
    if (counts[as.character(1)] > 0.95) {
      # This means that the number of ISCOs with 1 digit is too high, so this is probably
      # a 3-digit number.
      counts[as.character(0)] <- counts[as.character(0)]
    } else {
        # combine counts of 1 and 0 zeroes into only 0 because the 1 digit ISCO contains
        # a lot of both.
      counts[as.character(0)] <- counts[as.character(1)] + counts[as.character(0)]
    }

  }

  n_zeroes <- as.character(all_digits[[as.character(digits)]])
  if (counts[n_zeroes] < 0.85) {
    cli::cli_abort("`x` might not be a {digits}-digit ISCO vector. Convert to {digits} digits using `isco08/88/68_swap`.")
  }
}

multiple_cols_translator <- function(x,
                                     col_position,
                                     output_var,
                                     translate_df,
                                     translate_label_df,
                                     label,
                                     check_isco = NULL,
                                     digits = 4,
                                     to_factor = FALSE) {

  # All checks must being by whether the function has 4 digits (regardless of it's 1300 or 13111)
  x <- repair_isco(x, digits = 4)


  # TODO: in case we want to count the exact number of digits, uncomment.
  # this was commented because we want to allow flexibility. users with
  # 3-digits should be able to translate even if they get NAs
  ## count_digits(x, digits = digits)
  check_isco(x, check_isco)

  class_match <- match(x, translate_df[[1]])
  matrix_translate_df <- as.matrix(translate_df)
  transformed <- matrix_translate_df[cbind(class_match, col_position)]

  if (label) {
    res <-
      tibble::tibble(x_label = transformed) %>%
      dplyr::left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
    if (to_factor) {
      transformed <- factor(transformed, levels = translate_label_df[[2]], ordered = TRUE)
    }
  } else {
    if (to_factor) {
      transformed <- factor(transformed, levels = translate_label_df[[1]], ordered = TRUE)
    }
  }

  transformed
}

managers_professionals_helper <- function(x,
                                          esec,
                                          is_supervisor,
                                          n_employees,
                                          self_employed,
                                          lookup_labels,
                                          schema_labels,
                                          label,
                                          to_factor = FALSE
                                          ) {
  # TODO: Since this function does not have an excel, I've coded the "rules" manually
  # but ideally we want to move all of this into social_classes.txt such that
  # all schemas are contained in a single file.
  one_digit_isco <- as.numeric(substr(x, 1, 1))
  has_subordinates <- (is_supervisor | n_employees > 0)
  class_first <- esec == 1
  class_second <- esec == 2
  isco_first <- one_digit_isco == 1
  isco_zero <- one_digit_isco == 0
  large_employees <- self_employed == 1 & n_employees >= 10
  small_employees <- self_employed == 1 & dplyr::between(n_employees, 1, 9)
  self_employed_no_employees <- self_employed == 1 & n_employees < 1
  supervisor <- self_employed == 0 & n_employees > 0
  employee <- self_employed == 0 & n_employees == 0

  higher_managers_1 <- class_first & (isco_zero | isco_first | has_subordinates)
  higher_managers_2 <- class_first & (large_employees | small_employees)
  higher_professional <- class_first & one_digit_isco > 1 & (self_employed_no_employees | employee)
  lower_managers_1 <- class_second & (isco_zero | isco_first | has_subordinates)
  lower_managers_2 <- class_second & (large_employees | small_employees)
  lower_professional <- class_second & one_digit_isco > 1 & (self_employed_no_employees | employee)

  mp <- dplyr::case_when(
    higher_managers_1 | higher_managers_2 ~ "1",
    higher_professional ~ "2",
    lower_managers_1 | lower_managers_2 ~ "3",
    lower_professional ~ "4",
    TRUE ~ lookup_labels[esec]
  )


  if (label) {
    mp <- schema_labels[mp]
    if (to_factor) {
      mp <- factor(mp, levels = unname(schema_labels), ordered = TRUE)
    }
  } else if (to_factor) {
    mp <- factor(mp, levels = names(schema_labels), ordered = TRUE)
  }

  mp <- unname(mp)
  mp
}

construct_eseg <- function(isco1, isco2, work_status, main_activity, age, type, label, to_factor = FALSE) {
  type <- match.arg(type, c("one-digit", "two-digit"))

  eseg <-
    dplyr::case_when(
      # 0 digit
      isco2 %in% "02" & work_status %in% 0 ~ 3.5,
      isco2 %in% "03" & work_status %in% 0 ~ 5.4,

      # 1 digit
      isco2 %in% "11" & work_status %in% 1 ~ 1.1,
      isco2 %in% "11" & work_status %in% 0 ~ 1.3,
      isco2 %in% "12" & work_status %in% 1 ~ 1.1,
      isco2 %in% "12" & work_status %in% 0 ~ 1.3,
      isco2 %in% "13" & work_status %in% 1 ~ 1.1,
      isco2 %in% "13" & work_status %in% 0 ~ 1.3,
      isco2 %in% "14" & work_status %in% 1 ~ 1.2,
      isco2 %in% "14" & work_status %in% 0 ~ 1.4,
      isco1 %in% "1" & work_status %in% c(1, 0) & main_activity %in% 5 ~ 8.1,
      isco1 %in% "1" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,

      # 2 digit
      isco2 %in% "21" & work_status %in% c(1, 0) ~ 2.1,
      isco2 %in% "22" & work_status %in% c(1, 0) ~ 2.2,
      isco2 %in% "23" & work_status %in% c(1, 0) ~ 2.5,
      isco2 %in% "24" & work_status %in% c(1, 0) ~ 2.3,
      isco2 %in% "25" & work_status %in% c(1, 0) ~ 2.1,
      isco2 %in% "26" & work_status %in% c(1, 0) ~ 2.4,
      isco1 %in% "2" & work_status %in% c(1, 0) & main_activity %in% 5 ~ 8.2,
      isco1 %in% "2" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,

      # 3 digit
      isco2 %in% "31" & work_status %in% 0 ~ 3.1,
      isco2 %in% "32" & work_status %in% 0 ~ 3.2,
      isco2 %in% "33" & work_status %in% 0 ~ 3.3,
      isco2 %in% "34" & work_status %in% 0 ~ 3.4,
      isco2 %in% "35" & work_status %in% 0 ~ 3.1,
      isco1 %in% "3" & work_status %in% 1 ~ 4.2,
      isco1 %in% "3" & work_status %in% 0 & main_activity %in% 5 ~ 8.3,
      isco1 %in% "3" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,

      # 4 digit
      isco2 %in% "41" & work_status %in% 0 ~ 5.1,
      isco2 %in% "42" & work_status %in% 0 ~ 5.2,
      isco2 %in% "43" & work_status %in% 0 ~ 5.1,
      isco2 %in% "44" & work_status %in% 0 ~ 5.1,
      isco1 %in% "4" & work_status %in% 1 ~ 4.2,
      isco1 %in% "4" & work_status %in% 0 & main_activity %in% 5 ~ 8.5,
      isco1 %in% "4" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,

      # 5 digit
      isco2 %in% "51" & work_status %in% 0 ~ 7.1,
      isco2 %in% "52" & work_status %in% 0 ~ 7.1,
      isco2 %in% "53" & work_status %in% 0 ~ 5.3,
      isco2 %in% "54" & work_status %in% 0 ~ 5.4,
      isco1 %in% "5" & work_status %in% 1 ~ 4.2,
      isco1 %in% "5" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,
      # Here codes 8.5 and 8.7 overlap meaning that I could assign any of the two. I
      # picked 8.7 becasue 8.5 was used earlier.
      isco1 %in% "5" & work_status %in% 0 & main_activity %in% 5 ~ 8.7,

      # 6 digit
      isco1 %in% "6" & work_status %in% 1 ~ 4.1,
      isco1 %in% "6" & work_status %in% 0 ~ 7.4,
      isco1 %in% "6" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,
      isco1 %in% "6" & work_status %in% 0 & main_activity %in% 5 ~ 8.7,

      # 7 digit
      isco2 %in% "71" & work_status %in% 0 ~ 6.1,
      isco2 %in% "72" & work_status %in% 0 ~ 6.3,
      isco2 %in% "73" & work_status %in% 0 ~ 6.3,
      isco2 %in% "74" & work_status %in% 0 ~ 6.3,
      isco2 %in% "75" & work_status %in% 0 ~ 6.2,
      isco1 %in% "7" & work_status %in% 1 ~ 4.3,
      isco1 %in% "7" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,
      isco1 %in% "7" & work_status %in% 0 & main_activity %in% 5 ~ 8.6,

      # 8 digit
      isco2 %in% "81" & work_status %in% 0 ~ 6.4,
      isco2 %in% "82" & work_status %in% 0 ~ 6.4,
      isco2 %in% "83" & work_status %in% 0 ~ 6.5,
      isco1 %in% "8" & work_status %in% 1 ~ 4.3,
      isco1 %in% "8" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,
      isco1 %in% "8" & work_status %in% 0 & main_activity %in% 5 ~ 8.6,

      # 9 digit
      isco2 %in% "91" & work_status %in% 0 ~ 7.3,
      isco2 %in% "92" & work_status %in% 0 ~ 7.2,
      isco2 %in% "93" & work_status %in% 0 ~ 7.2,
      isco2 %in% "94" & work_status %in% 0 ~ 7.2,
      isco2 %in% "95" & work_status %in% 0 ~ 7.3,
      isco2 %in% "96" & work_status %in% 0 ~ 7.2,
      isco1 %in% "9" & work_status %in% 1 ~ 4.3,
      isco1 %in% "9" & work_status %in% 1 & main_activity %in% 5 ~ 8.4,
      isco1 %in% "9" & work_status %in% 0 & main_activity %in% 5 ~ 8.7,

      # Other
      work_status %in% 2 & age > 64 & main_activity %in% 5 ~ 8.8,
      work_status %in% 2 & age < 65 & main_activity %in% 2 ~ 9.1,
      work_status %in% 2 & age < 65 & main_activity %in% 3 ~ 9.2,

      # Here codes 9.3 and 9.4 overlap meaning that I could assign any of the two. I
      # picked 9.3.
      work_status %in% 2 & age < 65 & main_activity %in% 4 ~ 9.3,

      # All else NA.
      TRUE ~ NA
    )

  eseg <- as.character(eseg)

  lookup_twodigits <- c(
    `1.1` = "11 Higher managerial self-employed",
    `1.2` = "12 Lower managerial self-employed",
    `1.3` = "13 Higher managerial employees",
    `1.4` = "14 Lower managerial employees",
    `2.1` = "21 Science, engineering and information and communications technology (ICT) professionals",
    `2.2` = "22 Health professionals",
    `2.3` = "23 Business and administration professionals",
    `2.4` = "24 Legal, social and cultural professionals",
    `2.5` = "25 Teaching professionals",
    `3.1` = "31 Science, engineering and ICT technicians and associated professionals",
    `3.2` = "32 Health associate professionals",
    `3.3` = "33 Business and administration associate professionals",
    `3.4` = "34 Legal, social and cultural associate professionals",
    `3.5` = "35 Non-commissioned armed forces officers",
    `4.1` = "41 Skilled agricultural self-employed workers",
    `4.2` = "42 Technicians, clerical support, services and sales self-employed workers",
    `4.3` = "43 Craft and related trades self-employed workers",
    `5.1` = "51 General and numerical clerks and other clerical support employees",
    `5.2` = "52 Customer services clerks",
    `5.3` = "53 Personal care employees",
    `5.4` = "54 Armed forced occupations and protective service employees",
    `6.1` = "61 Building and related trade employees",
    `6.2` = "62 Food processing, wood working, garment employees",
    `6.3` = "63 Metal, machinery, handicraft, printing, electrical and electronic trades employees",
    `6.4` = "64 Stationary plant and machine operators and assemblers",
    `6.5` = "65 Drivers",
    `7.1` = "71 Personal services and sales employees",
    `7.2` = "72 Blue collar employees and food preparation assistants in elementary occupations",
    `7.3` = "73 Cleaners and helpers and services employees in elementary occupations",
    `7.4` = "74 Agricultural employees",
    `8.1` = "81 Retired Managers",
    `8.2` = "82 Retired professionals",
    `8.3` = "83 Retired technicians and associate professionals",
    `8.4` = "84 Retired small entrepreneurs",
    `8.5` = "85 Retired skilled white collars",
    `8.6` = "86 Retired skilled blue-collars",
    `8.7` = "87 Retired less skilled workers",
    `8.8` = "88 Other inactive aged 65 or more",
    `9.1` = "91 Students",
    `9.2` = "92 Permanently disabled",
    `9.3` = "93 Unemployed not elsewhere classified",
    `9.4` = "94 Other inactive aged less than 65 years"
  )

  lookup_onedigit <- c(
    `1` = "1 Manager",
    `2` = "2 Professionals",
    `3` = "3 Technicians and associated professeional employees",
    `4` = "4 Small entrepreneur",
    `5` = "5 Clerks and skilled service employees",
    `6` = "6 Industrial skilled employees",
    `7` = "7 Less skilled employees",
    `8` = "8 Retired persons and non-employed people >=65",
    `9` = "9 Other non-employed persons aged < 65"
  )

  eseg <- unname(eseg)

  if (type == "two-digit") {
    if (label && to_factor) {
      eseg <- factor(lookup_twodigits[eseg], levels = unname(lookup_twodigits), ordered = TRUE)
      eseg <- unname(eseg)
    } else if (label) {
      eseg <- lookup_twodigits[eseg]
      eseg <- unname(eseg)
    } else if (to_factor) {
      eseg <- factor(eseg, levels = names(lookup_twodigits), ordered = TRUE)
    }
    return(eseg)
  }

  if (type == "one-digit") {
    eseg_one <- substr(eseg, 1, 1)
    if (label && to_factor) {
      eseg_one <- factor(lookup_onedigit[eseg_one], levels = unname(lookup_onedigit), ordered = TRUE)
      eseg_one <- unname(eseg_one)
    } else if (label) {
      eseg_one <- lookup_onedigit[eseg_one]
      eseg_one <- unname(eseg_one)
    } else if (to_factor) {
      eseg_one <- factor(eseg_one, levels = names(lookup_onedigit), ordered = TRUE)
    }
    return(eseg_one)
  }
}


construct_wright <- function(x,
                             is_supervisor,
                             self_employed,
                             n_employees,
                             control_work = NULL,
                             control_daily,
                             type,
                             label = FALSE,
                             to_factor = FALSE) {

  type <- match.arg(type, c("simple", "decision-making", "power-class"))

  n_employees[n_employees >= 1 & n_employees <= 9] <- 1
  n_employees[n_employees >= 10 & n_employees <= 66665] <- 2

  ## # * Program for constructing 'occr'
  occr <- x
  occr[occr == 2200] <- 1 # Added
  occr[occr == 2212] <- 1
  occr[occr == 2221] <- 1
  occr[occr == 2222] <- 1
  occr[occr == 2223] <- 1
  occr[occr == 2224] <- 1
  occr[occr == 2220] <- 1
  occr[occr == 2229] <- 2
  occr[occr == 2230] <- 2
  occr[occr == 3220] <- 2
  occr[occr == 3223] <- 2
  occr[occr == 3224] <- 2
  occr[occr == 3226] <- 2
  occr[occr == 3229] <- 2
  occr[occr == 3231] <- 2
  occr[occr == 3232] <- 2
  occr[occr == 3241] <- 2 # Added
  occr[occr == 2411] <- 3
  occr[occr == 2320] <- 4
  occr[occr == 2331] <- 4
  occr[occr == 2332] <- 4
  occr[occr == 2340] <- 4
  occr[occr == 2351] <- 4
  occr[occr == 2359] <- 4
  occr[occr == 3310] <- 4
  occr[occr == 3320] <- 4
  occr[occr == 3330] <- 4
  occr[occr == 3340] <- 4
  occr[occr == 2300] <- 4
  occr[occr == 2330] <- 4
  occr[occr == 2350] <- 4
  occr[occr == 3300] <- 4
  occr[occr == 2310] <- 5
  occr[occr == 2400] <- 5
  occr[occr == 2410] <- 5
  occr[occr == 2431] <- 5
  occr[occr == 2432] <- 5
  occr[occr == 2430] <- 5
  occr[occr == 2211] <- 5.5
  occr[occr == 2212] <- 5.5
  occr[occr == 2213] <- 5.5
  occr[occr == 2412] <- 5.5
  occr[occr == 2419] <- 5.5
  occr[occr == 2441] <- 5.5
  occr[occr == 2442] <- 5.5
  occr[occr == 2443] <- 5.5
  occr[occr == 2444] <- 5.5
  occr[occr == 2445] <- 5.5
  occr[occr == 2000] <- 5.5
  occr[occr == 2210] <- 5.5
  occr[occr == 2440] <- 5.5
  occr[occr == 2111] <- 6
  occr[occr == 2112] <- 6
  occr[occr == 2113] <- 6
  occr[occr == 2120] <- 6 # Added
  occr[occr == 2121] <- 6
  occr[occr == 2122] <- 6
  occr[occr == 2131] <- 6
  occr[occr == 2132] <- 6 # Added
  occr[occr == 2139] <- 6
  occr[occr == 2141] <- 6
  occr[occr == 2142] <- 6
  occr[occr == 2143] <- 6
  occr[occr == 2144] <- 6
  occr[occr == 2145] <- 6
  occr[occr == 2146] <- 6
  occr[occr == 2147] <- 6
  occr[occr == 2148] <- 6
  occr[occr == 2149] <- 6
  occr[occr == 3112] <- 6
  occr[occr == 3141] <- 6
  occr[occr == 3434] <- 6
  occr[occr == 2100] <- 6
  occr[occr == 2110] <- 6
  occr[occr == 2114] <- 6
  occr[occr == 2130] <- 6
  occr[occr == 2140] <- 6
  occr[occr == 3114] <- 7
  occr[occr == 3117] <- 7
  occr[occr == 3118] <- 7
  occr[occr == 3121] <- 7
  occr[occr == 3122] <- 7
  occr[occr == 3123] <- 7
  occr[occr == 3131] <- 7
  occr[occr == 3132] <- 7
  occr[occr == 3133] <- 7
  occr[occr == 3139] <- 7
  occr[occr == 3142] <- 7
  occr[occr == 3143] <- 7
  occr[occr == 3144] <- 7
  occr[occr == 3145] <- 7
  occr[occr == 3211] <- 7
  occr[occr == 3212] <- 7
  occr[occr == 3213] <- 7
  occr[occr == 3111] <- 7
  occr[occr == 3113] <- 7
  occr[occr == 3115] <- 7
  occr[occr == 3116] <- 7
  occr[occr == 3119] <- 7
  occr[occr == 3151] <- 7
  occr[occr == 2446] <- 8
  occr[occr == 2460] <- 8
  occr[occr == 2470] <- 8
  occr[occr == 3423] <- 8
  occr[occr == 3429] <- 8
  occr[occr == 3432] <- 8
  occr[occr == 3460] <- 8
  occr[occr == 3480] <- 8
  occr[occr == 5150] <- 8
  occr[occr == 2421] <- 9
  occr[occr == 2422] <- 9
  occr[occr == 2429] <- 9
  occr[occr == 2451] <- 10
  occr[occr == 2452] <- 10
  occr[occr == 2453] <- 10
  occr[occr == 2454] <- 10
  occr[occr == 2455] <- 10
  occr[occr == 3471] <- 10
  occr[occr == 3472] <- 10
  occr[occr == 3473] <- 10
  occr[occr == 3474] <- 10
  occr[occr == 3475] <- 10
  occr[occr == 5210] <- 10
  occr[occr == 2450] <- 10
  occr[occr == 3470] <- 10
  occr[occr == 3000] <- 10
  occr[occr == 3100] <- 10
  occr[occr == 3110] <- 10
  occr[occr == 3120] <- 10
  occr[occr == 3130] <- 10
  occr[occr == 3140] <- 10
  occr[occr == 3150] <- 10
  occr[occr == 3200] <- 10
  occr[occr == 3210] <- 10
  occr[occr == 1000] <- 11 # Added
  occr[occr == 1110] <- 11
  occr[occr == 1100] <- 11 # Added
  occr[occr == 1120] <- 11 # Added
  occr[occr == 1130] <- 11 # Added
  occr[occr == 1141] <- 11
  occr[occr == 1142] <- 11
  occr[occr == 1143] <- 11
  occr[occr == 2352] <- 11
  occr[occr == 3152] <- 11
  occr[occr == 1140] <- 11
  occr[occr == 1210] <- 12
  occr[occr == 1231] <- 12
  occr[occr == 1232] <- 12
  occr[occr == 1234] <- 12
  occr[occr == 1235] <- 12
  occr[occr == 1236] <- 12
  occr[occr == 1237] <- 12
  occr[occr == 1238] <- 12
  occr[occr == 1239] <- 12
  occr[occr == 1316] <- 12
  occr[occr == 1317] <- 12
  occr[occr == 1318] <- 12
  occr[occr == 1319] <- 12
  occr[occr == 3416] <- 12
  occr[occr == 1225] <- 13
  occr[occr == 1233] <- 13
  occr[occr == 1314] <- 13
  occr[occr == 1315] <- 13
  occr[occr == 3431] <- 14
  occr[occr == 3233] <- 14
  occr[occr == 4115] <- 14
  occr[occr == 4111] <- 15
  occr[occr == 4112] <- 15
  occr[occr == 4113] <- 15
  occr[occr == 4114] <- 15
  occr[occr == 4121] <- 15
  occr[occr == 4122] <- 15
  occr[occr == 4131] <- 15
  occr[occr == 4132] <- 15
  occr[occr == 4133] <- 15
  occr[occr == 4141] <- 15
  occr[occr == 4142] <- 15
  occr[occr == 4143] <- 15
  occr[occr == 4144] <- 15
  occr[occr == 4190] <- 15
  occr[occr == 4211] <- 15
  occr[occr == 4212] <- 15
  occr[occr == 4213] <- 15
  occr[occr == 4214] <- 15
  occr[occr == 4215] <- 15
  occr[occr == 4221] <- 15
  occr[occr == 4222] <- 15
  occr[occr == 4223] <- 15
  occr[occr == 3430] <- 15
  occr[occr == 4000] <- 15
  occr[occr == 4100] <- 15
  occr[occr == 4110] <- 15
  occr[occr == 4120] <- 15
  occr[occr == 4130] <- 15
  occr[occr == 4140] <- 15
  occr[occr == 4200] <- 15
  occr[occr == 4210] <- 15
  occr[occr == 4220] <- 15
  occr[occr == 3411] <- 16
  occr[occr == 3412] <- 16
  occr[occr == 3413] <- 16
  occr[occr == 3414] <- 16
  occr[occr == 3415] <- 16
  occr[occr == 3417] <- 16
  occr[occr == 3419] <- 16
  occr[occr == 3421] <- 16
  occr[occr == 3422] <- 16
  occr[occr == 5220] <- 16
  occr[occr == 5221] <- 16
  occr[occr == 5222] <- 16
  occr[occr == 5223] <- 16
  occr[occr == 5230] <- 16
  occr[occr == 9111] <- 16
  occr[occr == 9113] <- 16
  occr[occr == 3400] <- 16
  occr[occr == 3410] <- 16
  occr[occr == 3420] <- 16
  occr[occr == 5200] <- 16
  occr[occr == 9100] <- 16
  occr[occr == 9110] <- 16
  occr[occr == 1221] <- 17
  occr[occr == 1222] <- 17
  occr[occr == 1226] <- 17
  occr[occr == 1227] <- 17
  occr[occr == 1228] <- 17
  occr[occr == 1229] <- 17
  occr[occr == 1223] <- 17
  occr[occr == 1224] <- 17
  occr[occr == 1311] <- 17
  occr[occr == 1312] <- 17
  occr[occr == 1313] <- 17
  occr[occr == 1200] <- 17
  occr[occr == 1220] <- 17
  occr[occr == 1300] <- 17
  occr[occr == 1310] <- 17
  occr[occr == 7111] <- 18
  occr[occr == 7112] <- 18
  occr[occr == 7113] <- 18
  occr[occr >= 7121 & occr <= 7137] <- 18
  occr[occr >= 7139 & occr <= 7143] <- 18
  occr[occr >= 7211 & occr <= 7216] <- 18
  occr[occr >= 7221 & occr <= 7224] <- 18
  occr[occr >= 7231 & occr <= 7245] <- 18
  occr[occr >= 7311 & occr <= 7313] <- 18
  occr[occr >= 7321 & occr <= 7324] <- 18
  occr[occr == 7331] <- 18
  occr[occr == 7332] <- 18
  occr[occr >= 7341 & occr <= 7343] <- 18
  occr[occr == 7345] <- 18
  occr[occr == 7346] <- 18
  occr[occr >= 7411 & occr <= 7413] <- 18
  occr[occr == 7415] <- 18
  occr[occr == 7421] <- 18
  occr[occr == 7422] <- 18
  occr[occr >= 7433 & occr <= 7437] <- 18
  occr[occr == 7441] <- 18
  occr[occr == 7442] <- 18
  occr[occr == 8124] <- 18
  occr[occr == 7000] <- 18
  occr[occr == 7100] <- 18
  occr[occr == 7110] <- 18
  occr[occr == 7120] <- 18
  occr[occr == 7200] <- 18
  occr[occr == 7210] <- 18
  occr[occr == 7220] <- 18
  occr[occr == 7230] <- 18
  occr[occr == 7300] <- 18
  occr[occr == 7310] <- 18
  occr[occr == 7320] <- 18
  occr[occr == 7330] <- 18
  occr[occr == 7340] <- 18
  occr[occr == 7400] <- 18
  occr[occr == 7440] <- 18 # Added
  occr[occr >= 3441 & occr <= 3444] <- 19
  occr[occr == 3449] <- 19
  occr[occr == 3450] <- 19
  occr[occr >= 5161 & occr <= 5163] <- 19
  occr[occr == 5169] <- 19
  occr[occr == 0100] <- 19
  occr[occr == 5160] <- 19
  occr[occr == 110] <- 19 # Added
  occr[occr == 5110] <- 20 # Added
  occr[occr == 5112] <- 20
  occr[occr == 8311] <- 20
  occr[occr == 8312] <- 20
  occr[occr >= 8321 & occr <= 8324] <- 20
  occr[occr >= 8331 & occr <= 8334] <- 20
  occr[occr == 8340] <- 20
  occr[occr == 8300] <- 20
  occr[occr == 8310] <- 20
  occr[occr == 8320] <- 20
  occr[occr == 8330] <- 20
  occr[occr == 9331] <- 20 # Added
  occr[occr == 9332] <- 20 # Added
  occr[occr == 9333] <- 20 # Added
  occr[occr == 7344] <- 21
  occr[occr == 7414] <- 21
  occr[occr == 7416] <- 21
  occr[occr == 7423] <- 21
  occr[occr == 7424] <- 21
  occr[occr == 7431] <- 21
  occr[occr == 7432] <- 21
  occr[occr == 8111] <- 21
  occr[occr == 8112] <- 21
  occr[occr == 8113] <- 21
  occr[occr >= 8121 & occr <= 8123] <- 21
  occr[occr == 8131] <- 21
  occr[occr >= 8139 & occr <= 8143] <- 21
  occr[occr >= 8151 & occr <= 8155] <- 21
  occr[occr >= 8159 & occr <= 8163] <- 21
  occr[occr == 8170] <- 21
  occr[occr == 8211] <- 21
  occr[occr == 8212] <- 21
  occr[occr >= 8221 & occr <= 8224] <- 21
  occr[occr >= 8229 & occr <= 8232] <- 21
  occr[occr == 8240] <- 21
  occr[occr >= 8251 & occr <= 8253] <- 21
  occr[occr >= 8261 & occr <= 8266] <- 21
  occr[occr >= 8269 & occr <= 8287] <- 21
  occr[occr == 8290] <- 21
  occr[occr == 9320] <- 21
  occr[occr == 9322] <- 21 # Added
  occr[occr == 7410] <- 21
  occr[occr == 7420] <- 21
  occr[occr == 7430] <- 21
  occr[occr == 8000] <- 21
  occr[occr == 8100] <- 21
  occr[occr == 8110] <- 21
  occr[occr == 8120] <- 21
  occr[occr == 8130] <- 21
  occr[occr == 8150] <- 21
  occr[occr == 8200] <- 21
  occr[occr == 8210] <- 21
  occr[occr == 8220] <- 21
  occr[occr == 8250] <- 21
  occr[occr == 8260] <- 21
  occr[occr >= 6121 & occr <= 6129] <- 22
  occr[occr == 6142] <- 22
  occr[occr == 9142] <- 22
  occr[occr == 9161] <- 22
  occr[occr >= 9311 & occr <= 9313] <- 22
  occr[occr == 9330] <- 22
  occr[occr == 9300] <- 22
  occr[occr == 9310] <- 22
  occr[occr == 6141] <- 23
  occr[occr >= 6151 & occr <= 6154] <- 23
  occr[occr == 9211] <- 23
  occr[occr == 9212] <- 23
  occr[occr == 9213] <- 23
  occr[occr == 6140] <- 23
  occr[occr == 6150] <- 23
  occr[occr == 9200] <- 23
  occr[occr == 9210] <- 23
  occr[occr == 3221] <- 24
  occr[occr == 3222] <- 24
  occr[occr == 3225] <- 24
  occr[occr == 3227] <- 24
  occr[occr == 3228] <- 24
  occr[occr == 5113] <- 25
  occr[occr == 5122] <- 25
  occr[occr == 5141] <- 25
  occr[occr == 5140] <- 25
  occr[occr == 5111] <- 26
  occr[occr == 5121] <- 26
  occr[occr >= 5131 & occr <= 5139] <- 26
  occr[occr == 5142] <- 26
  occr[occr == 5143] <- 26
  occr[occr == 5149] <- 26
  occr[occr == 5152] <- 26 # Added
  occr[occr == 9112] <- 26 # Added
  occr[occr == 9120] <- 26
  occr[occr >= 9131 & occr <= 9133] <- 26
  occr[occr == 9141] <- 26
  occr[occr >= 9151 & occr <= 9153] <- 26
  occr[occr == 9162] <- 26
  occr[occr == 5000] <- 26
  occr[occr == 5120] <- 26 # Added
  occr[occr == 5123] <- 26
  occr[occr == 5130] <- 26
  occr[occr == 9000] <- 26
  occr[occr == 9130] <- 26
  occr[occr == 9140] <- 26
  occr[occr == 9150] <- 26
  occr[occr == 9160] <- 26
  occr[occr >= 6121 & occr <= 6130] <- 27
  occr[occr == 6111] <- 27
  occr[occr == 6112] <- 27
  occr[occr == 6113] <- 27 # Added
  occr[occr == 6114] <- 27 # Added
  occr[occr == 6000] <- 27
  occr[occr == 6100] <- 27
  occr[occr == 6110] <- 27
  occr[occr == 6120] <- 27
  occr[occr == 6200] <- 27 # Added
  occr[occr == 6210] <- 27 # Added
  occr[occr == 1100] <- 11
  occr[occr == 1230] <- 12
  occr[occr == 2420] <- 9
  occr[occr == 3230] <- 2
  occr[occr == 3433] <- 14
  occr[occr == 3440] <- 19
  occr[occr == 5100] <- NA
  occr[occr == 0] <- NA

  skill <- occr

  skill[skill == 1] <- 1
  skill[skill == 3] <- 1
  skill[skill == 5] <- 1
  skill[skill == 5.5] <- 1
  skill[skill == 6] <- 1
  skill[skill == 9] <- 1
  skill[skill == 11] <- 1
  skill[skill == 12] <- 1
  skill[skill == 2] <- 2
  skill[skill == 4] <- 2
  skill[skill == 7] <- 2
  skill[skill == 8] <- 2
  skill[skill == 10] <- 2
  skill[skill == 13] <- 2
  skill[skill == 17] <- 2
  skill[skill == 18] <- 2
  skill[skill == 19] <- 2
  skill[skill == 25] <- 2
  skill[skill == 27] <- 2
  skill[skill == 14] <- 3
  skill[skill == 15] <- 3
  skill[skill == 16] <- 3
  skill[skill == 20] <- 3
  skill[skill == 21] <- 3
  skill[skill == 22] <- 3
  skill[skill == 23] <- 3
  skill[skill == 24] <- 3
  skill[skill == 26] <- 3

  wr_simp <- NA
  wr_simp[n_employees == 2] <- 1
  wr_simp[n_employees == 1] <- 2
  wr_simp[n_employees == 0] <- 3
  wr_simp[self_employed == 0 & is_supervisor == 1 & skill == 1] <- 4
  wr_simp[self_employed == 0 & is_supervisor == 0 & skill == 1] <- 6
  wr_simp[self_employed == 0 & is_supervisor == 1 & skill == 2] <- 7
  wr_simp[self_employed == 0 & is_supervisor == 0 & skill == 2] <- 9
  wr_simp[self_employed == 0 & is_supervisor == 1 & skill == 3] <- 10
  wr_simp[self_employed == 0 & is_supervisor == 0 & skill == 3] <- 12
  wr_simp <- as.character(wr_simp)

  # If it's type simple we return early. This was a request from
  # Oscar Smallenbroek. He wanted to be able to not provide "control_work"
  # if type equls simple, so I return early before computing the other classes
  if (type == "simple") {
    lookup_simple <- c(
      `1` = "Self empl w/10+ employees",
      `2` = "Self empl w/1-9 employees",
      `3` = "Self empl w/no empoyees",
      `4` = "Expert managers",
      `6` = "Expert workers",
      `7` = "Skilled manager/superv",
      `9` = "Skilled workers",
      `10` = "Low skilled manager/superv",
      `12` = "Low skilled workers"
    )

    if (label && to_factor) {
      wr_simp <- factor(lookup_simple[wr_simp], levels = unname(lookup_simple), ordered = TRUE)
      wr_simp <- unname(wr_simp)
    } else if (label) {
      wr_simp <- lookup_simple[wr_simp]
      wr_simp <- unname(wr_simp)
    } else if (to_factor) {
      wr_simp <- factor(wr_simp, levels = names(lookup_simple), ordered = TRUE)
    }
    return(wr_simp)
  }

  ## # MAN1: How to differentiate between supervisors and managers
  ## # based on whether they control their own work
  control_work[control_work >= 0 & control_work <= 7] <- 0
  control_work[control_work >= 8 & control_work <= 10] <- 1

  dp <- 0
  dp[self_employed == 0 & is_supervisor == 1 & control_work == 1] <- 1
  dp[self_employed == 0 & is_supervisor == 1 & control_work == 0] <- 2
  dp[self_employed == 0 & is_supervisor == 0 & control_work == 1] <- 3
  dp[self_employed == 0 & is_supervisor == 0 & control_work == 0] <- 4
  dp[dp == 0] <- NA

  man <- NA
  man[self_employed == 0 & dp == 1 & control_daily == 1] <- 1
  man[self_employed == 0 & dp == 1 & control_daily > 2] <- 3
  man[self_employed == 0 & dp == 2 & control_daily == 1] <- 2
  man[self_employed == 0 & dp == 1 & control_daily == 2] <- 2
  man[self_employed == 0 & dp == 2 & control_daily > 1] <- 3
  man[self_employed == 0 & dp == 3 & control_daily == 1] <- 2
  man[self_employed == 0 & dp == 3 & control_daily > 1] <- 3
  man[self_employed == 0 & dp == 4 & control_daily == 1] <- 3
  man[self_employed == 0 & dp == 4 & control_daily > 1] <- 4

  wr_dm <- NA
  wr_dm[wr_simp == 1] <- 1
  wr_dm[wr_simp == 2] <- 2
  wr_dm[wr_simp == 3] <- 3
  wr_dm[wr_simp == 4 & man == 1] <- 4
  wr_dm[wr_simp == 4 & man == 2] <- 5
  wr_dm[wr_simp == 6 & man == 2] <- 5
  wr_dm[wr_simp == 4 & man > 2] <- 6
  wr_dm[wr_simp == 6 & man > 2] <- 6
  wr_dm[wr_simp == 7 & man == 1] <- 7
  wr_dm[wr_simp == 7 & man == 2] <- 8
  wr_dm[wr_simp == 9 & man == 2] <- 8
  wr_dm[wr_simp == 7 & man > 2] <- 9
  wr_dm[wr_simp == 9 & man > 2] <- 9
  wr_dm[wr_simp == 10 & man == 1] <- 10
  wr_dm[wr_simp == 12 & man == 1] <- 10
  wr_dm[wr_simp == 10 & man == 2] <- 11
  wr_dm[wr_simp == 12 & man == 2] <- 11
  wr_dm[wr_simp == 10 & man > 2] <- 12
  wr_dm[wr_simp == 12 & man > 2] <- 12

  pc <- NA
  pc[wr_dm == 1] <- 1
  pc[wr_dm == 2] <- 2
  pc[wr_dm == 3] <- 3
  pc[wr_dm == 4] <- 4
  pc[wr_dm == 7] <- 4
  pc[wr_dm == 10] <- 4.5
  pc[wr_dm == 5] <- 5
  pc[wr_dm == 8] <- 5
  pc[wr_dm == 11] <- 5.5
  pc[wr_dm == 6] <- 6
  pc[wr_dm == 9] <- 7
  pc[wr_dm == 12] <- 7.5

  wr_p <- pc
  wr_p[pc == 6 & (control_daily %in% 1)] <- 6
  wr_p[pc == 7 & (control_daily %in% 1)] <- 6
  wr_p[pc == 6 & (control_daily %in% c(2, 3, 4))] <- 6.5
  wr_p[pc == 7 & (control_daily %in% c(2, 3, 4))] <- 6.5
  wr_p[pc == 7.5 & (control_daily %in% 1)] <- 7
  wr_p[pc == 7.5 & (control_daily %in% c(2, 3, 4))] <- 7.5

  wr_p2 <- wr_p
  wr_p <- NA
  wr_p[wr_p2 == 1] <- 1
  wr_p[wr_p2 == 2] <- 2
  wr_p[wr_p2 == 3] <- 3
  wr_p[wr_p2 == 4] <- 4
  wr_p[wr_p2 == 4.5] <- 4.5
  wr_p[wr_p2 == 5] <- 5
  wr_p[wr_p2 == 5.5] <- 5.5
  wr_p[wr_p2 == 6] <- 6
  wr_p[wr_p2 == 6.5] <- 7
  wr_p[wr_p2 == 7] <- 6.5
  wr_p[wr_p2 == 7.5] <- 7.5

  wr_dm <- as.character(wr_dm)
  wr_p <- as.character(wr_p)


  lookup_dm <- c(
    `1` = "Self empl w/10+ employees",
    `2` = "Self empl w/1-9 employees",
    `3` = "Self empl w/no empoyees",
    `4` = "Expert managers",
    `5` = "Expert supervisors",
    `6` = "Experts",
    `7` = "Skilled managers",
    `8` = "Skilled supervisors",
    `9` = "Skilled workers",
    `10` = "Low skilled managers",
    `11` = "Low skilled supervisors",
    `12` = "Low skilled workers"
  )

  lookup_p <- c(
    `1` = "Capitalist",
    `2` = "Small Employers",
    `3` = "Self empl",
    `4` = "Skilled Managers",
    `4.5` = "Low skilled Managers",
    `5` = "Skilled supervisors",
    `5.5` = "Low skilled supervisors",
    `6` = "Skilled Semi autonomous",
    `6.5` = "Low skilled Semi autonomous",
    `7` = "Skilled workers",
    `7.5` = "Low skilled workers"
  )

  if (type == "decision-making") {
    if (label && to_factor) {
      wr_dm <- factor(lookup_dm[wr_dm], levels = unname(lookup_dm), ordered = TRUE)
      wr_dm <- unname(wr_dm)
    } else if (label) {
      wr_dm <- lookup_dm[wr_dm]
      wr_dm <- unname(wr_dm)
    } else if (to_factor) {
      wr_dm <- factor(wr_dm, levels = names(lookup_dm), ordered = TRUE)
    }
    return(wr_dm)
  }

  if (type == "power-class") {
    if (label && to_factor) {
      wr_p <- factor(lookup_p[wr_p], levels = unname(lookup_p), ordered = TRUE)
      wr_p <- unname(wr_p)
    } else if (label) {
      wr_p <- lookup_p[wr_p]
      wr_p <- unname(wr_p)
    } else if (to_factor) {
      wr_p <- factor(wr_p, levels = names(lookup_p), ordered = TRUE)
    }
    return(wr_p)
  }
}


main_schema_to_others <- function(x, col_position, n_classes, schema, input_var, output_var, all_classes, label, check_isco = NULL, to_factor = FALSE) {

  main_class <-
    multiple_cols_translator(
      x = x,
      col_position = col_position,
      output_var = input_var,
      translate_df = schema,
      translate_label_df = NULL,
      check_isco = check_isco,
      label = FALSE
    )

  translation_tables <- all_classes[[as.character(n_classes)]]

  variant <- common_translator(
    main_class,
    input_var = input_var,
    output_var = output_var,
    translate_df = translation_tables[[1]],
    translate_label_df = translation_tables[[2]],
    label = label,
    # Do not repair because it's the translated main class which is not an ISCO variable
    repair_isco = FALSE,
    to_factor = to_factor
  )

  variant
}


rg_template_title <- function(from, to, digit = 4) {
  glue::glue("Translate {digit}-digit {from} to {to}")
}

rg_template_intro <- function(from, to, translate_df, digit = 4) {
  plural <- if (length(translate_df) == 1) "" else "s"

  glue::glue("This function translates a vector of {digit}-digit {from} codes to {to} codes using the translation table{plural} stored in `{paste0('all_schema$', translate_df, collapse = ' / ')}`.")
}

rg_template_details_iscogen <- function(from, to) {
  glue::glue("This translation was taken from the `iscogen` Stata package. For more details, check out the package documentation and search for `{from} -> {to}`.")
}

slot_digits <- function(digit = 3) {
  chr_dig <- as.character(digit)
  x <- list("4" = c(131, 1310, 10), "3" = c(131, 1310, 10), "2" = c(13, 1300, 100), "1" = c(1, 1000, 1000))
  chosen_x <- x[[chr_dig]]

  chosen_x
}

rg_template_digits_warning <- function(digit = 3) {
  chosen_x <- slot_digits(digit = digit)

  # IMPORTANTE: Do not change how some {digit} have {digit}- and other are just {digit} followed by a space
  # That's because in `isco_swap` I replace `{digit}-` for another digit in the docs.
  glue::glue("This function will accept {digit} digit codes as 4 digits. This means that if the {digit}-digit code is {chosen_x[1]} then it should be {chosen_x[2]}. All codes should be 4 digits, even though the code is represented as {digit}-digits ({chosen_x[2]}, {chosen_x[2] + chosen_x[3]}, etc..)")
}

rg_template_arg_x <- function(from, digit = 4) {
  glue::glue("A character vector of {digit}-digit {from} codes.")
}

rg_template_arg_x_digit <- function(from, digit = 4) {
  chosen_x <- slot_digits(digit = digit)
  x <- rg_template_arg_x(from, digit = digit)
  glue::glue("{x} This should be the 4-digit equivalent so instead of {chosen_x[1]}, the code should be {chosen_x[2]}, which is the 4-digit version of of the {digit}-digit ISCO.")
}


rg_template_arg_label <- function(to) {
  glue::glue("A logical value indicating whether to return the labels of the translated {to} codes (default is \\code{{FALSE}}).")
}

rg_template_arg_factor <- function(to) {
  glue::glue("A logical value indicating whether to return a factor instead of a character. The order of the labels is taken from the labels for {to} found in `all_labels` (default is \\code{{FALSE}}).")
}

rg_template_arg_supervisor <- function() {
  glue::glue("A numeric vector indicating whether each individual is a supervisor (1, e.g. responsible for other employees) or not (0).")
}


rg_template_arg_selfemployed <- function() {
  glue::glue("A numeric vector indicating whether each individual is self-employed (1) or an employee (0).")
}

rg_template_arg_nemployees <- function() {
  glue::glue("A numeric vector indicating the number of employees under each respondent.")
}


rg_template_return <- function(to) {
  glue::glue("A character vector of {to} codes.")
}

utils::globalVariables(c("all_schemas", "all_labels", "input_var", "output_var"))

## pad_right_with_zero <- function(x, width = 4) {
##   sapply(x, function(elem) {
##     needed <- width - nchar(elem)
##     if (needed > 0) {
##       paste0(elem, strrep("0", needed))
##     } else {
##       elem # If it's already >= total_length, leave it as is.
##     }
##   })
## }
