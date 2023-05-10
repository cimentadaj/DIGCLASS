common_translator <- function(x, input_var, output_var, translate_df, translate_label_df, label, digits = 4, repair_isco = TRUE) {

  if (repair_isco) {
    # All checks must being by whether the function has 4 digits (regardless of it's 1300 or 13111)
    x <- repair_isco(x, digits = 4)
  }

  count_digits(x, digits = digits)

  res <-
    tibble::tibble(x = as.character(x)) %>%
    dplyr::left_join(translate_df, by = c("x" = input_var))

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
  } else {
    transformed <- res[[output_var]]
  }

  transformed
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

multiple_cols_translator <- function(x, col_position, output_var, translate_df, translate_label_df, label, digits = 4) {

  # All checks must being by whether the function has 4 digits (regardless of it's 1300 or 13111)
  x <- repair_isco(x, digits = 4)
  count_digits(x, digits = digits)

  class_match <- match(x, translate_df[[1]])
  matrix_translate_df <- as.matrix(translate_df)
  transformed <- matrix_translate_df[cbind(class_match, col_position)]

  if (label) {
    res <-
      tibble::tibble(x_label = transformed) %>%
      dplyr::left_join(translate_label_df, by = c("x_label" = output_var))

    transformed <- res[[2]]
  } else {
    transformed <- transformed
  }

  transformed
}

managers_professionals_helper <- function(x, esec, is_supervisor, n_employees, self_employed, label) {
  # TODO: Since this function does not have an excel, I've coded the "rules" manually
  # but ideally we want to move all of this into social_classes.txt such that
  # all schemas are contained in a single file.
  one_digit_isco <- as.numeric(substr(x, 1, 1))
  has_subordinates <- (is_supervisor | n_employees > 0)
  esec_first <- esec == 1
  esec_second <- esec == 2
  isco_first <- one_digit_isco == 1
  isco_zero <- one_digit_isco == 0
  large_employees <- self_employed == 1 & n_employees >= 10
  small_employees <- self_employed == 1 & dplyr::between(n_employees, 1, 9)
  self_employed_no_employees <- self_employed == 1 & n_employees < 1
  supervisor <- self_employed == 0 & n_employees > 0
  employee <- self_employed == 0 & n_employees == 0

  higher_managers_1 <- esec_first & (isco_zero | isco_first | has_subordinates)
  higher_managers_2 <- esec_first & (large_employees | small_employees)
  higher_professional <- esec_first & one_digit_isco > 1 & (self_employed_no_employees | employee)
  lower_managers_1 <- esec_second & (isco_zero | isco_first | has_subordinates)
  lower_managers_2 <- esec_second & (large_employees | small_employees)
  lower_professional <- esec_second & one_digit_isco > 1 & (self_employed_no_employees | employee)

  mp <- dplyr::case_when(
    higher_managers_1 | higher_managers_2 ~ "1",
    higher_professional ~ "2",
    lower_managers_1 | lower_managers_2 ~ "3",
    lower_professional ~ "4",
    TRUE ~ esec
  )

  if (label) {
    labs <- c(
      "1" = "Higher Manager",
      "2" = "Higher Professional",
      "3" = "Lower Manager",
      "4" = "Lower Professional",
      "5" = "Higher-grade White-collar",
      "6" = "Self-employed and Small Employer",
      "7" = "Self-employed and Small Employer agriculture",
      "8" = "Higher-grade Blue-collar",
      "9" = "Lower-grade White-collar",
      "10" = "Lower-grade Blue-collar",
      "11" = "Routine"
    )

    mp <- labs[mp]
  }

  mp
}


construct_wright <- function(x,
                             is_supervisor,
                             self_employed,
                             n_employees,
                             control_work,
                             control_daily,
                             type,
                             label = FALSE) {

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


  ## # variable is different in rest of rounds, need to recode then merge
  orgwrk <- NA
  orgwrk[control_daily >= 8 & control_daily <= 10] <- 1
  orgwrk[control_daily >= 5 & control_daily <= 7] <- 2
  orgwrk[control_daily >= 2 & control_daily <= 4] <- 3
  orgwrk[control_daily >= 0 & control_daily <= 1] <- 4

  man <- NA
  man[self_employed == 0 & dp == 1 & orgwrk == 1] <- 1
  man[self_employed == 0 & dp == 1 & orgwrk > 2] <- 3
  man[self_employed == 0 & dp == 2 & orgwrk == 1] <- 2
  man[self_employed == 0 & dp == 1 & orgwrk == 2] <- 2
  man[self_employed == 0 & dp == 2 & orgwrk > 1] <- 3
  man[self_employed == 0 & dp == 3 & orgwrk == 1] <- 2
  man[self_employed == 0 & dp == 3 & orgwrk > 1] <- 3
  man[self_employed == 0 & dp == 4 & orgwrk == 1] <- 3
  man[self_employed == 0 & dp == 4 & orgwrk > 1] <- 4

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
  wr_p[pc == 6 & (control_daily >= 8 & control_daily <= 10)] <- 6
  wr_p[pc == 7 & (control_daily >= 8 & control_daily <= 10)] <- 6
  wr_p[pc == 6 & (control_daily >= 0 & control_daily <= 7)] <- 6.5
  wr_p[pc == 7 & (control_daily >= 0 & control_daily <= 7)] <- 6.5
  wr_p[pc == 7.5 & (control_daily >= 8 & control_daily <= 10)] <- 7
  wr_p[pc == 7.5 & (control_daily >= 0 & control_daily <= 7)] <- 7.5

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

  if (type == "simple") {
    if (label) {
      lookup <- c(
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

      wr_simp <- lookup[as.character(wr_simp)]
    }
    return(wr_simp)
  }

  if (type == "decision-making") {
    if (label) {
      lookup <- c(
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

      wr_dm <- lookup[as.character(wr_dm)]
    }
    return(wr_dm)
  }

  if (type == "power-class") {
    if (label) {
      lookup <- c(
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

      wr_p <- lookup[as.character(wr_p)]
    }
    return(wr_p)
  }
}

main_schema_to_others <- function(x, col_position, n_classes, schema, input_var, output_var, all_classes, label) {
  main_class <-
    multiple_cols_translator(
      x = x,
      col_position = col_position,
      output_var = input_var,
      translate_df = schema,
      translate_label_df = NULL,
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
    repair_isco = FALSE
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
