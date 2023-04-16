---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->




# DIGCLASS

<!-- badges: start -->
[![R-CMD-check](https://github.com/cimentadaj/socialclasses/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cimentadaj/socialclasses/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `DIGCLASS` R package aims to make translation between occupational social classes easy and comprehensive. It facilitates the translation of the International Standard Classification of Occupations (ISCO) from 1968, 1988 and 2008 to a wide range of social class schemes.

This package is a work in progress and has implemented currently these translations:

* **ISCO68**
  - [X] ISCO68 to ISCO88
  - [X] ISCO68 to ISCO08
  - [X] ISCO68 to SIOPS
  - [X] ISCO68 to ISEI
  - [X] ISCO68 to EGP
  - [X] ISCO68 to EGP11

* **ISCO88**
  - [X] ISCO88 to ISEI
  - [X] ISCO88 to SIOPS
  - [X] ISCO88 to MPS88
  - [X] ISCO88 to EGP
  - [X] ISCO88 to EGP11
  - [X] ISCO88 to OESCH16
  - [X] ISCO88 to ISCO88COM
  - [X] ISCO88 to ISCO08
  - [X] ISCO88 to ISCO68

* **ISCO08**
  - [X] ISCO08 to ISCO88
  - [X] ISCO08 to ISEI
  - [X] ISCO08 to SIOPS
  - [X] ISCO08 to OESCH16

* - [X] Translation between major/submajor/minor/unit groups for ISCO68, ISCO88 and ISCO08.

## Installation

You can install the development version of DIGCLASS from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/socialclasses")
```

## Example

Here's an example of translating ISCO68 to ISEI and EGP class schemas:


```r
library(DIGCLASS)
library(dplyr)

# Internal data for the European Social Survey round 5
# containing different ISCO variables
ess %>%
  mutate(
    isei = isco68_to_isei(isco68),
    egp = isco68_to_egp(isco68, self_employed, emplno),
    egp_labels = isco68_to_egp(isco68, self_employed, emplno, label = TRUE)
  )
#> # A tibble: 56,752 × 8
#>    isco68 isco88 isco08 emplno self_employed isei  egp   egp_labels            
#>    <chr>  <chr>  <chr>   <dbl>         <dbl> <chr> <chr> <chr>                 
#>  1 0140   3111   3111        0             0 47    2     'lo controllers'      
#>  2 3210   3431   4120        0             0 55    3     'routine nonmanual'   
#>  3 2119   1210   1120        0             0 69    1     'higher controllers'  
#>  4 1320   2320   2330        0             0 71    2     'lo controllers'      
#>  5 8550   7137   7411        0             0 40    8     'skilled manual'      
#>  6 5401   5131   5311        0             0 24    9     'semi-unskilld manual'
#>  7 <NA>   <NA>   <NA>        0             0 <NA>  <NA>  <NA>                  
#>  8 1593   2419   2432        0             0 66    2     'lo controllers'      
#>  9 1930   3460   2635        0             0 54    2     'lo controllers'      
#> 10 1950   2444   2643        0             0 54    2     'lo controllers'      
#> # ℹ 56,742 more rows
```

The nomenclature of the function is `{origin}_to_{destination}` where `origin` is the origin class schema and `destination` is the destination class schema. The usual workflow is for you to type, for example `isco` and then hit `TAB` to get auto-completion on all possible translations.

For those class schemas that have labels, the `label` argument returns the labels instead of the class codes.

## Steps to add a new translation

1. Add two csv files respectively in `data-raw/social_classes/labels/` and `data-raw/social_classes/translation/` containing the labels and translation for the two schemas.

2. Run the script `data-raw/social_classes.R` (with the root directory in `data-raw/`)

3. Add a new function inside `R/` with the convention `{origin}_to_{destination}()` where origin and destination are the class schemas we're translating. Please have a look at other translation to recycle common functions to do translations.

4. Add proper documentation to the function


## Other R packages

This package has benefitted greatly from other open source packages that already pave the way for translation between social class schemas. In particular, we've learned a lot and borrowed code from all of these packages:

- [ISCOGEN](https://github.com/benjann/iscogen): Stata package
- [SocialPosition](https://cran.r-project.org/web/packages/SocialPosition/index.html): R package
- [occupar](https://github.com/DiogoFerrari/occupar/): R package

## TODO:

- [] Oesch schemas have `.` inside the classes, what to do with those? We get NAs introduced when converting to numeric.
- [] Oesch seems to have other labels which are shorter but no requivalence
- [] From parsed txt file only ESEC missing
- [] Need to make sure which ESEC you parsed from txt matches the ones in their doc. They want more ESEC so you need to map them and add additional ESEC.
- [X] Add recoding of major/minor codes for all ISCO.
- [] See if EU-SILC contains all variables needed to construct the E.O Wright schema. If yes, adapt E.O Wright schema
- [X] Create vignette examples with already existing codifications
- [] Plan meeting with them to show current work
- [] Improve docs on each translation maybe mentioning what each  class schemas is and pointing to the source and the website.
- [X] Improve vignettes after implementing major/minor codes
- [X] Create a repair function to complete 4, 3, 2 and 1 digits
- [X] Stop if it's not a character
- [X] Warning if stuff is not 4 digits
- [] Apply repair_isco on all isco variables
- [X] Examples of reading the data as character vector directly from read_csv / read_dta
- [] Examples with ISSP
- [] Proofread README to make sure there aren't any grammatical errors
