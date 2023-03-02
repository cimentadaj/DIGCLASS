
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DIGCLASS

<!-- badges: start -->
<!-- badges: end -->

The `DIGCLASS` R package aims to make translation between occupational
social classes easy and comprehensive. It facilitates the translation of
the International Standard Classification of Occupations (ISCO) from
1968, 1988 and 2008 to a wide range of social class schemes.

This package is a work in progress and has implemented currently these
translations:

- **ISCO68**

- [x] ISCO68 to ISCO88

- [x] ISCO68 to ISCO08 \# If this one is done then ISCO08 -\> ISCO68
  should be done

- [x] ISCO68 to ISEI

- [x] ISCO68 to SIOPS

- [x] ISCO68 to EGP

- [x] ISCO68 to EGP11

- **ISCO88**

- [x] ISCO88 to ISEI

- [x] ISCO88 to SIOPS

- [x] ISCO88 to MPS88

- [x] ISCO88 to EGP

- [x] ISCO88 to EGP11

- [x] ISCO88 to OESCH16

- [x] ISCO88 to ISCO88COM

- [x] ISCO88 to ISCO08

- [x] ISCO88 to ISCO68

- **ISCO08**

- [x] ISCO08 to ISEI

- [x] ISCO08 to SIOPS

- [x] ISCO08 to OESCH16

- [ ] ISCO08 to ISCO68 (not available)

- [x] ISCO08 to ISCO88

## Installation

You can install the development version of DIGCLASS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/socialclasses")
```

## Example

Here’s an example of translating ISCO68 to ISEI and EGP class schemas:

``` r
library(DIGCLASS)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Internal data for the European Social Survey round 5
# containing different ISCO variables
ess %>%
  mutate(
    isei = isco68_to_isei(isco68),
    egp = isco68_to_egp(isco68, self_employed, emplno),
    egp_labels = isco68_to_egp(isco68, self_employed, emplno, label = TRUE)
  )
#> # A tibble: 56,752 × 8
#>    isco68 isco88 isco08 emplno self_employed  isei   egp egp_labels            
#>     <dbl>  <dbl>  <dbl>  <dbl>         <dbl> <dbl> <dbl> <chr>                 
#>  1    140   3111     NA      0             0    NA    NA <NA>                  
#>  2   3210   3431   4120      0             0    55     3 'routine nonmanual'   
#>  3   2119   1210   1120      0             0    69     1 'higher controllers'  
#>  4   1320   2320   2330      0             0    71     2 'lo controllers'      
#>  5   8550   7137   7411      0             0    40     8 'skilled manual'      
#>  6   5401   5131   5311      0             0    24     9 'semi-unskilld manual'
#>  7     NA     NA     NA      0             0    NA    NA <NA>                  
#>  8   1593   2419   2432      0             0    66     2 'lo controllers'      
#>  9   1930   3460   2635      0             0    54     2 'lo controllers'      
#> 10   1950   2444   2643      0             0    54     2 'lo controllers'      
#> # … with 56,742 more rows
```

## Steps to add a new translation

1.  Add two csv files respectively in `data-raw/social_classes/labels/`
    and `data-raw/social_classes/translation/` containing the labels and
    translation for the two schemas.

2.  Run the script `data-raw/social_classes.R` (with the root directory
    in `data-raw/`)

3.  Add a new function inside `R/` with the convention
    `{origin}_to_{destination}()` where origin and destination are the
    class schemas we’re translating. Please have a look at other
    translation to recycle common functions to do translations.

4.  Add proper documentation to the function

## Other R packages

This package has benefitted greatly from other open source packages that
already pave the way for translation between social class schemas. In
particular, we’ve learned a lot and borrowed code from all of these
packages:

• [ISCOGEN](https://github.com/benjann/iscogen): Stata package •
[SocialPosition](https://cran.r-project.org/web/packages/SocialPosition/index.html):
R package • [occupar](https://github.com/DiogoFerrari/occupar/): R
package

## TODO:

- Oesch schemas have `.` inside the classes, what to do with those? We
  get NAs introduced when converting to numeric.
- Oesch seems to have other labels which are shorter but no requivalence
- From parsed txt file only ESEC missing
- Need to make sure which ESEC you parsed from txt matches the ones in
  their doc. They want more ESEC so you need to map them and add
  additional ESEC.
- Add recoding of major/minor codes for all ISCO.
- Adapt E.O wright schema
- Create vignette examples with already existing codifications
- Plan meeting with them to show current work
- Improve docs on each translation maybe mentioning what each class
  schemas is and pointing to the source and the website.
