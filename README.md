# socialclasses

Implementations:

* All ISCO equivalents

- [X] ISCO68 to ISCO88
- [X] ISCO68 to ISCO08 # If this one is done then ISCO08 -> ISCO68 should be done
- [X] ISCO68 to ISEI
- [X] ISCO68 to SIOPS
- [X] ISCO68 to EGP
- [X] ISCO68 to EGP11

- [X] ISCO88 to ISEI
- [X] ISCO88 to SIOPS
- [X] ISCO88 to MPS88
- [X] ISCO88 to EGP
- [X] ISCO88 to EGP11
- [X] ISCO88 to OESCH16
- [X] ISCO88 to ISCO88COM
- [X] ISCO88 to ISCO08
- [X] ISCO88 to ISCO68

- [X] ISCO08 to ISEI
- [X] ISCO08 to SIOPS
- [X] ISCO08 to OESCH16
- [ ] ISCO08 to ISCO68 (not available)
- [X] ISCO08 to ISCO88


TODO:
- Oesch schemas have `.` inside the classes, what to do with those? We get NAs introduced when converting to numeric.
- Oesch seems to have other labels which are shorter but no requivalence
- From parsed txt file only ESEC missing
- Need to make sure which ESEC you parsed from txt matches the ones in their doc. They want more ESEC so you need to map them and add additional ESEC.
- Add recoding of major/minor codes for all ISCO.
- Adapt E.O wright schema
- Ask to them provide three datasets
- Move all functions to package and document each one
- Create vignette examples with already existing codifications
- Plan meeting with them to show current work
- Improve docs on each translation maybe mentioning what each  class schemas is and pointing to the source and the website.

## Steps to add a new translation

1. Add two csv files respectively in `data-raw/social_classes/labels/` and `data-raw/social_classes/translation/` containing the labels and translation for the two schemas.

2. Run the script `data-raw/social_classes.R` (with the root directory in `data-raw/`)

3. Add a new function inside `R/` with the convention `{origin}_to_{destination}()` where origin and destination are the class schemas we're translating. Please have a look at other translation to recycle common functions to do translations.

4. Add proper documentation to the function
