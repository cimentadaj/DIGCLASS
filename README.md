# socialclasses

Implementations:

* All ISCO equivalents

- [X] ISCO68 to ISCO88
- [X] ISCO68 to ISCO08
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
