# 0.0.1 DIGCLASS

* Release of the package

# 0.0.2 DIGCLASS

* All related MSEC functions replaced the current MSEC version for a new one. The legacy MSEC is also stored as a CSV file and a new argument has been added called `proto` to access the prototype version of the earlier versions.

# 0.0.3 DIGCLASS

* Bugs related to this:

(1) Military ISCO-codes were inconsistent with the ISCO-logic (because they consist in 2-digit codes at the 3-digit level and in 3-digit codes at the 4-digit level of ISCO).  
(2) Higher ISCOs codes were not filled with the codes of lower ISCO-levels (say: ISCO-1 code 6 becomes ISCO-2 code 60, becomes ISCO-3 code 600, becomes ISCO-4 code 6000).

have ben fixed in the latest ISCO08/88 to OEP08/88. The translation files have been updated and fixed.