# Dave['s ]R things

[![Travis-CI Build Status](https://travis-ci.org/kleinschmidt/daver.svg?branch=master)](https://travis-ci.org/kleinschmidt/daver)
[![Coverage Status](https://img.shields.io/codecov/c/github/kleinschmidt/daver/master.svg)](https://codecov.io/github/kleinschmidt/daver?branch=master)


Use at your own risk.  Some of these are documented, others are tested, none of
them very well.  Please yell at me (open an issue) if something is broken or
could be improved.

* Formatting $p$-values
    * `p_val_to_less_than`
    * `p_val_to_stars`
* Working with "tidy" probabilities in (grouped) data frames: 
    * `marginalize[_log]`
    * `aggregate[_log]_likelihood`
    * `normalize[_log]_probability`
* Bootstrapped confidence intervals with less pain (`boot_ci`)
* Pasting things
    * `paste_factors` (preserving ordering of levels)
    * `paste_with_last`
    * `paste_and`
* Numerically stable sum and mean of log-probabilities (etc.)
    * `log_sum_exp`
    * `log_mean_exp`
* Misc.
    * `p_val_to_two_tail`
    * `se`
