# longcfa 0.0.1.9000

* Penalized estimation with the `penalized_est()` function
    * with support of the alignment loss function (`alf()`) and L0 approximation penalty (`l0a()`), via the `composite_pair_loss()` function.
* New data sets `mackinnon_etal_long` and `mackinnon_etal_wide` from MacKinnon et al. ecological momentary assessment study, who made their data publicly available.
* New `par_to_mat()` and `get_lav_par_mat()` functions for converting parameter vectors to matrices, for use with alignment optimization.
* New `update_ustart()` function for updating user starting values in lavaan models.
* New vignettes on alignment optimization and penalized estimation.

# longcfa 0.0.1

* Initial release, with support for multi-factor models, partial invariance, and categorical indicators.
