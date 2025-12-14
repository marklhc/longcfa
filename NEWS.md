# longcfa 0.0.1.9990

* Added penalized estimation with `penalized_est()` function
    * Support for alignment loss function (`alf()`) and L0 approximation penalty (`l0a()`)
    * Robust sandwich standard errors via `se = "robust.huber.white"` argument (experimental)
    * Uses `composite_pair_loss()` for computing pairwise penalties
* Added `plinv_search()` function for searching partial invariance models using
the score test/modification indices
* New datasets `mackinnon_etal_long` and `mackinnon_etal_wide` from MacKinnon et al. ecological momentary assessment study (CC-BY 4.0)
* Exported `par_to_mat()` and `get_lav_par_mat()` functions for converting parameter vectors to matrices
* New `get_lav_par_id()` function for extracting parameter IDs from lavaan objects
* Added `update_ustart()` function for updating user starting values in lavaan models
* Enhanced documentation with cross-references and examples

# longcfa 0.0.1

* Initial release, with support for multi-factor models, partial invariance, and categorical indicators.
