# longcfa 0.0.2


* Added penalized estimation via `penalized_longcfa()` (wrapper around `plavaan::penalized_est()`)
    * Support for alignment loss function (`alf()`) and L0 approximation penalty (`l0a()`)
    * Robust sandwich standard errors via `se = "robust.huber.white"` argument (experimental)
    * Uses `composite_pair_loss()` for computing pairwise penalties
    * New `test` argument (`"Chisq"` / `"SatorraBentler"`) enables experimental fit measures
      at the effective degrees of freedom via `lavaan::fitmeasures()` and `summary()`
      (requires a recent `plavaan` with fit-evaluation support)
    * New `plavaan_args` list to forward extra options to `plavaan`, including penalty
      continuation (`eps = "telescoping"`) and multistart (`n_starts` / `starts`, via
      `plavaan::penalized_est_multistart()`); only options the installed `plavaan`
      supports are forwarded, with a clear error otherwise
* Added `plinv_search()` function for searching partial invariance models using
the score test/modification indices, with experimental support for categorical indicators
* New datasets `mackinnon_etal_long` and `mackinnon_etal_wide` from MacKinnon et al. ecological momentary assessment study (CC-BY 4.0)
* Exported `par_to_mat()` and `get_lav_par_mat()` functions for converting parameter vectors to matrices
* New `get_lav_par_id()` function for extracting parameter IDs from lavaan objects
* Added `update_ustart()` function for updating user starting values in lavaan models
    * `longcfa()` now accepts arguments to freely estimate latent means and variances
* Enhanced documentation with cross-references and examples
* New `get_lav_lrt()` function for computing 1-df likelihood ratio tests for releasing equality constraints
* `plinv_search()` now supports false discovery rate control via `control_fdr` and `sig_level`, using the adjusted thresholds of Benjamini and Gavrilov (2009)
* `plinv_search()` now supports the `min2` argument, which stops a stage when 2 or fewer items are still tied (applies to loadings, intercepts, and thresholds)
* `get_lav_mod()` and `get_lav_test_score()` now return a `p` column alongside `mi`
* longcfa now depends on `pinsearch` (>= 0.1.6) and reuses its `fdr_alpha()` and `type2op()` functions

# longcfa 0.0.1

* Initial release, with support for multi-factor models, partial invariance, and categorical indicators.
