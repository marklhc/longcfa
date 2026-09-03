# Changelog

## longcfa 0.0.2

- Added penalized estimation via
  [`penalized_longcfa()`](https://marklhc.github.io/longcfa/reference/penalized_longcfa.md)
  (wrapper around
  [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html))
  - Support for alignment loss function
    ([`alf()`](https://marklhc.github.io/plavaan/reference/loss.html))
    and L0 approximation penalty
    ([`l0a()`](https://marklhc.github.io/plavaan/reference/loss.html))
  - Robust sandwich standard errors via `se = "robust.huber.white"`
    argument (experimental)
  - Uses
    [`composite_pair_loss()`](https://marklhc.github.io/plavaan/reference/composite_pair_loss.html)
    for computing pairwise penalties
  - New `test` argument (`"Chisq"` / `"SatorraBentler"`) enables
    experimental fit measures at the effective degrees of freedom via
    [`lavaan::fitmeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
    and [`summary()`](https://rdrr.io/r/base/summary.html) (requires a
    recent `plavaan` with fit-evaluation support)
  - New `plavaan_args` list to forward extra options to `plavaan`,
    including penalty continuation (`eps = "telescoping"`) and
    multistart (`n_starts` / `starts`, via
    [`plavaan::penalized_est_multistart()`](https://marklhc.github.io/plavaan/reference/penalized_est_multistart.html));
    only options the installed `plavaan` supports are forwarded, with a
    clear error otherwise
- Added
  [`plinv_search()`](https://marklhc.github.io/longcfa/reference/plinv_search.md)
  function for searching partial invariance models using the score
  test/modification indices, with experimental support for categorical
  indicators
- New datasets `mackinnon_etal_long` and `mackinnon_etal_wide` from
  MacKinnon et al. ecological momentary assessment study (CC-BY 4.0)
- Exported
  [`par_to_mat()`](https://marklhc.github.io/longcfa/reference/par_to_mat.md)
  and
  [`get_lav_par_mat()`](https://marklhc.github.io/longcfa/reference/get_lav_par_mat.md)
  functions for converting parameter vectors to matrices
- New
  [`get_lav_par_id()`](https://marklhc.github.io/longcfa/reference/get_lav_par_mat.md)
  function for extracting parameter IDs from lavaan objects
- Added
  [`update_ustart()`](https://marklhc.github.io/longcfa/reference/update_ustart.md)
  function for updating user starting values in lavaan models
  - [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
    now accepts arguments to freely estimate latent means and variances
- Enhanced documentation with cross-references and examples

## longcfa 0.0.1

- Initial release, with support for multi-factor models, partial
  invariance, and categorical indicators.
