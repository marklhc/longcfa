# Penalized Longitudinal CFA with Alignment-Style Penalties

A wrapper function that performs penalized estimation for longitudinal
CFA models by applying penalties to differences in loadings, intercepts,
and residual variances across time points. This function internally
generates a configural invariance model with free latent means and
variances, then applies penalized estimation.

## Usage

``` r
penalized_longcfa(
  ind_matrix,
  lv_names = NULL,
  data = NULL,
  sample.cov = NULL,
  sample.mean = NULL,
  sample.nobs = NULL,
  w = 0.1,
  pen_fn = "l0a",
  pen_params = c("loadings", "intercepts"),
  se = "none",
  opt_control = list(),
  test = "none",
  plavaan_args = list(),
  ...
)
```

## Arguments

- ind_matrix:

  A character matrix where each column represents a time point and each
  row represents an indicator variable. Column names should be time
  point labels, and cell values should be variable names in the data.

- lv_names:

  Character vector of latent variable names for each time point. If
  NULL, default names will be generated.

- data:

  A data frame containing the observed variables. Optional if
  `sample.cov` is provided.

- sample.cov:

  A numeric covariance matrix. Optional if `data` is provided.

- sample.mean:

  A numeric vector of means. Optional if `data` is provided.

- sample.nobs:

  Numeric scalar. The number of observations. Required if using summary
  statistics instead of raw data.

- w:

  Numeric scalar. Penalty weight applied to the penalty terms. Default
  is 0.1.

- pen_fn:

  Character string specifying the penalty function. Options are `"l0a"`
  (default) or `"alf"`.

- pen_params:

  Character vector specifying which parameter types to penalize. Options
  are `"loadings"`, `"intercepts"`, and `"residuals"`. Default is
  `c("loadings", "intercepts")`.

- se:

  Character string specifying the type of standard errors. Default is
  `"none"`. See
  [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
  for options.

- opt_control:

  A list of control parameters passed to
  [`stats::nlminb()`](https://rdrr.io/r/stats/nlminb.html). See
  [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
  for defaults.

- test:

  Character string specifying the model test to compute. Options are
  `"none"` (default), `"Chisq"`, and `"SatorraBentler"`. Fit measures
  ([`lavaan::fitmeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html))
  and the chi-square test in
  [`summary()`](https://rdrr.io/r/base/summary.html) are only available
  when `test` is not `"none"`; they are evaluated at the effective
  degrees of freedom (see `plavaan::effective_df()`). Fit evaluation is
  experimental and requires a `plavaan` build with fit-evaluation
  support; on older builds a non-`"none"` `test` will error. See
  [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
  for details.

- plavaan_args:

  A named list of additional arguments forwarded to the underlying
  `plavaan` estimator
  ([`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html),
  or
  [`plavaan::penalized_est_multistart()`](https://marklhc.github.io/plavaan/reference/penalized_est_multistart.html)
  when multistart is requested). This is an escape hatch for options not
  exposed as dedicated arguments, for example:

  - `eps` / `telescoping_control` — smoothing and continuation control
    for the built-in penalties (e.g. `eps = "telescoping"`).

  - `n_starts`, `starts`, `keep_all`, `verbose` — multistart control.
    Supplying `starts` (or `n_starts > 1`) switches to
    [`plavaan::penalized_est_multistart()`](https://marklhc.github.io/plavaan/reference/penalized_est_multistart.html).

  - `start` — custom starting values for a single-start fit.

  Only arguments accepted by the installed `plavaan` build are
  forwarded; an option the build does not support produces an error
  suggesting an update. Options that have a dedicated argument (`w`,
  `pen_fn`, `se`, `opt_control`, `test`) must be set via that argument,
  not through `plavaan_args`. See
  [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
  and
  [`plavaan::penalized_est_multistart()`](https://marklhc.github.io/plavaan/reference/penalized_est_multistart.html)
  for the full set of options.

- ...:

  Additional arguments passed to
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md).

## Value

A lavaan model object with penalized parameter estimates. See
[`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
for details on interpretation.

## Details

This function simplifies the workflow for penalized longitudinal CFA by:

1.  Generating a configural invariance model syntax with free latent
    means and variances

2.  Identifying the relevant parameter IDs for loadings, intercepts, and
    residuals

3.  Applying
    [`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html)
    with penalties on pairwise differences

The penalty is applied to differences between corresponding parameters
at different time points, encouraging approximate measurement
invariance.

**Fit measures:** Setting `test` to `"Chisq"` or `"SatorraBentler"`
enables
[`lavaan::fitmeasures()`](https://rdrr.io/pkg/lavaan/man/fitMeasures.html)
and the chi-square test in
[`summary()`](https://rdrr.io/r/base/summary.html), which are computed
at the effective degrees of freedom. This relies on the experimental
fit-evaluation support in `plavaan`.

**Multistart and penalty continuation:** Non-convex penalties (`l0a`,
`alf`) can have local optima. Set `plavaan_args = list(n_starts = k)`
(or supply `starts`) to run
[`plavaan::penalized_est_multistart()`](https://marklhc.github.io/plavaan/reference/penalized_est_multistart.html)
and keep the best solution, or
`plavaan_args = list(eps = "telescoping")` to fit a continuation
sequence of decreasing penalty smoothing. Fit measures (`test`) are not
available together with multistart.

**Note:** If using summary statistics (`sample.cov`, `sample.mean`,
`sample.nobs`), ordered/categorical items cannot be automatically
handled because threshold counts must be derived from raw data.

## See also

[`plavaan::penalized_est()`](https://marklhc.github.io/plavaan/reference/penalized_est.html),
[`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md),
[`longcfa_syntax()`](https://marklhc.github.io/longcfa/reference/longcfa.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(lavaan)

# Prepare indicator matrix
ind_mat <- cbind(c("y1", "y2", "y3", "y4"), c("y5", "y6", "y7", "y8"))

# Fit penalized longitudinal CFA with raw data
pen_fit <- penalized_longcfa(
    ind_matrix = ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    w = 0.1,
    pen_fn = "alf"
)

# Fit measures are available when a model test is enabled (experimental)
pen_fit_test <- penalized_longcfa(
    ind_matrix = ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    w = 0.1,
    test = "Chisq"
)
lavaan::fitmeasures(pen_fit_test, c("chisq", "df", "cfi", "rmsea", "srmr"))

# Penalty continuation ("telescoping") and multistart via plavaan_args
pen_fit_tele <- penalized_longcfa(
    ind_matrix = ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    w = 0.1,
    plavaan_args = list(eps = "telescoping")
)
set.seed(1)
pen_fit_ms <- penalized_longcfa(
    ind_matrix = ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    w = 0.1,
    plavaan_args = list(n_starts = 10)
)
attr(pen_fit_ms, "multistart")
# Fit penalized longitudinal CFA with summary statistics
pen_fit_stat <- penalized_longcfa(
    ind_matrix = ind_mat,
    lv_names = c("dem60", "dem65"),
    sample.cov = my_cov,
    sample.mean = my_means,
    sample.nobs = 500,
    w = 0.1,
    pen_fn = "alf"
)

# Compare with scalar invariance model
fit_scalar <- longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    long_equal = c("loadings", "intercepts")
)

cbind(
    penalized = coef(pen_fit),
    scalar = coef(fit_scalar)
)
} # }
```
