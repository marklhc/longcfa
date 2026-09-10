# Specification Search for Partial Invariance

Performs a specification search for partial longitudinal invariance by
iteratively freeing parameters with high modification indices or score
test statistics, or other user-defined criteria.

## Usage

``` r
plinv_search(
  ind_matrix,
  lv_names,
  data,
  type,
  mi_fun,
  mi_min = 3.84,
  control_fdr = FALSE,
  sig_level = 0.05,
  min2 = FALSE,
  ...
)
```

## Arguments

- ind_matrix:

  Matrix defining the structure of indicators. See
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
  for details on the structure.

- lv_names:

  A character vector of names for the latent variables.

- data:

  A data frame containing the observed variables.

- type:

  A character vector specifying the types of parameters to search for
  partial invariance. Supported types are `"loadings"`, `"intercepts"`,
  `"thresholds"`, `"residuals"`, and `"residual.covariances"`. The
  search is performed sequentially in the order specified.

- mi_fun:

  A function to compute modification indices or score tests. Common
  choices are
  [`get_lav_test_score()`](https://marklhc.github.io/longcfa/reference/get_lav_test_score.md)
  (for score tests on equality constraints) or
  [`get_lav_mod()`](https://marklhc.github.io/longcfa/reference/get_lav_mod.md)
  (for modification indices), but other functions can be used as long as
  they return a similar data frame with columns of `lhs`, `op`, `rhs`,
  `id`, and `plabel` as defined in
  [`lavaan::parTable()`](https://rdrr.io/pkg/lavaan/man/parTable.html),
  and a column of `mi`.

- mi_min:

  A numeric value specifying the minimum threshold for the modification
  index or score test statistic to free a parameter. Default is `3.84`
  (the 1-df chi-squared value for p = .05). Ignored when
  `control_fdr = TRUE`.

- control_fdr:

  Logical; whether to control the false discovery rate for multiple
  testing. If `TRUE`, instead of a fixed `mi_min`, the k-th freed
  parameter at each stage must have a test statistic above a Benjamini
  and Gavrilov (2009) adjusted threshold, computed as
  `qchisq(pinsearch::fdr_alpha(k, m, q = sig_level), 1, lower.tail = FALSE)`,
  where `m` is the number of tied candidates at the start of the stage.

- sig_level:

  Significance level (target false discovery rate) used when
  `control_fdr = TRUE`. Default is .05.

- min2:

  Logical; whether to stop a stage when 2 or fewer items are still tied
  for the parameter type, analogous to `min2` in
  [`pinsearch::pinSearch()`](https://marklhc.github.io/pinsearch/reference/pinSearch.html).
  Applies only to `type` values `"loadings"`, `"intercepts"`, and
  `"thresholds"`.

- ...:

  Additional arguments passed to
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md).

## Value

A list containing:

- fit:

  The final fitted lavaan object with partial invariance constraints.

- traces:

  A data frame tracking the parameters freed during the search process,
  with one row per freed parameter and the columns returned by `mi_fun`
  (including the test statistic `mi` and the p-value `p` for the
  built-in candidates).

## References

Benjamini, Y. & Gavrilov, N. M. (2009). Sequential selection procedures
for testing dependent hypotheses.

## See also

[`pinsearch::pinSearch()`](https://marklhc.github.io/pinsearch/reference/pinSearch.html)
for cross-sectional (multi-group) specification search.
