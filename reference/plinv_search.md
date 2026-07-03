# Specification Search for Partial Invariance

Performs a specification search for partial longitudinal invariance by
iteratively freeing parameters with high modification indices or score
test statistics, or other user-defined criteria.

## Usage

``` r
plinv_search(ind_matrix, lv_names, data, type, mi_fun, mi_min, ...)
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
  index or score test statistic to free a parameter.

- ...:

  Additional arguments passed to
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md).

## Value

A list containing:

- fit:

  The final fitted lavaan object with partial invariance constraints.

- traces:

  A data frame tracking the parameters freed during the search process.
