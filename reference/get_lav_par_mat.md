# Extract Parameter Matrix from lavaan Object

Extracts parameter estimates from a fitted lavaan object and organizes
them into a matrix based on the indicator matrix structure.

## Usage

``` r
get_lav_par_mat(x, op = c("=~", "~1", "|"), ind_matrix)

get_lav_par_id(x, op = c("=~", "~1", "|"), ind_matrix)
```

## Arguments

- x:

  A fitted lavaan object.

- op:

  Character string specifying the operator type. Either `"=~"`
  (loadings) or `"~1"` (intercepts). Defaults to `"=~"`.

- ind_matrix:

  Matrix defining the structure of indicators. See
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
  for details on the structure.

## Value

A matrix containing the estimated parameters (or IDs) from the lavaan
object, organized according to `ind_matrix`.

## Details

`get_lav_par_id()` extracts the parameter IDs from a fitted lavaan
object, based on the specified operator and indicator matrix.
