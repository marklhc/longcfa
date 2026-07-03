# Extract Parameter Values to Matrix

Internal helper to extract values from a lavaan object's parameter table
and organize them into a matrix based on the indicator matrix.

## Usage

``` r
par_to_mat(x, op, ind_matrix, out_col)

par_to_mat_from_pt(pt, op, ind_matrix, out_col)
```

## Arguments

- x:

  A fitted lavaan object.

- op:

  Character string specifying the operator type. One of `"=~"`, `"~1"`,
  or `"|"`.

- ind_matrix:

  Matrix defining the structure of indicators. See
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
  for details.

- out_col:

  Character string specifying the column name in the parameter table to
  extract (e.g., `"est"`, `"free"`, `"se"`).

- pt:

  Parameter table from
  [`lavaan::partable()`](https://rdrr.io/pkg/lavaan/man/parTable.html).
  When a lavaan object is passed, this will be called internally.

## Value

A matrix with dimensions matching `ind_matrix` (or stacked matrices for
thresholds) containing the extracted values.
