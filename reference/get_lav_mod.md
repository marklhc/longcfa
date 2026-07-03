# Compute Modification Indices for Specific Parameters

Computes modification indices for specific parameters restricted by
indicator names and operator type.

## Usage

``` r
get_lav_mod(x, ind, op = c("=~", "~1", "~~", "|"))
```

## Arguments

- x:

  A fitted lavaan object.

- ind:

  Character vector of indicator names to consider.

- op:

  Character string specifying the operator type (`"=~"`, `"~1"`, `"~~"`,
  or `"|"`).

## Value

A data frame containing the modification indices.
