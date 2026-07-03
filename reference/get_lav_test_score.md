# Compute Score Tests for Equality Constraints

Computes score tests (Lagrange Multiplier tests) for releasing equality
constraints on specific parameters.

## Usage

``` r
get_lav_test_score(x, ind, op = c("=~", "~1", "~~", "|"))
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

A data frame containing the score test results, including the
modification index (`mi`) for each constraint.
