# Compute 1-df Likelihood Ratio Tests for Equality Constraints

Computes 1-df likelihood ratio tests (LRTs) for releasing equality
constraints on specific parameters, where each tied constraint is
released individually and the model is refit.

## Usage

``` r
get_lav_lrt(x, ind, op = c("=~", "~1", "~~", "|"))
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

A data frame with one row for each tied equality constraint, containing
`id`, `lhs`, `op`, `rhs`, `group`, `plabel`, the 1-df LRT statistic for
releasing the constraint (`mi`), and its p-value (`p`). For `op = "~1"`
and `op = "|"`, `rhs` is empty and the variable is in `lhs`. A data
frame with 0 rows (and the same column names) is returned when no
candidate is tied.

## Examples

``` r
library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
# Indicator matrix
spec <- matrix(c(
    "y1", "y2", "y3", "y4",
    "y5", "y6", "y7", "y8"
), ncol = 2)
# Scalar invariance
fit <- longcfa(spec,
               lv_names = c("dem60", "dem65"),
               data = PoliticalDemocracy,
               long_equal = c("loadings", "intercepts"))
# 1-df LRTs for releasing each tied intercept constraint
get_lav_lrt(fit, ind = paste0("y", 1:8), op = "~1")
#>    id lhs op rhs group plabel        mi          p
#> 17 17  y5 ~1         1  .p17. 0.2716331 0.60223840
#> 18 18  y6 ~1         1  .p18. 4.1504962 0.04162232
#> 19 19  y7 ~1         1  .p19. 0.2558715 0.61297077
#> 20 20  y8 ~1         1  .p20. 0.6129545 0.43367755
```
