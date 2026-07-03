# Update User Starting Values for Specific Parameters by ID

This function updates the user starting values, or the constrained
values, for specific parameters in a lavaan object and refits the model.

## Usage

``` r
update_ustart(x, par_id, new_start, ...)
```

## Arguments

- x:

  A fitted lavaan object.

- par_id:

  Integer vector of parameter IDs ("id" column in the parameter table)
  to update.

- new_start:

  Numeric vector of new starting values corresponding to `par_id`.

- ...:

  Additional arguments passed to the
  [`update()`](https://rdrr.io/r/stats/update.html) method for lavaan
  objects.

## Value

A refitted lavaan object with updated starting values.

## Examples

``` r
library(lavaan)
# Fit a simple CFA model
HS.model <- 'visual =~ x1 + x2 + x3'
fit <- cfa(HS.model, data = HolzingerSwineford1939)

# View parameter table to identify parameter IDs
lavaan::partable(fit)
#>   id    lhs op    rhs user block group free ustart exo label plabel start   est
#> 1  1 visual =~     x1    1     1     1    0      1   0         .p1. 1.000 1.000
#> 2  2 visual =~     x2    1     1     1    1     NA   0         .p2. 0.778 0.778
#> 3  3 visual =~     x3    1     1     1    2     NA   0         .p3. 1.107 1.107
#> 4  4     x1 ~~     x1    0     1     1    3     NA   0         .p4. 0.679 0.835
#> 5  5     x2 ~~     x2    0     1     1    4     NA   0         .p5. 0.691 1.065
#> 6  6     x3 ~~     x3    0     1     1    5     NA   0         .p6. 0.637 0.633
#> 7  7 visual ~~ visual    0     1     1    6     NA   0         .p7. 0.050 0.524
#>      se
#> 1 0.000
#> 2 0.141
#> 3 0.214
#> 4 0.118
#> 5 0.105
#> 6 0.129
#> 7 0.130

# Update value for the first loading from 1 to 1.5
fit_updated <- update_ustart(fit, par_id = 1, new_start = 1.5)
lavaan::coef(fit_updated)
#>     visual=~x2     visual=~x3         x1~~x1         x2~~x2         x3~~x3 
#>          1.167          1.661          0.835          1.065          0.633 
#> visual~~visual 
#>          0.233 
```
