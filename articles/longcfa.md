# Quick Start

``` r

library(longcfa)
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
```

The data is the `PoliticalDemocracy` dataset from the `lavaan` package,
which is a longitudinal dataset on political democracy scores across
multiple countries over several years. One can read the
[documentation](https://rdrr.io/cran/lavaan/man/PoliticalDemocracy.html)
for more details.

``` r

data("PoliticalDemocracy", package = "lavaan")
head(PoliticalDemocracy)
#>      y1       y2       y3       y4       y5       y6       y7       y8       x1
#> 1  2.50 0.000000 3.333333 0.000000 1.250000 0.000000 3.726360 3.333333 4.442651
#> 2  1.25 0.000000 3.333333 0.000000 6.250000 1.100000 6.666666 0.736999 5.384495
#> 3  7.50 8.800000 9.999998 9.199991 8.750000 8.094061 9.999998 8.211809 5.961005
#> 4  8.90 8.800000 9.999998 9.199991 8.907948 8.127979 9.999998 4.615086 6.285998
#> 5 10.00 3.333333 9.999998 6.666666 7.500000 3.333333 9.999998 6.666666 5.863631
#> 6  7.50 3.333333 6.666666 6.666666 6.250000 1.100000 6.666666 0.368500 5.533389
#>         x2       x3
#> 1 3.637586 2.557615
#> 2 5.062595 3.568079
#> 3 6.255750 5.224433
#> 4 7.567863 6.267495
#> 5 6.818924 4.573679
#> 6 5.135798 3.892270
```

In this data set, `y1` to `y4` are indicators of political democracy in
1960, and `y5` to `y8` are the same indicators in 1965. Using `lavaan`,
one can specify a longitudinal confirmatory factor analysis (CFA) model
as follows:

``` r

# Configural invariance: no constraints across time
lconfig_mod <- "
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
"
lconfig_fit <- cfa(lconfig_mod, data = PoliticalDemocracy, std.lv = TRUE)
lconfig_fit
#> lavaan 0.6-21 ended normally after 19 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        17
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                45.070
#>   Degrees of freedom                                19
#>   P-value (Chi-square)                           0.001
```

Because the same indicators are used at both time points, what is unique
for an indicator at one time point may remain at the other time point.
We can allow covariances between the unique factors of the same
indicators across time points:

``` r

lconfig2_mod <- "
  dem60 =~ y1 + y2 + y3 + y4
  dem65 =~ y5 + y6 + y7 + y8
  y1 ~~ y5
  y2 ~~ y6
  y3 ~~ y7
  y4 ~~ y8
"
lconfig2_fit <- cfa(lconfig2_mod, data = PoliticalDemocracy, std.lv = TRUE)
lconfig2_fit
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                23.841
#>   Degrees of freedom                                15
#>   P-value (Chi-square)                           0.068
```

The `longcfa` approach uses an input matrix to specify the indicators in
rows, and the time points in columns. In this case, with four indicators
measured at two time points, the input matrix would look like this:

``` r

ind_mat <- matrix(
    c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
    nrow = 4
)
ind_mat
#>      [,1] [,2]
#> [1,] "y1" "y5"
#> [2,] "y2" "y6"
#> [3,] "y3" "y7"
#> [4,] "y4" "y8"
```

and the model `lconfig2_mod` can be specified as follows, with the
`lag_cov = TRUE` argument indicating that we want to allow covariances
between the unique factors of the same indicators across time points:

``` r

lconfig2_fit2 <- longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    lag_cov = TRUE
)
summary(lconfig2_fit2)
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        29
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                23.841
#>   Degrees of freedom                                15
#>   P-value (Chi-square)                           0.068
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1      (.1_1)    2.133    0.252    8.473    0.000
#>     y2      (.2_1)    2.989    0.396    7.555    0.000
#>     y3      (.3_1)    2.264    0.341    6.649    0.000
#>     y4      (.4_1)    2.925    0.314    9.301    0.000
#>   dem65 =~                                            
#>     y5      (.1_2)    1.957    0.261    7.500    0.000
#>     y6      (.2_2)    2.689    0.330    8.156    0.000
#>     y7      (.3_2)    2.691    0.318    8.467    0.000
#>     y8      (.4_2)    2.817    0.305    9.252    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.878    0.374    2.349    0.019
#>  .y2 ~~                                               
#>    .y6                1.776    0.751    2.365    0.018
#>  .y3 ~~                                               
#>    .y7                1.309    0.627    2.089    0.037
#>  .y4 ~~                                               
#>    .y8                0.265    0.475    0.558    0.577
#>   dem60 ~~                                            
#>     dem65             0.941    0.028   34.136    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    5.465    0.299   18.269    0.000
#>    .y2      (.2_1)    4.256    0.453    9.394    0.000
#>    .y3      (.3_1)    6.563    0.376   17.449    0.000
#>    .y4      (.4_1)    4.453    0.385   11.579    0.000
#>    .y5      (.1_2)    5.136    0.298   17.211    0.000
#>    .y6      (.2_2)    2.978    0.387    7.690    0.000
#>    .y7      (.3_2)    6.196    0.378   16.387    0.000
#>    .y8      (.4_2)    4.043    0.372   10.860    0.000
#>     dem60             0.000                           
#>     dem65             0.000                           
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    2.160    0.454    4.753    0.000
#>    .y2      (.2_1)    6.460    1.234    5.235    0.000
#>    .y3      (.3_1)    5.483    0.994    5.519    0.000
#>    .y4      (.4_1)    2.536    0.670    3.786    0.000
#>    .y5      (.1_2)    2.849    0.538    5.294    0.000
#>    .y6      (.2_2)    4.020    0.800    5.025    0.000
#>    .y7      (.3_2)    3.479    0.718    4.845    0.000
#>    .y8      (.4_2)    2.459    0.621    3.961    0.000
#>     dem60             1.000                           
#>     dem65             1.000
```

``` r

pen_fit <- penalized_longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    lag_cov = TRUE,
    w = 0.03,
    pen_fn = "l0a",
    se = "robust.huber.white"
)
summary(pen_fit)
#> lavaan 0.6-21 ended normally after 103 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#> 
#>   Number of observations                            75
#> 
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Sandwich
#>   Information bread                           Observed
#>   Observed information based on                Hessian
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1      (.1_1)    2.105    0.207   10.182    0.000
#>     y2      (.2_1)    2.852    0.290    9.828    0.000
#>     y3      (.3_1)    2.531    0.241   10.512    0.000
#>     y4      (.4_1)    2.905    0.226   12.872    0.000
#>   dem65 =~                                            
#>     y5      (.1_2)    2.083    0.210    9.929    0.000
#>     y6      (.2_2)    2.819    0.290    9.710    0.000
#>     y7      (.3_2)    2.602    0.252   10.322    0.000
#>     y8      (.4_2)    2.898    0.225   12.905    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.842    0.433    1.943    0.052
#>  .y2 ~~                                               
#>    .y6                1.820    0.880    2.068    0.039
#>  .y3 ~~                                               
#>    .y7                1.222    0.644    1.898    0.058
#>  .y4 ~~                                               
#>    .y8                0.284    0.467    0.608    0.543
#>   dem60 ~~                                            
#>     dem65             0.918    0.057   16.132    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    5.456    0.288   18.918    0.000
#>    .y2      (.2_1)    4.251    0.453    9.389    0.000
#>    .y3      (.3_1)    6.572    0.355   18.513    0.000
#>    .y4      (.4_1)    4.460    0.366   12.173    0.000
#>    .y5      (.1_2)    5.454    0.288   18.945    0.000
#>    .y6      (.2_2)    3.393    0.428    7.937    0.000
#>    .y7      (.3_2)    6.572    0.355   18.490    0.000
#>    .y8      (.4_2)    4.460    0.366   12.182    0.000
#>     dem60             0.000                           
#>     dem65            -0.147    0.070   -2.091    0.037
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    2.129    0.487    4.370    0.000
#>    .y2      (.2_1)    6.632    1.319    5.030    0.000
#>    .y3      (.3_1)    5.388    1.095    4.921    0.000
#>    .y4      (.4_1)    2.594    0.643    4.033    0.000
#>    .y5      (.1_2)    2.816    0.591    4.769    0.000
#>    .y6      (.2_2)    4.003    0.803    4.986    0.000
#>    .y7      (.3_2)    3.590    0.637    5.640    0.000
#>    .y8      (.4_2)    2.457    0.721    3.409    0.001
#>     dem60             1.000                           
#>     dem65             0.951    0.097    9.812    0.000
```

## Invariance Constraints

The goal of `longcfa` is to make it less error-prone when specifying
longitudinal CFA models, and save time when the number of indicators
and/or time points are large. It is perhaps most useful when needing to
impose invariance constraints across time points, which is a
prerequisite for interpreting changes in the latent constructs over
time. For example, we can see the `lavaan` syntax for a longitudinal
partial scalar invariance model using the
[`longcfa_syntax()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
function:

``` r

longcfa_syntax(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    lag_cov = TRUE,
    long_equal = c("loadings", "intercepts"),
    long_partial = list(
        # noninvariant loading for y6 at time 2
        loadings = matrix(c(2, 2), nrow = 1),
        # noninvariant intercept for y6 at time 2, and
        # for y3 at time 1
        intercepts = matrix(c(2, 3, 2, 1), nrow = 2)
    )
) |>
    cat()
#> # Time 1
#> dem60 =~ .l1 * y1 + .l2 * y2 + .l3 * y3 + .l4 * y4
#> y1 ~ .i1 * 1
#> y2 ~ .i2 * 1
#> y3 ~ .i3_1 * 1
#> y4 ~ .i4 * 1
#> y1 ~~ .u1_1 * y1
#> y2 ~~ .u2_1 * y2
#> y3 ~~ .u3_1 * y3
#> y4 ~~ .u4_1 * y4
#> 
#> # Time 2
#> dem65 =~ .l1 * y5 + .l2_2 * y6 + .l3 * y7 + .l4 * y8
#> y5 ~ .i1 * 1
#> y6 ~ .i2_2 * 1
#> y7 ~ .i3 * 1
#> y8 ~ .i4 * 1
#> y5 ~~ .u1_2 * y5
#> y6 ~~ .u2_2 * y6
#> y7 ~~ .u3_2 * y7
#> y8 ~~ .u4_2 * y8
#> 
#> # Latent variances
#> dem60 ~~ 1 * dem60
#> dem65 ~~ NA * dem65
#> 
#> # Latent means
#> dem60 ~ 0 * 1
#> dem65 ~ NA * 1
#> 
#> # Lag Covariances
#> y1 ~~ y5
#> y2 ~~ y6
#> y3 ~~ y7
#> y4 ~~ y8
```

See the [Longitudinal
Invariance](https://marklhc.github.io/longcfa/articles/invariance.md)
article for more details on invariance constraints.

### `long_equal` and `long_partial`

The `long_equal` and `long_partial` arguments are the longitudinal
analogues of the `group.equal` and `group.partial` arguments in
`lavaan`, and they specify which parameters should be equal across time
points, and which parameters should be allowed to vary across time
points, respectively. The `long_equal` argument can take the following
values: `"loadings"`, `"intercepts"`, `"thresholds"`, and `"residuals"`.
`longcfa` generates syntax that labels all measurement parameters
(loadings, intercepts, thresholds, and residual variances) across time
points, and when `long_equal` is specified, it uses the same labels for
the specified parameters.

The `long_partial` argument, on the other hand, uses a different input
format than `group.equal` in
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html).
Specifically, it should be a named list (with names `"loadings"`,
`"intercepts"`, `"thresholds"`, and/or `"residuals"`), where each
element is a matrix with two columns. The first column specifies the
time point (1 for the first time point, 2 for the second time point,
etc.), and the second column specifies the indicator number (1 for the
first indicator, 2 for the second indicator, etc.). The example above
has the input:

``` r

list(
    # noninvariant loading for y6 at time 2
    loadings = matrix(c(2, 2), nrow = 1),
    # noninvariant intercept for y6 at time 2, and
    # for y3 at time 1
    intercepts = matrix(c(2, 3, 2, 1), nrow = 2)
)
#> $loadings
#>      [,1] [,2]
#> [1,]    2    2
#> 
#> $intercepts
#>      [,1] [,2]
#> [1,]    2    2
#> [2,]    3    1
```

which specifies one noninvariant loading for the second indicator at the
second time point (`y6`), and two noninvariant intercepts: one for the
second indicator at the second time point (`y6`), and one for the third
indicator at the first time point (`y3`). Those parameters have their
time points added to the labels.

## Models Supported by `longcfa()`

The
[`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
supports the following longitudinal CFA models that can be fitted using
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html):

- Single-group models with continuous and/or categorical indicators
  - Mixed continuous and categorical indicators are supported, but not
    fully tested
  - Multiple-group models may be supported in the future
- Models with different numbers of factors across time (not tested yet)
- Single and multi-factor (experimental and not fully tested) models
  within a time point
- Loading, intercept, unique variance, and concurrent unique covariance
  (experimental) invariance across time points
  - Currently, not supporting invariance of only a subset of thresholds
    for a categorical indicator over time; for example, for an indicator
    with three thresholds, one cannot specify that only the first two
    thresholds are invariant across time points
- Other models can be specified by adding to the `model` argument of
  [`longcfa()`](https://marklhc.github.io/longcfa/reference/longcfa.md),
  or modifying the output from the
  [`longcfa_syntax()`](https://marklhc.github.io/longcfa/reference/longcfa.md)
  function. E.g.,
  - Labels and equality constraints on the cross-time covariances of the
    unique factors of the same indicators
