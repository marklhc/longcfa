# Wrapper function for longitudinal CFA (Experimental)

This function estimates a longitudinal measurement model by first
generating model syntax (using the `longcfa_syntax()` function), and
then fit the model using
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). An input
matrix is needed where the rows are the indicators and the columns are
the time points.

## Usage

``` r
longcfa(
  ind_matrix,
  lv_names,
  pattern = NULL,
  ucov_mat = NULL,
  data = NULL,
  model = NULL,
  ordered = NULL,
  allow.empty.cell = FALSE,
  lag_cov = FALSE,
  long_equal = NULL,
  long_partial = NULL,
  nthres = NULL,
  fix_theta = NULL,
  free_latvars = FALSE,
  free_latmeans = FALSE,
  do.fit = TRUE,
  ...
)

longcfa_syntax(
  ind_matrix,
  lv_names,
  pattern = NULL,
  ucov_mat = matrix(nrow = 0, ncol = 2),
  lag_cov = FALSE,
  long_equal = NULL,
  long_partial = NULL,
  nthres = matrix(0, nrow = nrow(ind_matrix), ncol = ncol(ind_matrix)),
  fix_theta = FALSE,
  free_latvars = FALSE,
  free_latmeans = FALSE
)
```

## Arguments

- ind_matrix:

  A \\p \times T\\ matrix of character strings, where \\p\\ is the
  number of indicators and \\T\\ is the number of time points. Rows
  correspond to repeated measures of the same item, and columns
  correspond to time points. Elements should be the variable names in
  `data`.

- lv_names:

  A vector of names of \\T\\ latent variables.

- pattern:

  A list of \\T\\ elements specifying the pattern of which indicators
  load on which latent variable. Each element can be a \\p \times q\\
  matrix, where \\q\\ is the number of latent variables and can be
  different across time points. Alternatively, each element can be a
  list of \\q\\ integer vectors, specifying which indicators load on
  which latent variable. If a list of length 1 is provided, the same
  pattern is used across all time points. The default is `NULL`, which
  assumes only one latent variable per time point, and all indicators
  load on it.

- ucov_mat:

  A two-column matrix specifying pairs of indicators within the same
  time point to have their residuals correlated. Each row specifies a
  pair of indicators by their row indices in `ind_matrix`.

- data, do.fit, ordered, allow.empty.cell:

  Same as in
  [`lavaan::lavOptions()`](https://rdrr.io/pkg/lavaan/man/lavOptions.html).

- model:

  A character string showing additional syntax to be added to the model.
  Defaults to `NULL`.

- lag_cov:

  Logical; whether the same indicator is allowed to correlate over time.

- long_equal:

  A character vector indicating types of parameters to be constrained
  equal across time points. This is similar to the `group.equal`
  argument in
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html). Currently,
  only `"loadings"`, `"intercepts"`, `"thresholds"` and `"residuals"`
  are supported.

- long_partial:

  A named list of matrices specifying specific indicators to have
  different parameter values for a specific time point. The list should
  have names "loadings", "intercepts", "thresholds", and/or "residuals",
  and each element is a 2-column matrix where each row specifies the
  indicator (column 1) and the time point (column 2) for the parameter
  to be free. See example below.

- nthres:

  A matrix specifying the number of thresholds for each item, in the
  same dimension as `ind_matrix`.

- fix_theta:

  Logical; whether to fix the unique variances to 1. This is default for
  ordered categorical data.

- free_latvars:

  Logical; whether to free all latent variances, regardless of
  `long_equal` specifications.

- free_latmeans:

  Logical; whether to free all latent means, regardless of `long_equal`
  specifications.

- ...:

  Other arguments passed to
  [`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html), such as
  `data`.

## Value

A fit object as returned by
[`lavaan::cfa()`](https://rdrr.io/pkg/lavaan/man/cfa.html).

## Details

Currently only supports model with one latent variable at each time
point.

## Examples

``` r
library(lavaan)
#> This is lavaan 0.6-21
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
summary(fit)
#> lavaan 0.6-21 ended normally after 29 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        27
#>   Number of equality constraints                     8
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                51.504
#>   Degrees of freedom                                25
#>   P-value (Chi-square)                           0.001
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
#>     y1       (.l1)    2.148    0.225    9.544    0.000
#>     y2       (.l2)    2.863    0.324    8.835    0.000
#>     y3       (.l3)    2.563    0.290    8.825    0.000
#>     y4       (.l4)    2.840    0.288    9.870    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    2.148    0.225    9.544    0.000
#>     y6       (.l2)    2.863    0.324    8.835    0.000
#>     y7       (.l3)    2.563    0.290    8.825    0.000
#>     y8       (.l4)    2.840    0.288    9.870    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             0.949    0.069   13.844    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.512    0.282   19.546    0.000
#>    .y2       (.i2)    3.827    0.394    9.717    0.000
#>    .y3       (.i3)    6.657    0.353   18.866    0.000
#>    .y4       (.i4)    4.537    0.365   12.417    0.000
#>    .y5       (.i1)    5.512    0.282   19.546    0.000
#>    .y6       (.i2)    3.827    0.394    9.717    0.000
#>    .y7       (.i3)    6.657    0.353   18.866    0.000
#>    .y8       (.i4)    4.537    0.365   12.417    0.000
#>     dem60             0.000                           
#>     dem65            -0.204    0.066   -3.074    0.002
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    1.976    0.395    4.998    0.000
#>    .y2      (.2_1)    6.795    1.221    5.566    0.000
#>    .y3      (.3_1)    5.348    0.963    5.555    0.000
#>    .y4      (.4_1)    2.848    0.601    4.740    0.000
#>    .y5      (.1_2)    2.585    0.483    5.346    0.000
#>    .y6      (.2_2)    4.212    0.800    5.266    0.000
#>    .y7      (.3_2)    3.541    0.667    5.309    0.000
#>    .y8      (.4_2)    2.855    0.588    4.857    0.000
#>     dem60             1.000                           
#>     dem65             0.944    0.126    7.501    0.000
#> 
# Partial invariance
fit2 <- longcfa(spec,
                lv_names = c("dem60", "dem65"),
                data = PoliticalDemocracy,
                long_equal = c("loadings", "intercepts"),
                long_partial = list(
                    loadings = matrix(c(1, 2), ncol = 2),
                    intercepts = matrix(c(1, 3, 2, 2), ncol = 2)
                ))
summary(fit2)
#> lavaan 0.6-21 ended normally after 37 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        27
#>   Number of equality constraints                     5
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                50.535
#>   Degrees of freedom                                22
#>   P-value (Chi-square)                           0.000
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
#>     y1       (.l1)    2.201    0.247    8.899    0.000
#>     y2       (.l2)    2.831    0.327    8.658    0.000
#>     y3       (.l3)    2.548    0.294    8.654    0.000
#>     y4       (.l4)    2.799    0.290    9.650    0.000
#>   dem65 =~                                            
#>     y5      (.1_2)    2.058    0.299    6.888    0.000
#>     y6       (.l2)    2.831    0.327    8.658    0.000
#>     y7       (.l3)    2.548    0.294    8.654    0.000
#>     y8       (.l4)    2.799    0.290    9.650    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             0.966    0.079   12.301    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.465    0.301   18.166    0.000
#>    .y2       (.i2)    3.910    0.402    9.730    0.000
#>    .y3       (.i3)    6.563    0.397   16.537    0.000
#>    .y4       (.i4)    4.604    0.370   12.458    0.000
#>    .y5      (.1_2)    5.658    0.348   16.244    0.000
#>    .y6       (.i2)    3.910    0.402    9.730    0.000
#>    .y7      (.3_2)    6.842    0.421   16.234    0.000
#>    .y8       (.i4)    4.604    0.370   12.458    0.000
#>     dem60             0.000                           
#>     dem65            -0.253    0.088   -2.896    0.004
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    1.942    0.398    4.882    0.000
#>    .y2      (.2_1)    6.758    1.212    5.574    0.000
#>    .y3      (.3_1)    5.321    0.958    5.557    0.000
#>    .y4      (.4_1)    2.915    0.606    4.808    0.000
#>    .y5      (.1_2)    2.588    0.483    5.361    0.000
#>    .y6      (.2_2)    4.175    0.796    5.244    0.000
#>    .y7      (.3_2)    3.512    0.665    5.278    0.000
#>    .y8      (.4_2)    2.872    0.592    4.849    0.000
#>     dem60             1.000                           
#>     dem65             0.979    0.150    6.536    0.000
#> 
```
