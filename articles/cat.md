# Categorical Indicators

``` r

library(longcfa)
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
```

## All Indicators Categorical

We first discretize the indicators of the `PoliticalDemocracy` dataset
from the `lavaan` package. The first two indicators will be
dichotomized, and the last two indicators will be trichotomized.

``` r

# Discretized the PoliticalDemocracy data
data("PoliticalDemocracy", package = "lavaan")
pd_cat <- PoliticalDemocracy
# Dichotomize first two indicators
pd_cat[, c(1:2, 5:6)] <- lapply(PoliticalDemocracy[, c(1:2, 5:6)], function(x) {
    cut(x, breaks = c(-Inf, 5.0, Inf), labels = FALSE)
})
# Trichotomize the last two indicators
pd_cat[, c(3:4, 7:8)] <- lapply(PoliticalDemocracy[, c(3:4, 7:8)], function(x) {
    cut(x, breaks = c(-Inf, 2.5, 7.5, Inf), labels = FALSE)
})
```

### Configural model

Same factor structure across time points, no constraints on measurement
parameters.

``` r

# Using lavaan
cfa(
    "
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
",
    data = pd_cat,
    std.lv = TRUE,
    ordered = TRUE,
    parameterization = "theta"
) |>
    summary()
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 66 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                15.026      24.418
#>   Degrees of freedom                                19          19
#>   P-value (Unknown)                                 NA       0.181
#>   Scaling correction factor                                  0.743
#>   Shift parameter                                            4.207
#>     simple second-order correction                                
#> 
#> Parameter Estimates:
#> 
#>   Parameterization                               Theta
#>   Standard errors                           Robust.sem
#>   Information                                 Expected
#>   Information saturated (h1) model        Unstructured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1                1.524    0.347    4.393    0.000
#>     y2                2.044    0.628    3.256    0.001
#>     y3                1.361    0.298    4.573    0.000
#>     y4                1.875    0.361    5.190    0.000
#>   dem65 =~                                            
#>     y5                1.221    0.316    3.859    0.000
#>     y6                5.512    7.374    0.748    0.455
#>     y7                1.838    0.388    4.742    0.000
#>     y8                1.489    0.267    5.574    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             1.021    0.034   29.849    0.000
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1|t1            -0.091    0.266   -0.343    0.731
#>     y2|t1             0.816    0.377    2.165    0.030
#>     y3|t1            -2.102    0.381   -5.516    0.000
#>     y3|t2             0.486    0.251    1.937    0.053
#>     y4|t1            -1.239    0.321   -3.859    0.000
#>     y4|t2             2.000    0.421    4.752    0.000
#>     y5|t1             0.292    0.233    1.254    0.210
#>     y6|t1             3.957    5.187    0.763    0.446
#>     y7|t1            -2.324    0.472   -4.923    0.000
#>     y7|t2             0.901    0.331    2.721    0.007
#>     y8|t1            -0.773    0.265   -2.913    0.004
#>     y8|t2             1.784    0.342    5.213    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1                1.000                           
#>    .y2                1.000                           
#>    .y3                1.000                           
#>    .y4                1.000                           
#>    .y5                1.000                           
#>    .y6                1.000                           
#>    .y7                1.000                           
#>    .y8                1.000                           
#>     dem60             1.000                           
#>     dem65             1.000                           
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1                0.549                           
#>     y2                0.439                           
#>     y3                0.592                           
#>     y4                0.471                           
#>     y5                0.634                           
#>     y6                0.179                           
#>     y7                0.478                           
#>     y8                0.557
# Using longcfa
ind_mat <- matrix(
    c("y1", "y2", "y3", "y4",
      "y5", "y6", "y7", "y8"
    ),
    nrow = 4
)
longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = pd_cat,
    ordered = TRUE
) |> 
    summary()
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 66 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        21
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                15.026      24.418
#>   Degrees of freedom                                19          19
#>   P-value (Unknown)                                 NA       0.181
#>   Scaling correction factor                                  0.743
#>   Shift parameter                                            4.207
#>     simple second-order correction                                
#> 
#> Parameter Estimates:
#> 
#>   Parameterization                               Theta
#>   Standard errors                           Robust.sem
#>   Information                                 Expected
#>   Information saturated (h1) model        Unstructured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1      (.1_1)    1.524    0.347    4.393    0.000
#>     y2      (.2_1)    2.044    0.628    3.256    0.001
#>     y3      (.3_1)    1.361    0.298    4.573    0.000
#>     y4      (.4_1)    1.875    0.361    5.190    0.000
#>   dem65 =~                                            
#>     y5      (.1_2)    1.221    0.316    3.859    0.000
#>     y6      (.2_2)    5.512    7.374    0.748    0.455
#>     y7      (.3_2)    1.838    0.388    4.742    0.000
#>     y8      (.4_2)    1.489    0.267    5.574    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             1.021    0.034   29.849    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     dem60             0.000                           
#>     dem65             0.000                           
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1|t1  (.11_1)   -0.091    0.266   -0.343    0.731
#>     y2|t1  (.21_1)    0.816    0.377    2.165    0.030
#>     y3|t1  (.31_1)   -2.102    0.381   -5.516    0.000
#>     y3|t2  (.32_1)    0.486    0.251    1.937    0.053
#>     y4|t1  (.41_1)   -1.239    0.321   -3.859    0.000
#>     y4|t2  (.42_1)    2.000    0.421    4.752    0.000
#>     y5|t1  (.11_2)    0.292    0.233    1.254    0.210
#>     y6|t1  (.21_2)    3.957    5.187    0.763    0.446
#>     y7|t1  (.31_2)   -2.324    0.472   -4.923    0.000
#>     y7|t2  (.32_2)    0.901    0.331    2.721    0.007
#>     y8|t1  (.41_2)   -0.773    0.265   -2.913    0.004
#>     y8|t2  (.42_2)    1.784    0.342    5.213    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1                1.000                           
#>    .y2                1.000                           
#>    .y3                1.000                           
#>    .y4                1.000                           
#>    .y5                1.000                           
#>    .y6                1.000                           
#>    .y7                1.000                           
#>    .y8                1.000                           
#>     dem60             1.000                           
#>     dem65             1.000                           
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1                0.549                           
#>     y2                0.439                           
#>     y3                0.592                           
#>     y4                0.471                           
#>     y5                0.634                           
#>     y6                0.179                           
#>     y7                0.478                           
#>     y8                0.557
```

The warning message is likely due to the latent correlation being
estimated to be larger than 1, which could be due to the arbitrary
discretization of the indicators.

### Strict invariance

``` r

longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = pd_cat,
    ordered = TRUE,
    long_equal = c("loadings", "thresholds")
) |>
    summary()
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 43 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        23
#>   Number of equality constraints                    10
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                20.854      31.538
#>   Degrees of freedom                                27          27
#>   P-value (Unknown)                                 NA       0.250
#>   Scaling correction factor                                  0.839
#>   Shift parameter                                            6.683
#>     simple second-order correction                                
#> 
#> Parameter Estimates:
#> 
#>   Parameterization                               Theta
#>   Standard errors                           Robust.sem
#>   Information                                 Expected
#>   Information saturated (h1) model        Unstructured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1       (.l1)    1.349    0.261    5.166    0.000
#>     y2       (.l2)    2.804    0.888    3.159    0.002
#>     y3       (.l3)    1.577    0.280    5.642    0.000
#>     y4       (.l4)    1.677    0.244    6.871    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    1.349    0.261    5.166    0.000
#>     y6       (.l2)    2.804    0.888    3.159    0.002
#>     y7       (.l3)    1.577    0.280    5.642    0.000
#>     y8       (.l4)    1.677    0.244    6.871    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             1.024    0.141    7.238    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     dem60             0.000                           
#>     dem65            -0.218    0.088   -2.475    0.013
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1|t1   (.t11)   -0.034    0.222   -0.155    0.877
#>     y2|t1   (.t21)    1.265    0.527    2.400    0.016
#>     y3|t1   (.t31)   -2.374    0.341   -6.962    0.000
#>     y3|t2   (.t32)    0.500    0.249    2.011    0.044
#>     y4|t1   (.t41)   -1.173    0.252   -4.660    0.000
#>     y4|t2   (.t42)    1.709    0.298    5.741    0.000
#>     y5|t1   (.t11)   -0.034    0.222   -0.155    0.877
#>     y6|t1   (.t21)    1.265    0.527    2.400    0.016
#>     y7|t1   (.t31)   -2.374    0.341   -6.962    0.000
#>     y7|t2   (.t32)    0.500    0.249    2.011    0.044
#>     y8|t1   (.t41)   -1.173    0.252   -4.660    0.000
#>     y8|t2   (.t42)    1.709    0.298    5.741    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1                1.000                           
#>    .y2                1.000                           
#>    .y3                1.000                           
#>    .y4                1.000                           
#>    .y5                1.000                           
#>    .y6                1.000                           
#>    .y7                1.000                           
#>    .y8                1.000                           
#>     dem60             1.000                           
#>     dem65             1.000    0.261    3.834    0.000
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1                0.596                           
#>     y2                0.336                           
#>     y3                0.535                           
#>     y4                0.512                           
#>     y5                0.596                           
#>     y6                0.336                           
#>     y7                0.535                           
#>     y8                0.512
```

## Mixed Continuous and Categorical

Here, only the first two indicators are categorical.

``` r

pd_mixed <- pd_cat
pd_mixed[, c(1:2, 5:6)] <- PoliticalDemocracy[, c(1:2, 5:6)]
# Configural, using lavaan
cfa(
    "
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
",
    data = pd_mixed,
    std.lv = TRUE,
    ordered = paste0("y", c(3:4, 7:8)),
    parameterization = "theta"
) |>
    summary()
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 57 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        25
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                 4.926      27.877
#>   Degrees of freedom                                19          19
#>   P-value (Unknown)                                 NA       0.086
#>   Scaling correction factor                                  0.215
#>   Shift parameter                                            4.914
#>     simple second-order correction                                
#> 
#> Parameter Estimates:
#> 
#>   Parameterization                               Theta
#>   Standard errors                           Robust.sem
#>   Information                                 Expected
#>   Information saturated (h1) model        Unstructured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1                2.116    0.369    5.737    0.000
#>     y2                2.673    0.773    3.457    0.001
#>     y3                1.444    0.329    4.390    0.000
#>     y4                1.602    0.332    4.826    0.000
#>   dem65 =~                                            
#>     y5                1.942    0.328    5.928    0.000
#>     y6                2.545    0.514    4.956    0.000
#>     y7                2.155    0.490    4.397    0.000
#>     y8                1.600    0.334    4.787    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             1.010    0.044   22.721    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1                5.465    0.304   17.957    0.000
#>    .y2                4.256    0.505    8.436    0.000
#>    .y5                5.136    0.308   16.663    0.000
#>    .y6                2.978    0.561    5.304    0.000
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y3|t1            -2.186    0.417   -5.239    0.000
#>     y3|t2             0.506    0.259    1.954    0.051
#>     y4|t1            -1.101    0.296   -3.718    0.000
#>     y4|t2             1.778    0.372    4.784    0.000
#>     y7|t1            -2.639    0.593   -4.452    0.000
#>     y7|t2             1.023    0.378    2.706    0.007
#>     y8|t1            -0.813    0.298   -2.731    0.006
#>     y8|t2             1.876    0.397    4.722    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1                2.310    0.518    4.459    0.000
#>    .y2                8.225    1.789    4.598    0.000
#>    .y3                1.000                           
#>    .y4                1.000                           
#>    .y5                2.961    0.592    5.005    0.000
#>    .y6                4.747    0.961    4.941    0.000
#>    .y7                1.000                           
#>    .y8                1.000                           
#>     dem60             1.000                           
#>     dem65             1.000                           
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y3                0.569                           
#>     y4                0.529                           
#>     y7                0.421                           
#>     y8                0.530
# configural, using longcfa
longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = pd_mixed,
    ordered = c("y3", "y4", "y7", "y8")
)
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 57 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        25
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                 4.926      27.877
#>   Degrees of freedom                                19          19
#>   P-value (Unknown)                                 NA       0.086
#>   Scaling correction factor                                  0.215
#>   Shift parameter                                            4.914
#>     simple second-order correction
```

``` r

# strict invariance, using longcfa
longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = pd_mixed,
    ordered = c("y3", "y4", "y7", "y8"),
    long_equal = c("loadings", "intercepts", "thresholds", "residuals")
) |>
    summary()
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
#> lavaan 0.6-21 ended normally after 40 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        27
#>   Number of equality constraints                    12
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                 8.499      28.134
#>   Degrees of freedom                                29          29
#>   P-value (Unknown)                                 NA       0.511
#>   Scaling correction factor                                  0.550
#>   Shift parameter                                           12.691
#>     simple second-order correction                                
#> 
#> Parameter Estimates:
#> 
#>   Parameterization                               Theta
#>   Standard errors                           Robust.sem
#>   Information                                 Expected
#>   Information saturated (h1) model        Unstructured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 =~                                            
#>     y1       (.l1)    2.047    0.352    5.818    0.000
#>     y2       (.l2)    2.638    0.640    4.122    0.000
#>     y3       (.l3)    1.764    0.300    5.886    0.000
#>     y4       (.l4)    1.649    0.287    5.751    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    2.047    0.352    5.818    0.000
#>     y6       (.l2)    2.638    0.640    4.122    0.000
#>     y7       (.l3)    1.764    0.300    5.886    0.000
#>     y8       (.l4)    1.649    0.287    5.751    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             0.980    0.112    8.720    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.498    0.303   18.164    0.000
#>    .y2       (.i2)    3.913    0.456    8.577    0.000
#>    .y5       (.i1)    5.498    0.303   18.164    0.000
#>    .y6       (.i2)    3.913    0.456    8.577    0.000
#>     dem60             0.000                           
#>     dem65            -0.193    0.078   -2.486    0.013
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y3|t1   (.t31)   -2.537    0.388   -6.533    0.000
#>     y3|t2   (.t32)    0.549    0.272    2.023    0.043
#>     y4|t1   (.t41)   -1.129    0.254   -4.445    0.000
#>     y4|t2   (.t42)    1.689    0.306    5.519    0.000
#>     y7|t1   (.t31)   -2.537    0.388   -6.533    0.000
#>     y7|t2   (.t32)    0.549    0.272    2.023    0.043
#>     y8|t1   (.t41)   -1.129    0.254   -4.445    0.000
#>     y8|t2   (.t42)    1.689    0.306    5.519    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.u1)    2.694    0.421    6.391    0.000
#>    .y2       (.u2)    5.554    0.953    5.827    0.000
#>    .y3                1.000                           
#>    .y4                1.000                           
#>    .y5       (.u1)    2.694    0.421    6.391    0.000
#>    .y6       (.u2)    5.554    0.953    5.827    0.000
#>    .y7                1.000                           
#>    .y8                1.000                           
#>     dem60             1.000                           
#>     dem65             0.948    0.192    4.950    0.000
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y3                0.493                           
#>     y4                0.519                           
#>     y7                0.503                           
#>     y8                0.529
```
