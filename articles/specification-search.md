# Specification Search for Partial Invariance

``` r

library(longcfa)
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.
```

This vignette demonstrates how to use the
[`plinv_search()`](https://marklhc.github.io/longcfa/reference/plinv_search.md)
function to perform a specification search for partial invariance in
longitudinal CFA models. We will use the `PoliticalDemocracy` dataset
from the `lavaan` package.

## Data Preparation

First, we define the indicator matrix and latent variable names for the
`PoliticalDemocracy` dataset.

``` r

data("PoliticalDemocracy", package = "lavaan")

# Define indicator matrix (rows = indicators, columns = time points)
ind_mat <- cbind(
    c("y1", "y2", "y3", "y4"),
    c("y5", "y6", "y7", "y8")
)

# Define latent variable names
lv_names <- c("dem60", "dem65")
```

## Invariance Model

We start by fitting a model with strict longitudinal invariance
constraints (on loadings, intercepts, and unique variances).

``` r

fit_strict <- longcfa(
    ind_mat,
    lv_names = lv_names,
    data = PoliticalDemocracy,
    lag_cov = TRUE,
    long_equal = c("loadings", "intercepts", "residuals")
)

summary(fit_strict)
#> lavaan 0.6-21 ended normally after 44 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#>   Number of equality constraints                    12
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                40.802
#>   Degrees of freedom                                25
#>   P-value (Chi-square)                           0.024
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
#>     y1       (.l1)    2.083    0.239    8.701    0.000
#>     y2       (.l2)    2.953    0.344    8.580    0.000
#>     y3       (.l3)    2.499    0.299    8.346    0.000
#>     y4       (.l4)    2.901    0.293    9.887    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    2.083    0.239    8.701    0.000
#>     y6       (.l2)    2.953    0.344    8.580    0.000
#>     y7       (.l3)    2.499    0.299    8.346    0.000
#>     y8       (.l4)    2.901    0.293    9.887    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.844    0.374    2.257    0.024
#>  .y2 ~~                                               
#>    .y6                1.697    0.795    2.134    0.033
#>  .y3 ~~                                               
#>    .y7                1.176    0.638    1.843    0.065
#>  .y4 ~~                                               
#>    .y8                0.273    0.488    0.560    0.576
#>   dem60 ~~                                            
#>     dem65             0.910    0.066   13.872    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.521    0.288   19.167    0.000
#>    .y2       (.i2)    3.929    0.412    9.540    0.000
#>    .y3       (.i3)    6.644    0.355   18.730    0.000
#>    .y4       (.i4)    4.554    0.370   12.322    0.000
#>    .y5       (.i1)    5.521    0.288   19.167    0.000
#>    .y6       (.i2)    3.929    0.412    9.540    0.000
#>    .y7       (.i3)    6.644    0.355   18.730    0.000
#>    .y8       (.i4)    4.554    0.370   12.322    0.000
#>     dem60             0.000                           
#>     dem65            -0.211    0.066   -3.196    0.001
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.u1)    2.477    0.379    6.544    0.000
#>    .y2       (.u2)    5.414    0.804    6.734    0.000
#>    .y3       (.u3)    4.558    0.644    7.073    0.000
#>    .y4       (.u4)    2.563    0.500    5.122    0.000
#>    .y5       (.u1)    2.477    0.379    6.544    0.000
#>    .y6       (.u2)    5.414    0.804    6.734    0.000
#>    .y7       (.u3)    4.558    0.644    7.073    0.000
#>    .y8       (.u4)    2.563    0.500    5.122    0.000
#>     dem60             1.000                           
#>     dem65             0.929    0.122    7.591    0.000
```

## Specification Search

The
[`plinv_search()`](https://marklhc.github.io/longcfa/reference/plinv_search.md)
function iteratively relaxes invariance constraints based on
modification indices or score tests. Here, we search for partial
invariance in both loadings and intercepts.

We use `get_lav_test_score` as the “modification index” function
(`mi_fun`), which uses the score test
([`lavaan::lavTestScore()`](https://rdrr.io/pkg/lavaan/man/lavTestScore.html))
to identify constraints to release. We set a minimum threshold
(`mi_min`) of 3.84 (corresponding to p \< .05 for 1 df).

``` r

get_lav_test_score(fit_strict, ind = paste0("y", 1:8), op = "~~")
#>    id lhs op rhs group plabel         mi
#> 21 21  y5 ~~  y5     1  .p21. 0.96575556
#> 22 22  y6 ~~  y6     1  .p22. 3.63593977
#> 23 23  y7 ~~  y7     1  .p23. 2.62279730
#> 24 24  y8 ~~  y8     1  .p24. 0.01845535
```

Note that the
[`plinv_search()`](https://marklhc.github.io/longcfa/reference/plinv_search.md)
function proceeds in steps, so when
`type = c("loadings", "intercepts", "residuals")` is specified, it will
first search for loadings from the metric invariance model, then
intercepts, and finally residuals.

``` r

# Perform specification search
search_res <- plinv_search(
    ind_matrix = ind_mat,
    lv_names = lv_names,
    data = PoliticalDemocracy,
    type = c("loadings", "intercepts", "residuals"),
    mi_fun = get_lav_test_score,
    mi_min = 3.84,
    lag_cov = TRUE # Passed to longcfa()
)
```

## Results

The result object contains the final fitted model (`fit`) and a trace of
the relaxed constraints (`traces`).

### Relaxed Constraints

We can examine which constraints were relaxed during the search process.

``` r

search_res$traces
#>    id lhs op rhs group plabel       mi
#> 18 18  y6 ~1         1  .p18. 5.822431
#> 22 22  y6 ~~  y6     1  .p22. 3.984035
```

The output shows the left-hand side (`lhs`), operator (`op`), and
right-hand side (`rhs`) of the parameters that were freed, along with
the modification index (`mi`) at that step.

### Final Model Summary

We can inspect the summary of the final partial invariance model.

``` r

summary(search_res$fit)
#> lavaan 0.6-21 ended normally after 41 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#>   Number of equality constraints                    10
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                30.828
#>   Degrees of freedom                                23
#>   P-value (Chi-square)                           0.127
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
#>     y1       (.l1)    2.084    0.240    8.693    0.000
#>     y2       (.l2)    2.834    0.341    8.305    0.000
#>     y3       (.l3)    2.509    0.300    8.353    0.000
#>     y4       (.l4)    2.915    0.294    9.928    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    2.084    0.240    8.693    0.000
#>     y6       (.l2)    2.834    0.341    8.305    0.000
#>     y7       (.l3)    2.509    0.300    8.353    0.000
#>     y8       (.l4)    2.915    0.294    9.928    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.842    0.374    2.249    0.025
#>  .y2 ~~                                               
#>    .y6                1.873    0.766    2.446    0.014
#>  .y3 ~~                                               
#>    .y7                1.218    0.638    1.910    0.056
#>  .y4 ~~                                               
#>    .y8                0.271    0.485    0.560    0.576
#>   dem60 ~~                                            
#>     dem65             0.911    0.066   13.711    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.454    0.289   18.862    0.000
#>    .y2       (.i2)    4.256    0.443    9.612    0.000
#>    .y3       (.i3)    6.564    0.357   18.382    0.000
#>    .y4       (.i4)    4.462    0.372   11.989    0.000
#>    .y5       (.i1)    5.454    0.289   18.862    0.000
#>    .y6      (.2_2)    3.395    0.433    7.835    0.000
#>    .y7       (.i3)    6.564    0.357   18.382    0.000
#>    .y8       (.i4)    4.462    0.372   11.989    0.000
#>     dem60             0.000                           
#>     dem65            -0.147    0.070   -2.089    0.037
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.u1)    2.483    0.379    6.549    0.000
#>    .y2       (.u2)    6.674    1.245    5.362    0.000
#>    .y3       (.u3)    4.536    0.645    7.036    0.000
#>    .y4       (.u4)    2.491    0.499    4.997    0.000
#>    .y5       (.u1)    2.483    0.379    6.549    0.000
#>    .y6       (.u2)    4.021    0.814    4.942    0.000
#>    .y7       (.u3)    4.536    0.645    7.036    0.000
#>    .y8       (.u4)    2.491    0.499    4.997    0.000
#>     dem60             1.000                           
#>     dem65             0.942    0.125    7.537    0.000
```

### Compare Models

Finally, we can compare the initial strict invariance model with the
partial invariance model found by the search.

``` r

anova(fit_strict, search_res$fit)
#> 
#> Chi-Squared Difference Test
#> 
#>                Df    AIC    BIC  Chisq Chisq diff   RMSEA Df diff Pr(>Chisq)   
#> search_res$fit 23 2698.0 2746.6 30.828                                         
#> fit_strict     25 2703.9 2748.0 40.802     9.9739 0.23056       2   0.006827 **
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Specification Search with Categorical Data

This section demonstrates how to perform a specification search when
some or all indicators are categorical (ordinal). We discretize the
continuous `PoliticalDemocracy` variables into ordinal categories and
then constrain thresholds during invariance testing.

### Creating Ordered Indicators

We start by converting the first two items (`y1`, `y2`) into 3-category
ordinal variables, and the remaining two items (`y3`, `y4`) into binary
variables. The same transformation is applied at both time points.

``` r

data("PoliticalDemocracy", package = "lavaan")

# Create ordered versions: 3 categories for y1/y2, 2 (binary) for y3/y4
ordinal_data <- PoliticalDemocracy

ordinal_data$y1_cat <- cut(
    ordinal_data$y1,
    breaks = 3,
    labels = c("low", "mid", "high")
)
ordinal_data$y2_cat <- cut(
    ordinal_data$y2,
    breaks = 3,
    labels = c("low", "mid", "high")
)

ordinal_data$y3_cat <- ifelse(
    PoliticalDemocracy$y3 >= median(PoliticalDemocracy$y3),
    "yes",
    "no"
)
ordinal_data$y4_cat <- ifelse(
    PoliticalDemocracy$y4 >= median(PoliticalDemocracy$y4),
    "yes",
    "no"
)

ordinal_data$y5_cat <- cut(
    ordinal_data$y5,
    breaks = 3,
    labels = c("low", "mid", "high")
)
ordinal_data$y6_cat <- cut(
    ordinal_data$y6,
    breaks = 3,
    labels = c("low", "mid", "high")
)

ordinal_data$y7_cat <- ifelse(
    PoliticalDemocracy$y7 >= median(PoliticalDemocracy$y7),
    "yes",
    "no"
)
ordinal_data$y8_cat <- ifelse(
    PoliticalDemocracy$y8 >= median(PoliticalDemocracy$y8),
    "yes",
    "no"
)

# Define indicator matrix for the ordinal items
ind_mat_ord <- cbind(
    c("y1_cat", "y2_cat", "y3_cat", "y4_cat"),
    c("y5_cat", "y6_cat", "y7_cat", "y8_cat")
)

# Specify which variables are ordered (all 8 items)
ordered_vars <- c(
    "y1_cat",
    "y2_cat",
    "y3_cat",
    "y4_cat",
    "y5_cat",
    "y6_cat",
    "y7_cat",
    "y8_cat"
)

# Define latent variable names
lv_names_ord <- c("dem60", "dem65")
```

### Fitting Strict Threshold Invariance Model

We fit a configural invariance model first, allowing thresholds to
differ across time points. Then we test strict (scalar) threshold
invariance by constraining corresponding thresholds equal across time.

``` r

# Configural invariance: thresholds free across time
fit_conf <- longcfa(
    ind_mat_ord,
    lv_names = lv_names_ord,
    data = ordinal_data,
    ordered = ordered_vars,
    estimator = "WLSMV",
    lag_cov = TRUE,
    long_equal = c("loadings") # only constrain loadings
)

summary(fit_conf, fit.measures = TRUE)
#> lavaan 0.6-21 ended normally after 63 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        26
#>   Number of equality constraints                     4
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                18.831      26.877
#>   Degrees of freedom                                18          18
#>   P-value (Unknown)                                 NA       0.081
#>   Scaling correction factor                                  0.818
#>   Shift parameter                                            3.847
#>     simple second-order correction                                
#> 
#> Model Test Baseline Model:
#> 
#>   Test statistic                              1822.105    1098.798
#>   Degrees of freedom                                28          28
#>   P-value                                           NA       0.000
#>   Scaling correction factor                                  1.675
#> 
#> User Model versus Baseline Model:
#> 
#>   Comparative Fit Index (CFI)                    1.000       0.992
#>   Tucker-Lewis Index (TLI)                       0.999       0.987
#>                                                                   
#>   Robust Comparative Fit Index (CFI)                            NA
#>   Robust Tucker-Lewis Index (TLI)                               NA
#> 
#> Root Mean Square Error of Approximation:
#> 
#>   RMSEA                                          0.025       0.082
#>   90 Percent confidence interval - lower         0.000       0.000
#>   90 Percent confidence interval - upper         0.108       0.142
#>   P-value H_0: RMSEA <= 0.050                    0.603       0.201
#>   P-value H_0: RMSEA >= 0.080                    0.180       0.556
#>                                                                   
#>   Robust RMSEA                                                  NA
#>   90 Percent confidence interval - lower                        NA
#>   90 Percent confidence interval - upper                        NA
#>   P-value H_0: Robust RMSEA <= 0.050                            NA
#>   P-value H_0: Robust RMSEA >= 0.080                            NA
#> 
#> Standardized Root Mean Square Residual:
#> 
#>   SRMR                                           0.096       0.096
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
#>     y1_cat   (.l1)    2.408    1.057    2.279    0.023
#>     y2_cat   (.l2)    1.426    0.376    3.790    0.000
#>     y3_cat   (.l3)    1.407    0.409    3.436    0.001
#>     y4_cat   (.l4)    0.880    0.227    3.882    0.000
#>   dem65 =~                                            
#>     y5_cat   (.l1)    2.408    1.057    2.279    0.023
#>     y6_cat   (.l2)    1.426    0.376    3.790    0.000
#>     y7_cat   (.l3)    1.407    0.409    3.436    0.001
#>     y8_cat   (.l4)    0.880    0.227    3.882    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1_cat ~~                                           
#>    .y5_cat            1.081    0.354    3.050    0.002
#>  .y2_cat ~~                                           
#>    .y6_cat            0.720    0.183    3.931    0.000
#>  .y3_cat ~~                                           
#>    .y7_cat            0.309    0.325    0.952    0.341
#>  .y4_cat ~~                                           
#>    .y8_cat            0.139    0.271    0.514    0.607
#>   dem60 ~~                                            
#>     dem65             1.082    0.225    4.816    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     dem60             0.000                           
#>     dem65             0.000                           
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1_c|1 (.11_1)   -1.028    0.475   -2.164    0.030
#>     y1_c|2 (.12_1)    0.660    0.474    1.395    0.163
#>     y2_c|1 (.21_1)   -0.441    0.256   -1.721    0.085
#>     y2_c|2 (.22_1)    0.687    0.278    2.469    0.014
#>     y3_c|1 (.31_1)   -0.619    0.283   -2.185    0.029
#>     y4_c|1 (.41_1)   -0.673    0.211   -3.196    0.001
#>     y5_c|1 (.11_2)   -2.171    0.979   -2.216    0.027
#>     y5_c|2 (.12_2)    1.671    0.816    2.049    0.040
#>     y6_c|1 (.21_2)    0.233    0.294    0.794    0.427
#>     y6_c|2 (.22_2)    1.583    0.424    3.730    0.000
#>     y7_c|1 (.31_2)   -0.777    0.328   -2.366    0.018
#>     y8_c|1 (.41_2)   -0.471    0.214   -2.203    0.028
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1_cat            1.000                           
#>    .y2_cat            1.000                           
#>    .y3_cat            1.000                           
#>    .y4_cat            1.000                           
#>    .y5_cat            1.000                           
#>    .y6_cat            1.000                           
#>    .y7_cat            1.000                           
#>    .y8_cat            1.000                           
#>     dem60             1.000                           
#>     dem65             1.457    0.617    2.363    0.018
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1_cat            0.384                           
#>     y2_cat            0.574                           
#>     y3_cat            0.579                           
#>     y4_cat            0.751                           
#>     y5_cat            0.325                           
#>     y6_cat            0.502                           
#>     y7_cat            0.507                           
#>     y8_cat            0.685
```

### Specification Search on Thresholds

Now we use
[`plinv_search()`](https://marklhc.github.io/longcfa/reference/plinv_search.md)
to find which threshold constraints should be relaxed. Support for
threshold is currently experimental. We search only over thresholds (the
`"thresholds"` type maps to the `"|"` operator internally). The function
iteratively frees the threshold with the highest modification index
until no remaining constraint exceeds `mi_min`.

``` r

# Perform specification search on thresholds
search_thresh <- plinv_search(
    ind_matrix = ind_mat_ord,
    lv_names = lv_names_ord,
    data = ordinal_data,
    type = c("loadings", "thresholds"),
    parameterization = "theta",
    mi_fun = get_lav_test_score,
    mi_min = 3.84,
    ordered = ordered_vars,
    estimator = "WLSMV",
    lag_cov = TRUE
)
#> Warning: lavaan->lavTestScore():  
#>    se is not `standard'; not implemented yet; falling back to ordinary score 
#>    test
#> Warning: lavaan->lavTestScore():  
#>    se is not `standard'; not implemented yet; falling back to ordinary score 
#>    test
#> Warning: lavaan->lavTestScore():  
#>    se is not `standard'; not implemented yet; falling back to ordinary score 
#>    test
```

### Relaxed Threshold Constraints

The search may free certain threshold parameters that show significant
misspecification. We examine the trace of relaxed constraints:

``` r

search_thresh$traces
#>    id    lhs op rhs group plabel       mi
#> 19 19 y5_cat  |  t1     1  .p19. 5.953566
```

The output shows which indicator and which threshold level was freed at
each step, along with the modification index that justified the
relaxation.

### Final Partial Threshold Invariance Model

We compare the configural model (all thresholds free) with the partial
threshold invariance model found by the search:

``` r

anova(fit_conf, search_thresh$fit)
#> 
#> Scaled Chi-Squared Difference Test (method = "satorra.2000")
#> 
#> lavaan->lavTestLRT():  
#>    lavaan NOTE: The "Chisq" column contains standard test statistics, not the 
#>    robust test that should be reported per model. A robust difference test is 
#>    a function of two standard (not robust) statistics.
#> 
#>                   Df AIC BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)  
#> fit_conf          18         18.831                                      
#> search_thresh$fit 22         22.485      8.868     0       4    0.06449 .
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(search_thresh$fit)
#> lavaan 0.6-21 ended normally after 47 iterations
#> 
#>   Estimator                                       DWLS
#>   Optimization method                           NLMINB
#>   Number of model parameters                        27
#>   Number of equality constraints                     9
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                               Standard      Scaled
#>   Test Statistic                                22.485      34.004
#>   Degrees of freedom                                22          22
#>   P-value (Unknown)                                 NA       0.049
#>   Scaling correction factor                                  0.782
#>   Shift parameter                                            5.244
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
#>     y1_cat   (.l1)    2.560    1.187    2.157    0.031
#>     y2_cat   (.l2)    1.495    0.396    3.771    0.000
#>     y3_cat   (.l3)    1.476    0.425    3.471    0.001
#>     y4_cat   (.l4)    0.918    0.230    3.986    0.000
#>   dem65 =~                                            
#>     y5_cat   (.l1)    2.560    1.187    2.157    0.031
#>     y6_cat   (.l2)    1.495    0.396    3.771    0.000
#>     y7_cat   (.l3)    1.476    0.425    3.471    0.001
#>     y8_cat   (.l4)    0.918    0.230    3.986    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1_cat ~~                                           
#>    .y5_cat            1.099    0.370    2.969    0.003
#>  .y2_cat ~~                                           
#>    .y6_cat            0.719    0.183    3.934    0.000
#>  .y3_cat ~~                                           
#>    .y7_cat            0.307    0.330    0.930    0.352
#>  .y4_cat ~~                                           
#>    .y8_cat            0.141    0.269    0.522    0.602
#>   dem60 ~~                                            
#>     dem65             0.986    0.181    5.454    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     dem60             0.000                           
#>     dem65            -0.312    0.118   -2.648    0.008
#> 
#> Thresholds:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1_ct|1 (.t11)   -1.084    0.528   -2.052    0.040
#>     y1_ct|2 (.t12)    0.755    0.510    1.480    0.139
#>     y2_ct|1 (.t21)   -0.354    0.244   -1.454    0.146
#>     y2_ct|2 (.t22)    0.860    0.290    2.966    0.003
#>     y3_ct|1 (.t31)   -0.905    0.311   -2.913    0.004
#>     y4_ct|1 (.t41)   -0.716    0.179   -3.988    0.000
#>     y5_ct|1 (.t11)   -2.916    1.313   -2.222    0.026
#>     y5_ct|2 (.t12)    0.755    0.510    1.480    0.139
#>     y6_ct|1 (.t21)   -0.354    0.244   -1.454    0.146
#>     y6_ct|2 (.t22)    0.860    0.290    2.966    0.003
#>     y7_ct|1 (.t31)   -0.905    0.311   -2.913    0.004
#>     y8_ct|1 (.t41)   -0.716    0.179   -3.988    0.000
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1_cat            1.000                           
#>    .y2_cat            1.000                           
#>    .y3_cat            1.000                           
#>    .y4_cat            1.000                           
#>    .y5_cat            1.000                           
#>    .y6_cat            1.000                           
#>    .y7_cat            1.000                           
#>    .y8_cat            1.000                           
#>     dem60             1.000                           
#>     dem65             1.220    0.461    2.643    0.008
#> 
#> Scales y*:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     y1_cat            0.364                           
#>     y2_cat            0.556                           
#>     y3_cat            0.561                           
#>     y4_cat            0.737                           
#>     y5_cat            0.333                           
#>     y6_cat            0.518                           
#>     y7_cat            0.523                           
#>     y8_cat            0.702
```

The partial threshold invariance model retains equality constraints on
most thresholds while freeing only those that significantly violate
scalar invariance. This provides a more accurate representation of
measurement equivalence across time points for ordinal indicators.
