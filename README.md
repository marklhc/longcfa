
<!-- README.md is generated from README.Rmd. Please edit that file -->

# longcfa

<!-- badges: start -->

<!-- badges: end -->

The goal of longcfa is to simplify the specification of longitudinal
confirmatory factor analysis (CFA) using the `lavaan` package using a
matrix-like approach.

## Installation

You can install the development version of longcfa from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("marklhc/longcfa")
```

## Example

This is a basic example. See `vignette("longcfa")` for a longer example.
The package also supports

- Invariance constraints and partial invariance, as discussed in [this
  article](articles/invariance.html).
- Categorical indicators, as discussed in `vignette("cat")`.
- Penalized estimation to obtain approximate invariance, as discussed in
  `vignette("penalized")`.
- Automatic specification search by relaxing invariance constraints one
  at a time, as discussed in `vignette("specification-search")`.

``` r
library(longcfa)
library(lavaan)
#> This is lavaan 0.6-20
#> lavaan is FREE software! Please report any bugs.
# Indicator matrix
spec <- matrix(
    c(
        "y1",
        "y2",
        "y3",
        "y4",
        "y5",
        "y6",
        "y7",
        "y8"
    ),
    ncol = 2
)
# Scalar invariance
fit <- longcfa(
    spec,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    long_equal = c("loadings", "intercepts")
)
summary(fit)
#> lavaan 0.6-20 ended normally after 29 iterations
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
# Partial invariance
fit2 <- longcfa(
    spec,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    long_equal = c("loadings", "intercepts"),
    long_partial = list(
        loadings = matrix(c(1, 2), ncol = 2),
        intercepts = matrix(c(1, 3, 2, 2), ncol = 2)
    )
)
summary(fit2)
#> lavaan 0.6-20 ended normally after 37 iterations
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
```

``` r
# Approximate invariance with penalized unique covariances and differences on loadings and intercepts
# Starts with an unidentified model
mod3_un <- longcfa_syntax(
    spec,
    lv_names = c("dem60", "dem65"),
    lag_cov = TRUE,
    # Allow all unique covariances
    ucov_mat = t(combn(4, 2)),
    free_latvars = TRUE,
    free_latmeans = TRUE
)
lavaanify(mod3_un) # check parameter IDs for applying penalties
#>    id   lhs op   rhs user block group free ustart exo   label plabel
#> 1   1 dem60 =~    y1    1     1     1    1     NA   0   .l1_1   .p1.
#> 2   2 dem60 =~    y2    1     1     1    2     NA   0   .l2_1   .p2.
#> 3   3 dem60 =~    y3    1     1     1    3     NA   0   .l3_1   .p3.
#> 4   4 dem60 =~    y4    1     1     1    4     NA   0   .l4_1   .p4.
#> 5   5    y1 ~1          1     1     1    5     NA   0   .i1_1   .p5.
#> 6   6    y2 ~1          1     1     1    6     NA   0   .i2_1   .p6.
#> 7   7    y3 ~1          1     1     1    7     NA   0   .i3_1   .p7.
#> 8   8    y4 ~1          1     1     1    8     NA   0   .i4_1   .p8.
#> 9   9    y1 ~~    y1    1     1     1    9     NA   0   .u1_1   .p9.
#> 10 10    y2 ~~    y2    1     1     1   10     NA   0   .u2_1  .p10.
#> 11 11    y3 ~~    y3    1     1     1   11     NA   0   .u3_1  .p11.
#> 12 12    y4 ~~    y4    1     1     1   12     NA   0   .u4_1  .p12.
#> 13 13    y1 ~~    y2    1     1     1   13     NA   0 .uc12_1  .p13.
#> 14 14    y1 ~~    y3    1     1     1   14     NA   0 .uc13_1  .p14.
#> 15 15    y1 ~~    y4    1     1     1   15     NA   0 .uc14_1  .p15.
#> 16 16    y2 ~~    y3    1     1     1   16     NA   0 .uc23_1  .p16.
#> 17 17    y2 ~~    y4    1     1     1   17     NA   0 .uc24_1  .p17.
#> 18 18    y3 ~~    y4    1     1     1   18     NA   0 .uc34_1  .p18.
#> 19 19 dem65 =~    y5    1     1     1   19     NA   0   .l1_2  .p19.
#> 20 20 dem65 =~    y6    1     1     1   20     NA   0   .l2_2  .p20.
#> 21 21 dem65 =~    y7    1     1     1   21     NA   0   .l3_2  .p21.
#> 22 22 dem65 =~    y8    1     1     1   22     NA   0   .l4_2  .p22.
#> 23 23    y5 ~1          1     1     1   23     NA   0   .i1_2  .p23.
#> 24 24    y6 ~1          1     1     1   24     NA   0   .i2_2  .p24.
#> 25 25    y7 ~1          1     1     1   25     NA   0   .i3_2  .p25.
#> 26 26    y8 ~1          1     1     1   26     NA   0   .i4_2  .p26.
#> 27 27    y5 ~~    y5    1     1     1   27     NA   0   .u1_2  .p27.
#> 28 28    y6 ~~    y6    1     1     1   28     NA   0   .u2_2  .p28.
#> 29 29    y7 ~~    y7    1     1     1   29     NA   0   .u3_2  .p29.
#> 30 30    y8 ~~    y8    1     1     1   30     NA   0   .u4_2  .p30.
#> 31 31    y5 ~~    y6    1     1     1   31     NA   0 .uc12_2  .p31.
#> 32 32    y5 ~~    y7    1     1     1   32     NA   0 .uc13_2  .p32.
#> 33 33    y5 ~~    y8    1     1     1   33     NA   0 .uc14_2  .p33.
#> 34 34    y6 ~~    y7    1     1     1   34     NA   0 .uc23_2  .p34.
#> 35 35    y6 ~~    y8    1     1     1   35     NA   0 .uc24_2  .p35.
#> 36 36    y7 ~~    y8    1     1     1   36     NA   0 .uc34_2  .p36.
#> 37 37 dem60 ~~ dem60    1     1     1    0      1   0          .p37.
#> 38 38 dem65 ~~ dem65    1     1     1   37     NA   0          .p38.
#> 39 39 dem60 ~1          1     1     1    0      0   0          .p39.
#> 40 40 dem65 ~1          1     1     1   38     NA   0          .p40.
#> 41 41    y1 ~~    y5    1     1     1   39     NA   0          .p41.
#> 42 42    y2 ~~    y6    1     1     1   40     NA   0          .p42.
#> 43 43    y3 ~~    y7    1     1     1   41     NA   0          .p43.
#> 44 44    y4 ~~    y8    1     1     1   42     NA   0          .p44.
# Dry run
fit3 <- cfa(
    mod3_un,
    data = PoliticalDemocracy,
    std.lv = TRUE,
    do.fit = FALSE,
    start = fit2
)
# Penalized estimation
pen_fit3 <- penalized_est(
    fit3,
    w = .03,
    # penalize concurrent unique covariances
    pen_par_id = c(13:18, 31:36),
    pen_diff_id = list(
        loadings = rbind(1:4, 19:22),
        intercepts = rbind(5:8, 23:26)
    )
)
summary(pen_fit3)
#> lavaan 0.6-20 ended normally after 95 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        43
#> 
#>   Number of observations                            75
#> 
#> 
#> Parameter Estimates:
#> 
#> 
#> Latent Variables:
#>                    Estimate
#>   dem60 =~                 
#>     y1      (.1_1)    2.096
#>     y2      (.2_1)    2.911
#>     y3      (.3_1)    2.515
#>     y4      (.4_1)    2.894
#>   dem65 =~                 
#>     y5      (.1_2)    2.072
#>     y6      (.2_2)    2.887
#>     y7      (.3_2)    2.583
#>     y8      (.4_2)    2.884
#> 
#> Covariances:
#>                    Estimate
#>  .y1 ~~                    
#>    .y2     (.12_1)   -0.005
#>    .y3     (.13_1)    0.010
#>    .y4     (.14_1)   -0.010
#>  .y2 ~~                    
#>    .y3     (.23_1)   -0.004
#>    .y4     (.24_1)    0.008
#>  .y3 ~~                    
#>    .y4     (.34_1)   -0.000
#>  .y5 ~~                    
#>    .y6     (.12_2)   -0.009
#>    .y7     (.13_2)    0.006
#>    .y8     (.14_2)   -0.007
#>  .y6 ~~                    
#>    .y7     (.23_2)   -0.005
#>    .y8     (.24_2)    0.011
#>  .y7 ~~                    
#>    .y8     (.34_2)    0.002
#>  .y1 ~~                    
#>    .y5                0.845
#>  .y2 ~~                    
#>    .y6                1.676
#>  .y3 ~~                    
#>    .y7                1.222
#>  .y4 ~~                    
#>    .y8                0.266
#>   dem60 ~~                 
#>     dem65             0.919
#> 
#> Intercepts:
#>                    Estimate
#>    .y1      (.1_1)    5.499
#>    .y2      (.2_1)    3.799
#>    .y3      (.3_1)    6.666
#>    .y4      (.4_1)    4.543
#>    .y5      (.1_2)    5.505
#>    .y6      (.2_2)    3.783
#>    .y7      (.3_2)    6.670
#>    .y8      (.4_2)    4.550
#>     dem60             0.000
#>     dem65            -0.210
#> 
#> Variances:
#>                    Estimate
#>    .y1      (.1_1)    2.128
#>    .y2      (.2_1)    6.804
#>    .y3      (.3_1)    5.416
#>    .y4      (.4_1)    2.607
#>    .y5      (.1_2)    2.816
#>    .y6      (.2_2)    4.021
#>    .y7      (.3_2)    3.617
#>    .y8      (.4_2)    2.481
#>     dem60             1.000
#>     dem65             0.948
```
