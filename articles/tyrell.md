# A harmonization example with summary statistics

The following example demonstrates longitudinal harmonization using
summary statistics. The data come from the summary statistics in [Tyrell
et al. (2019)](https://doi.org/10.1080/15374416.2019.1622124), with two
common parcels across five waves, but two unique parcels that are common
in Waves 1-3, and two other unique parcels that are common in Waves 4-5.
The correlation matrix, standard deviations, and means are provided in
the paper, and imported below.

``` r

library(longcfa)

# Load the lavaan package
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.

# 1. Define the variable names (20 variables total)
# fmt: skip
var_names <- c(
  "w1com1", "w1com2", "w1uniq1", "w1uniq2",
  "w2com1", "w2com2", "w2uniq1", "w2uniq2",
  "w3com1", "w3com2", "w3uniq1", "w3uniq2",
  "w4com1", "w4com2", "w4uniq1", "w4uniq2",
  "w5com1", "w5com2", "w5uniq1", "w5uniq2"
)

# 2. Input the lower triangular correlation matrix as a string
# We include the diagonal 1s to complete the matrix shape.
lower_cor_string <- "
  1
 .32 1
 .36 .45 1
 .20 .36 .35 1
 .27 .22 .28 .27 1
 .16 .31 .27 .23 .37 1
 .14 .22 .30 .18 .31 .50 1
 .20 .19 .24 .31 .44 .47 .38 1
 .15 .18 .24 .21 .25 .19 .22 .22 1
 .20 .30 .32 .10 .17 .30 .30 .22 .44 1
 .07 .19 .26 .13 .25 .27 .38 .28 .36 .43 1
 .17 .28 .21 .26 .22 .18 .14 .37 .32 .36 .25 1
 .00 .12 .11 .08 .16 .11 .10 .12 .23 .16 .26 .13 1
 .00 .16 .24 .14 .16 .07 .13 .04 .21 .19 .34 .15 .57 1
 .02 .20 .18 .13 .18 .11 .10 .16 .26 .18 .29 .21 .64 .69 1
-.06 .16 .19 .19 .13 .15 .07 .13 .17 .18 .25 .23 .50 .56 .67 1
 .07 .08 .15 .08 .13 .11 .19 .21 .19 .19 .31 .13 .31 .24 .31 .30 1
 .04 .13 .18 .12 .10 .17 .23 .14 .17 .17 .35 .09 .33 .45 .39 .36 .57 1
 .04 .19 .16 .16 .10 .08 .14 .10 .23 .19 .22 .15 .43 .44 .60 .45 .51 .61 1
 .05 .11 .14 .16 .11 .12 .13 .15 .14 .11 .20 .13 .33 .34 .39 .43 .40 .56 .53 1
"

# Parse the string into a full symmetric correlation matrix
cor_matrix <- lav_getcov(lower_cor_string, names = var_names)

# 3. Input Standard Deviations and Means
# fmt: skip
sds <- c(0.38, 0.49, 0.44, 0.50, 0.39, 0.44, 0.44, 0.47, 0.34, 0.44, 
         0.43, 0.50, 0.33, 0.40, 0.33, 0.29, 0.33, 0.40, 0.28, 0.27)

# fmt: skip
means <- c(1.44, 1.42, 1.42, 1.41, 1.39, 1.40, 1.35, 1.42, 1.42, 1.41, 
           1.42, 1.51, 1.21, 1.29, 1.22, 1.18, 1.25, 1.32, 1.19, 1.16)

# Assign variable names to the vectors for safe keeping
names(sds) <- var_names
names(means) <- var_names

# 4. Convert Correlation matrix to Covariance matrix
cov_matrix <- lav_cor2cov(cor_matrix, sds)
```

``` r

ind_mat <- matrix(NA, nrow = 6, ncol = 5)
ind_mat[1:4, 1:3] <- var_names[1:12]
ind_mat[c(1:2, 5:6), 4:5] <- var_names[13:20]
ind_mat
#>      [,1]      [,2]      [,3]      [,4]      [,5]     
#> [1,] "w1com1"  "w2com1"  "w3com1"  "w4com1"  "w5com1" 
#> [2,] "w1com2"  "w2com2"  "w3com2"  "w4com2"  "w5com2" 
#> [3,] "w1uniq1" "w2uniq1" "w3uniq1" NA        NA       
#> [4,] "w1uniq2" "w2uniq2" "w3uniq2" NA        NA       
#> [5,] NA        NA        NA        "w4uniq1" "w5uniq1"
#> [6,] NA        NA        NA        "w4uniq2" "w5uniq2"
```

``` r

lconfig_fit <- longcfa(
    ind_mat,
    lv_names = paste0("w", 1:5, "dep"),
    sample.cov = cov_matrix,
    sample.mean = means,
    sample.nobs = 392,
    lag_cov = TRUE
)
summary(lconfig_fit, fit.measures = TRUE)
#> lavaan 0.6-21 ended normally after 161 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        98
#> 
#>   Number of observations                           392
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                               250.998
#>   Degrees of freedom                               132
#>   P-value (Chi-square)                           0.000
#> 
#> Model Test Baseline Model:
#> 
#>   Test statistic                              2891.919
#>   Degrees of freedom                               190
#>   P-value                                        0.000
#> 
#> User Model versus Baseline Model:
#> 
#>   Comparative Fit Index (CFI)                    0.956
#>   Tucker-Lewis Index (TLI)                       0.937
#> 
#> Loglikelihood and Information Criteria:
#> 
#>   Loglikelihood user model (H0)              -2365.878
#>   Loglikelihood unrestricted model (H1)      -2240.379
#>                                                       
#>   Akaike (AIC)                                4927.755
#>   Bayesian (BIC)                              5316.939
#>   Sample-size adjusted Bayesian (SABIC)       5005.989
#> 
#> Root Mean Square Error of Approximation:
#> 
#>   RMSEA                                          0.048
#>   90 Percent confidence interval - lower         0.039
#>   90 Percent confidence interval - upper         0.057
#>   P-value H_0: RMSEA <= 0.050                    0.634
#>   P-value H_0: RMSEA >= 0.080                    0.000
#> 
#> Standardized Root Mean Square Residual:
#> 
#>   SRMR                                           0.042
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   w1dep =~                                            
#>     w1com1  (.1_1)    0.172    0.021    8.140    0.000
#>     w1com2  (.2_1)    0.318    0.027   11.926    0.000
#>     w1uniq1 (.3_1)    0.318    0.024   13.281    0.000
#>     w1uniq2 (.4_1)    0.252    0.027    9.183    0.000
#>   w2dep =~                                            
#>     w2com1  (.1_2)    0.221    0.021   10.708    0.000
#>     w2com2  (.2_2)    0.314    0.022   13.971    0.000
#>     w2uniq1 (.3_2)    0.276    0.023   12.075    0.000
#>     w2uniq2 (.4_2)    0.305    0.024   12.742    0.000
#>   w3dep =~                                            
#>     w3com1  (.1_3)    0.207    0.018   11.400    0.000
#>     w3com2  (.2_3)    0.302    0.023   13.031    0.000
#>     w3uniq1 (.3_3)    0.267    0.023   11.726    0.000
#>     w3uniq2 (.4_3)    0.248    0.027    9.207    0.000
#>   w4dep =~                                            
#>     w4com1  (.1_4)    0.236    0.015   15.552    0.000
#>     w4com2  (.2_4)    0.315    0.018   17.719    0.000
#>     w4uniq1 (.5_4)    0.285    0.013   21.158    0.000
#>     w4uniq2 (.6_4)    0.213    0.013   16.309    0.000
#>   w5dep =~                                            
#>     w5com1  (.1_5)    0.220    0.016   13.844    0.000
#>     w5com2  (.2_5)    0.325    0.018   18.066    0.000
#>     w5uniq1 (.5_5)    0.219    0.013   17.289    0.000
#>     w5uniq2 (.6_5)    0.179    0.013   13.872    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .w1com1 ~~                                           
#>    .w2com1            0.016    0.006    2.544    0.011
#>    .w3com1            0.002    0.005    0.406    0.685
#>    .w4com1            0.001    0.004    0.175    0.861
#>    .w5com1            0.004    0.005    0.908    0.364
#>  .w2com1 ~~                                           
#>    .w3com1            0.008    0.005    1.490    0.136
#>    .w4com1            0.003    0.004    0.702    0.483
#>    .w5com1            0.001    0.005    0.268    0.789
#>  .w3com1 ~~                                           
#>    .w4com1            0.005    0.004    1.351    0.177
#>    .w5com1            0.001    0.004    0.158    0.874
#>  .w4com1 ~~                                           
#>    .w5com1            0.004    0.003    1.313    0.189
#>  .w1com2 ~~                                           
#>    .w2com2            0.018    0.008    2.255    0.024
#>    .w3com2            0.011    0.008    1.446    0.148
#>    .w4com2           -0.005    0.006   -0.852    0.394
#>    .w5com2           -0.004    0.006   -0.752    0.452
#>  .w2com2 ~~                                           
#>    .w3com2            0.010    0.007    1.479    0.139
#>    .w4com2           -0.007    0.005   -1.338    0.181
#>    .w5com2            0.007    0.005    1.305    0.192
#>  .w3com2 ~~                                           
#>    .w4com2           -0.001    0.005   -0.101    0.920
#>    .w5com2           -0.004    0.005   -0.824    0.410
#>  .w4com2 ~~                                           
#>    .w5com2            0.014    0.004    3.433    0.001
#>  .w1uniq1 ~~                                          
#>    .w2uniq1           0.008    0.007    1.170    0.242
#>    .w3uniq1           0.003    0.007    0.389    0.697
#>  .w2uniq1 ~~                                          
#>    .w3uniq1           0.022    0.007    3.085    0.002
#>  .w1uniq2 ~~                                          
#>    .w2uniq2           0.026    0.009    2.868    0.004
#>    .w3uniq2           0.028    0.010    2.659    0.008
#>  .w2uniq2 ~~                                          
#>    .w3uniq2           0.044    0.009    4.677    0.000
#>  .w4uniq1 ~~                                          
#>    .w5uniq1           0.011    0.002    5.167    0.000
#>  .w4uniq2 ~~                                          
#>    .w5uniq2           0.007    0.002    2.945    0.003
#>   w1dep ~~                                            
#>     w2dep             0.556    0.055   10.197    0.000
#>     w3dep             0.553    0.058    9.568    0.000
#>     w4dep             0.288    0.060    4.799    0.000
#>     w5dep             0.285    0.062    4.584    0.000
#>   w2dep ~~                                            
#>     w3dep             0.570    0.053   10.718    0.000
#>     w4dep             0.244    0.059    4.103    0.000
#>     w5dep             0.271    0.061    4.475    0.000
#>   w3dep ~~                                            
#>     w4dep             0.442    0.055    8.086    0.000
#>     w5dep             0.417    0.058    7.222    0.000
#>   w4dep ~~                                            
#>     w5dep             0.629    0.038   16.557    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .w1com1  (.1_1)    1.440    0.019   75.353    0.000
#>    .w1com2  (.2_1)    1.420    0.025   57.312    0.000
#>    .w1uniq1 (.3_1)    1.420    0.022   63.981    0.000
#>    .w1uniq2 (.4_1)    1.410    0.025   55.960    0.000
#>    .w2com1  (.1_2)    1.390    0.020   70.588    0.000
#>    .w2com2  (.2_2)    1.400    0.022   62.996    0.000
#>    .w2uniq1 (.3_2)    1.350    0.022   61.140    0.000
#>    .w2uniq2 (.4_2)    1.420    0.024   60.037    0.000
#>    .w3com1  (.1_3)    1.420    0.017   82.781    0.000
#>    .w3com2  (.2_3)    1.410    0.022   63.419    0.000
#>    .w3uniq1 (.3_3)    1.420    0.022   65.680    0.000
#>    .w3uniq2 (.4_3)    1.510    0.025   59.808    0.000
#>    .w4com1  (.1_4)    1.210    0.017   72.590    0.000
#>    .w4com2  (.2_4)    1.290    0.020   63.405    0.000
#>    .w4uniq1 (.5_4)    1.220    0.016   74.581    0.000
#>    .w4uniq2 (.6_4)    1.180    0.015   80.904    0.000
#>    .w5com1  (.1_5)    1.250    0.017   74.983    0.000
#>    .w5com2  (.2_5)    1.320    0.020   65.143    0.000
#>    .w5uniq1 (.5_5)    1.190    0.014   84.041    0.000
#>    .w5uniq2 (.6_5)    1.160    0.014   85.446    0.000
#>     w1dep             0.000                           
#>     w2dep             0.000                           
#>     w3dep             0.000                           
#>     w4dep             0.000                           
#>     w5dep             0.000                           
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .w1com1  (.1_1)    0.113    0.009   12.642    0.000
#>    .w1com2  (.2_1)    0.140    0.014   10.024    0.000
#>    .w1uniq1 (.3_1)    0.092    0.011    8.100    0.000
#>    .w1uniq2 (.4_1)    0.185    0.015   12.216    0.000
#>    .w2com1  (.1_2)    0.103    0.009   11.878    0.000
#>    .w2com2  (.2_2)    0.095    0.010    9.268    0.000
#>    .w2uniq1 (.3_2)    0.115    0.010   10.985    0.000
#>    .w2uniq2 (.4_2)    0.126    0.012   10.702    0.000
#>    .w3com1  (.1_3)    0.073    0.007   11.147    0.000
#>    .w3com2  (.2_3)    0.102    0.011    9.590    0.000
#>    .w3uniq1 (.3_3)    0.112    0.010   10.885    0.000
#>    .w3uniq2 (.4_3)    0.188    0.015   12.465    0.000
#>    .w4com1  (.1_4)    0.053    0.004   11.993    0.000
#>    .w4com2  (.2_4)    0.063    0.006   10.836    0.000
#>    .w4uniq1 (.5_4)    0.024    0.003    7.393    0.000
#>    .w4uniq2 (.6_4)    0.038    0.003   11.693    0.000
#>    .w5com1  (.1_5)    0.060    0.005   11.861    0.000
#>    .w5com2  (.2_5)    0.055    0.006    8.800    0.000
#>    .w5uniq1 (.5_5)    0.031    0.003    9.774    0.000
#>    .w5uniq2 (.6_5)    0.040    0.003   11.895    0.000
#>     w1dep             1.000                           
#>     w2dep             1.000                           
#>     w3dep             1.000                           
#>     w4dep             1.000                           
#>     w5dep             1.000
```

``` r

pen_fit <- penalized_longcfa(
    ind_mat,
    lv_names = paste0("w", 1:5, "dep"),
    sample.cov = cov_matrix,
    sample.mean = means,
    sample.nobs = 392,
    lag_cov = TRUE,
    pen_fn = "l0a",
    w = 0.1
)
#> Warning in trans(x): NaNs produced
```

``` r

summary(pen_fit)
#> lavaan 0.6-21 ended normally after 439 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                       106
#> 
#>   Number of observations                           392
#> 
#> 
#> Parameter Estimates:
#> 
#> 
#> Latent Variables:
#>                    Estimate
#>   w1dep =~                 
#>     w1com1  (.1_1)    0.217
#>     w1com2  (.2_1)    0.293
#>     w1uniq1 (.3_1)    0.290
#>     w1uniq2 (.4_1)    0.272
#>   w2dep =~                 
#>     w2com1  (.1_2)    0.217
#>     w2com2  (.2_2)    0.293
#>     w2uniq1 (.3_2)    0.289
#>     w2uniq2 (.4_2)    0.273
#>   w3dep =~                 
#>     w3com1  (.1_3)    0.217
#>     w3com2  (.2_3)    0.293
#>     w3uniq1 (.3_3)    0.289
#>     w3uniq2 (.4_3)    0.272
#>   w4dep =~                 
#>     w4com1  (.1_4)    0.217
#>     w4com2  (.2_4)    0.292
#>     w4uniq1 (.5_4)    0.244
#>     w4uniq2 (.6_4)    0.185
#>   w5dep =~                 
#>     w5com1  (.1_5)    0.217
#>     w5com2  (.2_5)    0.294
#>     w5uniq1 (.5_5)    0.241
#>     w5uniq2 (.6_5)    0.185
#> 
#> Covariances:
#>                    Estimate
#>  .w1com1 ~~                
#>    .w2com1            0.015
#>    .w3com1            0.003
#>    .w4com1            0.001
#>    .w5com1            0.004
#>  .w2com1 ~~                
#>    .w3com1            0.008
#>    .w4com1            0.002
#>    .w5com1            0.001
#>  .w3com1 ~~                
#>    .w4com1            0.004
#>    .w5com1           -0.000
#>  .w4com1 ~~                
#>    .w5com1            0.005
#>  .w1com2 ~~                
#>    .w2com2            0.018
#>    .w3com2            0.015
#>    .w4com2           -0.005
#>    .w5com2           -0.006
#>  .w2com2 ~~                
#>    .w3com2            0.011
#>    .w4com2           -0.007
#>    .w5com2            0.006
#>  .w3com2 ~~                
#>    .w4com2           -0.001
#>    .w5com2           -0.007
#>  .w4com2 ~~                
#>    .w5com2            0.015
#>  .w1uniq1 ~~               
#>    .w2uniq1           0.008
#>    .w3uniq1           0.003
#>  .w2uniq1 ~~               
#>    .w3uniq1           0.021
#>  .w1uniq2 ~~               
#>    .w2uniq2           0.026
#>    .w3uniq2           0.025
#>  .w2uniq2 ~~               
#>    .w3uniq2           0.044
#>  .w4uniq1 ~~               
#>    .w5uniq1           0.011
#>  .w4uniq2 ~~               
#>    .w5uniq2           0.007
#>   w1dep ~~                 
#>     w2dep             0.590
#>     w3dep             0.526
#>     w4dep             0.319
#>     w5dep             0.284
#>   w2dep ~~                 
#>     w3dep             0.577
#>     w4dep             0.283
#>     w5dep             0.270
#>   w3dep ~~                 
#>     w4dep             0.496
#>     w5dep             0.405
#>   w4dep ~~                 
#>     w5dep             0.717
#> 
#> Intercepts:
#>                    Estimate
#>    .w1com1  (.1_1)    1.400
#>    .w1com2  (.2_1)    1.448
#>    .w1uniq1 (.3_1)    1.411
#>    .w1uniq2 (.4_1)    1.445
#>    .w2com1  (.1_2)    1.396
#>    .w2com2  (.2_2)    1.451
#>    .w2uniq1 (.3_2)    1.403
#>    .w2uniq2 (.4_2)    1.455
#>    .w3com1  (.1_3)    1.396
#>    .w3com2  (.2_3)    1.442
#>    .w3uniq1 (.3_3)    1.408
#>    .w3uniq2 (.4_3)    1.464
#>    .w4com1  (.1_4)    1.375
#>    .w4com2  (.2_4)    1.456
#>    .w4uniq1 (.5_4)    1.369
#>    .w4uniq2 (.6_4)    1.291
#>    .w5com1  (.1_5)    1.387
#>    .w5com2  (.2_5)    1.462
#>    .w5uniq1 (.5_5)    1.334
#>    .w5uniq2 (.6_5)    1.273
#>     w1dep             0.000
#>     w2dep            -0.144
#>     w3dep             0.042
#>     w4dep            -0.624
#>     w5dep            -0.584
#> 
#> Variances:
#>                    Estimate
#>    .w1com1  (.1_1)    0.111
#>    .w1com2  (.2_1)    0.145
#>    .w1uniq1 (.3_1)    0.100
#>    .w1uniq2 (.4_1)    0.183
#>    .w2com1  (.1_2)    0.104
#>    .w2com2  (.2_2)    0.097
#>    .w2uniq1 (.3_2)    0.110
#>    .w2uniq2 (.4_2)    0.131
#>    .w3com1  (.1_3)    0.073
#>    .w3com2  (.2_3)    0.111
#>    .w3uniq1 (.3_3)    0.110
#>    .w3uniq2 (.4_3)    0.189
#>    .w4com1  (.1_4)    0.054
#>    .w4com2  (.2_4)    0.061
#>    .w4uniq1 (.5_4)    0.026
#>    .w4uniq2 (.6_4)    0.038
#>    .w5com1  (.1_5)    0.062
#>    .w5com2  (.2_5)    0.065
#>    .w5uniq1 (.5_5)    0.027
#>    .w5uniq2 (.6_5)    0.040
#>     w1dep             1.000
#>     w2dep             1.084
#>     w3dep             0.917
#>     w4dep             1.289
#>     w5dep             0.965
# Penalized estimates of loadings and intercepts
(load_mat <- longcfa::get_lav_par_mat(pen_fit, "=~", ind_matrix = ind_mat))
#>           [,1]      [,2]      [,3]      [,4]      [,5]
#> [1,] 0.2165253 0.2168179 0.2168724 0.2170678 0.2170292
#> [2,] 0.2933352 0.2931462 0.2932684 0.2921349 0.2935263
#> [3,] 0.2897478 0.2886876 0.2890463        NA        NA
#> [4,] 0.2719914 0.2726679 0.2720269        NA        NA
#> [5,]        NA        NA        NA 0.2435006 0.2406786
#> [6,]        NA        NA        NA 0.1854648 0.1854557
(int_mat <- longcfa::get_lav_par_mat(pen_fit, "~1", ind_matrix = ind_mat))
#>          [,1]     [,2]     [,3]     [,4]     [,5]
#> [1,] 1.400136 1.396193 1.396458 1.375497 1.387389
#> [2,] 1.448369 1.450903 1.442467 1.455825 1.461692
#> [3,] 1.411214 1.402568 1.407927       NA       NA
#> [4,] 1.444567 1.454539 1.463549       NA       NA
#> [5,]       NA       NA       NA 1.368720 1.334150
#> [6,]       NA       NA       NA 1.291416 1.272949
# Effective number of loadings
eff_load_diff <- plavaan::composite_pair_loss(load_mat, fun = plavaan::l0a)
cat("Effective number of non-invariant loadings:", eff_load_diff, "\n")
#> Effective number of non-invariant loadings: 2.019139
# Effective number of intercepts
eff_int_diff <- plavaan::composite_pair_loss(int_mat, fun = plavaan::l0a)
cat("Effective number of non-invariant intercepts:", eff_int_diff, "\n")
#> Effective number of non-invariant intercepts: 2.50879
```
