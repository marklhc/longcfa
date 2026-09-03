# Standard Errors with Penalized Estimation

``` r

library(longcfa)
library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
library(plavaan)
data(PoliticalDemocracy)
```

## Penalize cross-loadings

### Two-factor CFA model

``` r

mod0 <- "
  ind60 =~ x1 + x2 + x3
  dem60 =~ y1 + y2 + y3 + y4
  ind60 ~~ dem60
"
fit0 <- cfa(mod0, data = PoliticalDemocracy, std.lv = TRUE, estimator = "MLR")
```

Penalized

``` r

mod <- "
  ind60 =~ x1 + x2 + x3 + y1 + y2 + y3 + y4
  dem60 =~ x1 + x2 + x3 + y1 + y2 + y3 + y4
  ind60 ~~ ind60
"
fit <- cfa(mod, data = PoliticalDemocracy, std.lv = TRUE, do.fit = FALSE)
```

``` r

pefa_fit <- penalized_est(
    fit,
    w = .03,
    pen_par_id = 4:10,
    se = "robust.huber.white"
)
summary(pefa_fit)
#> lavaan 0.7-2 ended normally after 128 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        22
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
#>   ind60 =~                                            
#>     x1                0.657    0.055   11.934    0.000
#>     x2                1.458    0.107   13.680    0.000
#>     x3                1.221    0.104   11.707    0.000
#>     y1                0.002    0.008    0.278    0.781
#>     y2               -0.005    0.007   -0.787    0.431
#>     y3                0.003    0.007    0.505    0.614
#>     y4                0.423    0.330    1.280    0.201
#>   dem60 =~                                            
#>     x1                0.027    0.026    1.049    0.294
#>     x2               -0.001    0.014   -0.096    0.923
#>     x3               -0.011    0.017   -0.682    0.495
#>     y1                2.129    0.212   10.020    0.000
#>     y2                2.980    0.292   10.206    0.000
#>     y3                2.314    0.326    7.103    0.000
#>     y4                2.752    0.289    9.520    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   ind60 ~~                                            
#>     dem60             0.394    0.121    3.248    0.001
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     ind60             1.000                           
#>    .x1                0.081    0.018    4.507    0.000
#>    .x2                0.120    0.073    1.648    0.099
#>    .x3                0.463    0.084    5.525    0.000
#>    .y1                2.228    0.545    4.085    0.000
#>    .y2                6.458    1.417    4.558    0.000
#>    .y3                5.229    1.165    4.488    0.000
#>    .y4                2.341    0.729    3.212    0.001
#>     dem60             1.000
```

``` r

# Quick simulation to check SEs
set.seed(1234)
R <- 250
est_res <- matrix(NA, nrow = R, ncol = length(coef(pefa_fit)))
se_res <- matrix(NA, nrow = R, ncol = length(coef(pefa_fit)))

# Use the simple structure model as population
pop_model <- parTable(pefa_fit)

for (i in 1:R) {
    # Simulate data
    dat_sim <- simulateData(pop_model, sample.nobs = 200)

    # Fit penalized model
    fit_sim <- cfa(mod, data = dat_sim, std.lv = TRUE, do.fit = FALSE)
    pefa_sim <- try(
        penalized_est(
            fit_sim,
            w = .03,
            pen_par_id = 4:10,
            se = "robust.huber.white"
        ),
        silent = TRUE
    )

    if (!inherits(pefa_sim, "try-error")) {
        est_res[i, ] <- coef(pefa_sim)
        se_res[i, ] <- sqrt(diag(vcov(pefa_sim)))
    }
}

# Compare empirical SD vs mean SE for a few parameters
# (e.g., first few loadings)
res_summary <- data.frame(
    param = names(coef(pefa_fit)),
    emp_sd = apply(est_res, 2, sd, na.rm = TRUE),
    mean_se = apply(se_res, 2, mean, na.rm = TRUE)
)
```

``` r

print(res_summary, digits = 2)
#>           param emp_sd mean_se
#> 1     ind60=~x1 0.0380  0.0393
#> 2     ind60=~x2 0.0750  0.0772
#> 3     ind60=~x3 0.0787  0.0777
#> 4     ind60=~y1 0.0291  0.0060
#> 5     ind60=~y2 0.0704  0.0087
#> 6     ind60=~y3 0.0532  0.0069
#> 7     ind60=~y4 0.2631  0.1266
#> 8     dem60=~x1 0.0154  0.0152
#> 9     dem60=~x2 0.0093  0.0091
#> 10    dem60=~x3 0.0109  0.0106
#> 11    dem60=~y1 0.1586  0.1575
#> 12    dem60=~y2 0.2616  0.2445
#> 13    dem60=~y3 0.2160  0.2088
#> 14    dem60=~y4 0.2340  0.2057
#> 15       x1~~x1 0.0106  0.0116
#> 16       x2~~x2 0.0388  0.0436
#> 17       x3~~x3 0.0540  0.0536
#> 18       y1~~y1 0.3085  0.3133
#> 19       y2~~y2 0.8018  0.7797
#> 20       y3~~y3 0.5965  0.6039
#> 21       y4~~y4 0.4302  0.4333
#> 22 ind60~~dem60 0.0763  0.0686
```

``` r

# meat <- lavInspect(pefa_fit, "information.first.order")
# bread <- attr(pefa_fit, "hessian")
# vc_pefa <- solve(bread) %*% meat %*% solve(bread) / 75
# pefa_fit@vcov$vcov <- vc_pefa
# se_pefa <- sqrt(diag(vc_pefa))
# pefa_fit@ParTable$se <- 0 * pefa_fit@ParTable$est
# pefa_fit@ParTable$se[which(pefa_fit@ParTable$free > 0)] <- se_pefa
# cbind(coef(pefa_fit), sqrt(diag(vc_pefa)))
```

## Penalize non-invariance

``` r

ind_mat <- matrix(
    c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
    nrow = 4
)
lconfig_mod_un <- longcfa_syntax(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    lag_cov = TRUE,
    free_latvars = TRUE,
    free_latmeans = TRUE
)
# Specify the under-identified model
lconfig_fit_un <- cfa(
    lconfig_mod_un,
    data = PoliticalDemocracy,
    do.fit = FALSE,
    std.lv = TRUE,
    missing = "fiml",
    estimator = "mlr"
)
ld_id <- get_lav_par_id(lconfig_fit_un, "=~", ind_mat)
int_id <- get_lav_par_id(lconfig_fit_un, "~1", ind_mat)
pen_fit <- penalized_est(
    lconfig_fit_un,
    w = 0.03,
    pen_fn = "l0a",
    pen_diff_id = list(loadings = t(ld_id), intercepts = t(int_id)),
    se = "robust.huber.white"
)
```

Compared to scalar invariance model

``` r

lscalar_fit <- longcfa(
    ind_mat,
    lv_names = c("dem60", "dem65"),
    data = PoliticalDemocracy,
    lag_cov = TRUE,
    long_equal = c("loadings", "intercepts"),
    se = "robust.huber.white"
)
summary(lscalar_fit)
#> lavaan 0.7-2 ended normally after 46 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        31
#>   Number of equality constraints                     8
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                33.131
#>   Degrees of freedom                                21
#>   P-value (Chi-square)                           0.045
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
#>     y1       (.l1)    2.085    0.211    9.884    0.000
#>     y2       (.l2)    2.896    0.284   10.189    0.000
#>     y3       (.l3)    2.552    0.246   10.366    0.000
#>     y4       (.l4)    2.889    0.225   12.842    0.000
#>   dem65 =~                                            
#>     y5       (.l1)    2.085    0.211    9.884    0.000
#>     y6       (.l2)    2.896    0.284   10.189    0.000
#>     y7       (.l3)    2.552    0.246   10.366    0.000
#>     y8       (.l4)    2.889    0.225   12.842    0.000
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>  .y1 ~~                                               
#>    .y5                0.845    0.433    1.953    0.051
#>  .y2 ~~                                               
#>    .y6                1.670    0.934    1.788    0.074
#>  .y3 ~~                                               
#>    .y7                1.206    0.646    1.868    0.062
#>  .y4 ~~                                               
#>    .y8                0.261    0.465    0.563    0.574
#>   dem60 ~~                                            
#>     dem65             0.918    0.057   16.191    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1       (.i1)    5.508    0.289   19.037    0.000
#>    .y2       (.i2)    3.796    0.429    8.856    0.000
#>    .y3       (.i3)    6.670    0.353   18.915    0.000
#>    .y4       (.i4)    4.554    0.366   12.458    0.000
#>    .y5       (.i1)    5.508    0.289   19.037    0.000
#>    .y6       (.i2)    3.796    0.429    8.856    0.000
#>    .y7       (.i3)    6.670    0.353   18.915    0.000
#>    .y8       (.i4)    4.554    0.366   12.458    0.000
#>     dem60             0.000                           
#>     dem65            -0.210    0.067   -3.115    0.002
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .y1      (.1_1)    2.141    0.488    4.384    0.000
#>    .y2      (.2_1)    6.840    1.444    4.738    0.000
#>    .y3      (.3_1)    5.424    1.101    4.927    0.000
#>    .y4      (.4_1)    2.623    0.641    4.091    0.000
#>    .y5      (.1_2)    2.824    0.577    4.895    0.000
#>    .y6      (.2_2)    4.032    0.818    4.931    0.000
#>    .y7      (.3_2)    3.650    0.639    5.713    0.000
#>    .y8      (.4_2)    2.482    0.705    3.522    0.000
#>     dem60             1.000                           
#>     dem65             0.947    0.097    9.745    0.000
```
