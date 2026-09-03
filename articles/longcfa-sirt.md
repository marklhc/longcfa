# Approximate Longitudinal Invariance With Alignment Optimization

``` r

library(longcfa)
library(lavaan)
#> This is lavaan 0.7-2
#> lavaan is FREE software! Please report any bugs.
library(sirt)
#> - sirt 4.2-133 (2025-09-27 12:57:51)
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
lconfig_fit <- cfa(
    lconfig_mod,
    data = PoliticalDemocracy,
    std.lv = TRUE,
    meanstructure = TRUE
)
lconfig_fit
#> lavaan 0.7-2 ended normally after 19 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        25
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                45.070
#>   Degrees of freedom                                19
#>   P-value (Chi-square)                           0.001
```

``` r

ind_mat <- matrix(
    c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8"),
    nrow = 4
)
```

## Alignment optimization

The idea of alignment optimization ([Asparouhov & Muthén,
2014](https://www.statmodel.com/examples/webnotes/webnote18_3.pdf)) is
to find, among all solutions that have the same fit as the configural
invariance model, approximate measurement invariance by minimizing the
amount of non-invariance in the model parameters.

The package does not directly import functions from `sirt`, but instead
contains some helper functions to help prepare inputs to pass to
[`sirt::invariance.alignment()`](https://rdrr.io/pkg/sirt/man/invariance.alignment.html),
as shown below.

``` r

ld_mat <- get_lav_par_mat(lconfig_fit, op = "=~", ind_matrix = ind_mat)
int_mat <- get_lav_par_mat(lconfig_fit, op = "~1", ind_matrix = ind_mat)
aligned <- invariance.alignment(t(ld_mat), t(int_mat), meth = 2)
aligned_fit <- update_ustart(
    lconfig_fit,
    c(18, 29),
    c(aligned$pars$psi0[2]^2, aligned$pars$alpha0[2]),
    se = "none" # SE not trustworthy after alignment with arbitrary constraints
)
#> Warning: lavaan->lav_step02_options():  
#>    the following argument(s) override(s) the options in slot_options: se
```

### Two-stage estimation

``` r

fs_aligned <- lavPredict(aligned_fit, method = "Bartlett", acov = TRUE)
colnames(fs_aligned) <- paste0("fs_", colnames(fs_aligned))
attr(fs_aligned, "acov")
#> [[1]]
#>              dem60        dem65
#> dem60 1.294956e-01 1.221245e-15
#> dem65 1.443290e-15 1.084581e-01
step2_fit <- cfa(
    "dem60 =~ 1 * fs_dem60
   dem65 =~ 1 * fs_dem65
   fs_dem60 ~~ 1.294956e-01 * fs_dem60
   fs_dem65 ~~ 1.084581e-01 * fs_dem65
   dem60 ~~ dem65",
    data = fs_aligned,
    meanstructure = TRUE
)
summary(step2_fit)
#> lavaan 0.7-2 ended normally after 12 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                         5
#> 
#>   Number of observations                            75
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
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
#>     fs_dem60          1.000                           
#>   dem65 =~                                            
#>     fs_dem65          1.000                           
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   dem60 ~~                                            
#>     dem65             0.913    0.161    5.668    0.000
#> 
#> Intercepts:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .fs_dem60          0.000    0.123    0.000    1.000
#>    .fs_dem65         -0.139    0.115   -1.217    0.224
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>    .fs_dem60          0.129                           
#>    .fs_dem65          0.108                           
#>     dem60             1.000    0.184    5.422    0.000
#>     dem65             0.876    0.161    5.449    0.000
```

### Alignment with an approximate \\L_0\\ loss function

``` r

aligned_l0a <- invariance.alignment(
    t(ld_mat),
    t(int_mat),
    align.pow = c(0, 0),
    meth = 2
)
aligned_l0a$pars
#>        alpha0      psi0
#> G1  0.0000000 1.0000000
#> G2 -0.1387832 0.9329944
```

The results are quite similar to those obtained from the ALF loss
function.
