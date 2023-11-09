linear regression
================
Aiying Huang
2023-11-09

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(p8105.datasets)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal()+theme(legend.position="bottom"))

options(
  ggplot2.continuous.color="viridis",
  ggplot2.continuous.fill="viridis"
)
scale_colour_discrete=scale_color_viridis_d
scale_fill_discrete=scale_fill_viridis_d

set.seed(1)
```

## Import data

``` r
data("nyc_airbnb")

nyc_airbnb=
  nyc_airbnb|>
  mutate(stars=review_scores_location/2)|>
  rename(
    borough=neighbourhood_group,
    neighborhood= neighbourhood)|>
  filter(borough!="Staten Island")|>
  select(price,stars,borough, neighborhood,room_type )
```

## Fit a model

``` r
nyc_airbnb|>
  ggplot(aes(x=stars,y=price,color=borough))+
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

<img src="linear_model_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Let’s fit the model we care about

``` r
fit=lm(price~stars+borough,data=nyc_airbnb)
```

let’s look at the result…

let’s look at the results better..

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
broom::tidy(fit)|>
  select(-std.error,-statistic)|>
  mutate(
    term=str_replace(term,"borough","Bourough!")
  )|>
  knitr::kable(digits=3)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |  -70.414 |   0.000 |
| stars              |   31.990 |   0.000 |
| Bourough!Brooklyn  |   40.500 |   0.000 |
| Bourough!Manhattan |   90.254 |   0.000 |
| Bourough!Queens    |   13.206 |   0.145 |

## Be in control of factors

``` r
nyc_airbnb=
  nyc_airbnb|>
  mutate(
    borough=fct_infreq(borough), #order the borough in frequency
    room_type=fct_infreq(room_type )
  )
```

look at the plot again

``` r
nyc_airbnb|>
  ggplot(aes(x=stars,y=price,color=borough))+
  geom_point()
```

    ## Warning: Removed 9962 rows containing missing values (`geom_point()`).

<img src="linear_model_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

``` r
fit=lm(price~stars+borough,data=nyc_airbnb)
broom::tidy(fit)#every borough now compare to manattan
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
broom::glance(fit)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df   logLik    AIC    BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>    <dbl>  <dbl>  <dbl>
    ## 1    0.0342        0.0341  182.      271. 6.73e-229     4 -202113. 4.04e5 4.04e5
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>
