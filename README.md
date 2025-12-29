
<!-- README.md is generated from README.Rmd. Please edit that file -->

# prophetExt: An Extension for Prophet Time-Series Forecasting

<!-- badges: start -->

<!-- badges: end -->

The goal of prophetExt is to …

## Installation

You can install the development version of prophetExt like so:

``` r
remotes::install_github("hoxo-m/prophetExt")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
df <- read.csv("https://raw.githubusercontent.com/facebook/prophet/master/examples/example_wp_log_R_outliers2.csv")
head(df)
#>           ds        y
#> 1 2008-01-30 5.976351
#> 2 2008-01-16 6.049733
#> 3 2008-01-17 6.011267
#> 4 2008-01-14 5.953243
#> 5 2008-01-15 5.910797
#> 6 2008-01-12 5.407172
```

``` r
library(prophet)

model <- prophet(df)
df_future <- make_future_dataframe(model, 365)
fore <- predict(model, df_future)

plot(model, fore)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

``` r
library(prophetExt)

outliers <- detect_outliers(model)

plot(model, fore) + autolayer(outliers)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

``` r
calendar_plot(outliers)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
library(dplyr)

df_without_outliers <- df |> filter(!ds %in% outliers$ds)

model <- prophet(df_without_outliers)
fore <- predict(model, df_future)

plot(model, fore)
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->
