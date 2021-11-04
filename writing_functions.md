writing\_functions
================
Ruiqi Yan
11/4/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)
y_vec = rnorm(25, mean = 10, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -1.73461885 -0.06581347  0.58671161 -0.21941696 -1.61283686 -0.24156607
    ##  [7]  0.74447804 -0.10623663 -1.93385343 -0.37654610 -0.15054231 -0.54666449
    ## [13] -0.60356284  0.75197388 -1.39530230 -0.73298649  1.03291632  0.14179939
    ## [19]  0.15848200  0.64389744  1.94837522  0.86990410  1.25687169  1.43939345
    ## [25]  0.14514365

``` r
z_scores <- function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -1.73461885 -0.06581347  0.58671161 -0.21941696 -1.61283686 -0.24156607
    ##  [7]  0.74447804 -0.10623663 -1.93385343 -0.37654610 -0.15054231 -0.54666449
    ## [13] -0.60356284  0.75197388 -1.39530230 -0.73298649  1.03291632  0.14179939
    ## [19]  0.15848200  0.64389744  1.94837522  0.86990410  1.25687169  1.43939345
    ## [25]  0.14514365

How great is it?

Let’s try again

``` r
z_scores <- function(x) {
  if(!is.numeric(x)){
    stop("Argument x should be numeric")
  }
  
  if(length(x) < 3){
    stop("Z scores cannot be computed for length 1 vectors")
  }
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): Z scores cannot be computed for length 1 vectors

``` r
z_scores("my", "name", "is", "jeff")
```

    ## Error in z_scores("my", "name", "is", "jeff"): unused arguments ("name", "is", "jeff")

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Argument x should be numeric

## Multiple outputs

``` r
mean_and_sd <- function(x) {
  if(!is.numeric(x)){
    stop("Argument x should be numeric")
  }
  
  if(length(x) < 3){
    stop("Z scores cannot be computed for length 1 vectors")
  }
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  output_df <- 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(output_df)
  
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.23  4.06

## Different smaple sizes, means, sds

``` r
sim_data <- tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.21  2.55

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4){
  
  sim_data <- tibble(
  x = rnorm(n, mean = mu, sd = sigma)
  )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.35  3.08

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.35  3.21
