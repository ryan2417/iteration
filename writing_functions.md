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

    ##  [1] -1.22476387  0.08388575 -0.68737761  1.90496436  0.05150947 -1.16132145
    ##  [7]  0.34210454  1.12100931  0.56463443  0.01314564 -1.21334459 -0.58715441
    ## [13] -1.78985749  1.23129924  1.54118162 -0.32115546  0.31703701  0.57653063
    ## [19]  0.36634887 -0.03919370  0.50203599 -2.12110084  0.44371237  0.68019003
    ## [25] -0.59431983

``` r
z_scores <- function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1] -1.22476387  0.08388575 -0.68737761  1.90496436  0.05150947 -1.16132145
    ##  [7]  0.34210454  1.12100931  0.56463443  0.01314564 -1.21334459 -0.58715441
    ## [13] -1.78985749  1.23129924  1.54118162 -0.32115546  0.31703701  0.57653063
    ## [19]  0.36634887 -0.03919370  0.50203599 -2.12110084  0.44371237  0.68019003
    ## [25] -0.59431983

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
    ## 1  4.78  3.70
