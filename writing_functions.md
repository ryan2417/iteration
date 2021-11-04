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

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.10598106  0.78393333 -2.16489664 -0.07259915 -0.92255246  0.71371123
    ##  [7] -0.70325305 -0.08437699 -1.15779330  2.50272114 -0.60670407 -0.84913216
    ## [13]  0.39620187 -1.33050374  0.81223559  1.12936804 -0.47443869 -0.14223516
    ## [19]  0.65648263 -0.11195306 -0.17132348 -0.46124075 -0.67026613  0.83637672
    ## [25]  0.98625721

``` r
z_scores <- function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  1.10598106  0.78393333 -2.16489664 -0.07259915 -0.92255246  0.71371123
    ##  [7] -0.70325305 -0.08437699 -1.15779330  2.50272114 -0.60670407 -0.84913216
    ## [13]  0.39620187 -1.33050374  0.81223559  1.12936804 -0.47443869 -0.14223516
    ## [19]  0.65648263 -0.11195306 -0.17132348 -0.46124075 -0.67026613  0.83637672
    ## [25]  0.98625721

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
