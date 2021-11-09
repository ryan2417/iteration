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

    ##  [1]  1.03505583 -0.68971949  0.33435048  0.01063235 -0.85666300 -0.38147473
    ##  [7]  0.89851360  0.10683464  0.35648983  0.85537020 -0.67699461 -1.95390504
    ## [13]  1.76225913 -0.38736907 -0.11251145 -0.92859653  1.39394626  0.82877036
    ## [19] -1.15301037  1.75766280  0.28824383 -1.35958736  0.49372586 -0.16372102
    ## [25] -1.45830251

``` r
z_scores <- function(x) {
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  1.03505583 -0.68971949  0.33435048  0.01063235 -0.85666300 -0.38147473
    ##  [7]  0.89851360  0.10683464  0.35648983  0.85537020 -0.67699461 -1.95390504
    ## [13]  1.76225913 -0.38736907 -0.11251145 -0.92859653  1.39394626  0.82877036
    ## [19] -1.15301037  1.75766280  0.28824383 -1.35958736  0.49372586 -0.16372102
    ## [25] -1.45830251

How great is this??

Only kinda great.

Let’s try again.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(x = y_vec)
```

    ##  [1] -1.27396150 -0.08961645 -2.52929005  0.64960949  2.03135980 -0.06949811
    ##  [7]  0.89446659  0.48227644 -0.13490140 -1.13666748  0.91524685 -0.19541555
    ## [13] -0.56716793  1.57084014  0.87752940 -1.30837294  0.04457950 -0.15243391
    ## [19] -0.30088047  0.84629423 -0.56602085  1.12121236 -0.41230539 -0.11789445
    ## [25] -0.57898833

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
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
    ## 1  4.71  4.76

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.9  4.39

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
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
    ## 1  1.72  2.59

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4) {
  
  # do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}
sim_mean_sd(n = 30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.1  2.94

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.63  3.58

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
dynamite_html = read_html(url)
review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()
review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()
review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()
reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
```

Okay but there are a lot of pages of reviews.

Write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  return(reviews)
  
  
}
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(base_url, 1:5)
bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  2 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  3 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  4 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  5 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  6 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  7 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  8 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  9 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ## 10 Painful                                               1.0 ou… "\n  I think I…
    ## # … with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}
x = 1
y = 2
f(x = y)
```

    ## [1] 4
