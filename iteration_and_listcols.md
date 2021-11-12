Iteration\_and\_Listcols
================
Ruiqi Yan
11/9/2021

## Lists

``` r
l <- 
  list(
    vec_numeric = 5:8,
    vec_logical = c(TRUE, FALSE),
    summary = summary(rnorm(1000, mean = 5, sd = 3))
  )
l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.963   2.867   4.931   4.903   7.042  14.340

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.963   2.867   4.931   4.903   7.042  14.340

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -6.963   2.867   4.931   4.903   7.042  14.340

## Lists of normals

``` r
list_norms <-
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 0.5)
  )

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

mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.05 0.975

## for loop

Let’s use a for loop iteration over my list of normals

``` r
output <- vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] <- mean_and_sd(list_norms[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.05 0.975
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.70  3.37
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.0  1.23
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9 0.538

let’s use `map` instead

``` r
output <- map(list_norms, mean_and_sd)

output_2 <- map(list_norms, summary)

output_3 <- map_dbl(list_norms, median)
```

## List Columns!!!!

``` r
listcol_df <- 
  tibble(
    name = c("a", "b","c", "d"),
    norms = list_norms
  )

listcol_df %>% pull(norms)
```

    ## $a
    ##  [1]  2.4044172  3.4628249  1.5765395  3.3615566  1.0706256  3.1198219
    ##  [7]  2.8469249  2.4763589  2.9268907  2.1193010  2.1653475  1.4254399
    ## [13]  1.9620740  3.1347244  2.0320735  2.8052393  0.8102770  2.8978684
    ## [19]  0.6865431  0.6901030  2.4838818  1.5106203  0.9532675  2.3215267
    ## [25]  2.0285409  3.0196859  1.9021839  3.3269942  1.0127674  3.3029523
    ## [31]  0.5800075  0.2151057  2.9125792  1.2477420  2.2443933  2.9610184
    ## [37]  1.6933556  2.6938379  1.7028946  3.0040049  1.2418032  1.8011069
    ## [43]  3.1821951 -0.1149105  0.2388797  2.7731223  1.8726843  3.2913067
    ## [49]  0.5965155  2.4902072
    ## 
    ## $b
    ##  [1] -3.2810981  5.3073264  7.3844875  4.5137790  8.4246284  1.3270639
    ##  [7]  8.4911796  6.7891949  6.3866111  1.1102692  4.8635279  2.1781098
    ## [13]  4.5269362  5.0088384  7.3640775  6.6240798  6.3672840  3.9596556
    ## [19]  9.4466278  9.2877545  2.2169336  1.6834797  1.7181494  5.5199835
    ## [25]  4.8897553 -1.7087656  2.3395974  3.9529605  8.1473174  4.7790706
    ## [31]  2.7070620  3.6164224 -0.9707923  0.1414378  2.6479303  4.7189081
    ## [37]  9.8058735  7.8740106 -1.0033668  3.5745107  8.3140502  6.6426843
    ## [43]  6.6275038  8.6986489 -0.4779652  9.0883482 10.1468261  0.9585022
    ## [49]  8.7411422  3.7408989
    ## 
    ## $c
    ##  [1] 19.93649 20.34964 18.44713 18.98917 19.48422 21.22498 19.22328 20.85314
    ##  [9] 17.78327 18.18750 19.33478 20.95399 21.22544 21.53822 17.45723 20.61385
    ## [17] 20.44680 19.81224 21.93747 20.94108 19.33240 20.24177 19.42758 20.79026
    ## [25] 19.99008 20.59876 17.56812 18.56090 20.92871 21.04482 20.83024 17.45048
    ## [33] 18.78460 20.95447 19.81088 19.76345 21.85313 20.09408 20.57129 17.14253
    ## [41] 20.93472 21.92910 19.83895 19.43584 19.81655 21.04554 20.82197 21.11287
    ## [49] 20.66772 19.36628
    ## 
    ## $d
    ##  [1] -11.88138 -10.93682 -11.86094 -12.04168 -12.49851 -11.52565 -11.96845
    ##  [8] -11.92958 -11.48431 -12.14007 -12.73857 -12.30470 -10.87859 -11.43232
    ## [15] -12.36526 -12.75384 -11.42227 -12.13083 -12.18764 -11.96873 -12.42288
    ## [22] -11.53306 -12.29622 -12.61217 -11.38023 -12.32207 -11.49799 -11.27330
    ## [29] -11.16018 -12.29019 -11.82675 -12.17558 -11.33622 -11.68238 -12.11891
    ## [36] -11.25013 -11.35958 -11.77686 -12.06589 -11.49194 -11.64891 -10.51321
    ## [43] -12.81466 -11.87689 -12.54890 -12.04412 -12.41093 -12.87147 -12.25442
    ## [50] -11.35712

``` r
listcol_df %>% 
  mutate(
    summaries = map(norms, mean_and_sd)
  ) %>% 
  pull(summaries)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.05 0.975
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.70  3.37
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  20.0  1.23
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.9 0.538

## Nested Data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

Nest data within location

``` r
weather_nested <- nest(weather_df, data = date:tmin)

weather_nested %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
lm(tmax ~ tmin, data = weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nested$data[[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
weather_lm <- function(df) {
  lm(tmax ~ tmin, data = df)
}

weather_lm(weather_nested$data[[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nested$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nested %>% 
  mutate(
    lm_results = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
weather_nested %>% unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

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

map(urls, get_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
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
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  2 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  3 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  4 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  5 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  6 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ##  7 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi…
    ##  8 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie…
    ##  9 Watch it twice! Trust me!         5.0 out of 5 stars "\n  Nothing to dislike…
    ## 10 A classic                         5.0 out of 5 stars "\n  If you don’t enjoy…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  2 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  3 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  4 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  5 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ##  6 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  7 Love it                                     5.0 out of 5 stars "\n  What of …
    ##  8 WORTH IT!                                   5.0 out of 5 stars "\n  It's the…
    ##  9 Funny movie.                                5.0 out of 5 stars "\n  Great co…
    ## 10 Best movie ever!                            5.0 out of 5 stars "\n  Got this…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  2 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  3 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  4 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  5 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ##  6 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ##  7 Love this movie!                              5.0 out of 5 stars "\n  Great …
    ##  8 Love it                                       5.0 out of 5 stars "\n  Love t…
    ##  9 As described                                  3.0 out of 5 stars "\n  Book i…
    ## 10 GOSH!!!                                       5.0 out of 5 stars "\n  Just w…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  2 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  3 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  4 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  5 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  6 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  7 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ##  8 So Funny                          5.0 out of 5 stars "\n  This is such a goo…
    ##  9 Best movie ever                   5.0 out of 5 stars "\n  It's napoleon dyna…
    ## 10 Funny                             5.0 out of 5 stars "\n  Classic\n"

``` r
napolean_df <- 
  tibble(
    urls = urls
  )
napolean_df %>% 
  mutate(
    reviews = map(urls, get_page_reviews)
  ) %>% 
  select(reviews) %>% 
  unnest()
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
