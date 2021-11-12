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
    ##  -4.626   2.912   5.107   5.038   7.088  14.825

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.626   2.912   5.107   5.038   7.088  14.825

``` r
l$summary
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.626   2.912   5.107   5.038   7.088  14.825

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
    ## 1  1.99 0.908

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
    ## 1  1.99 0.908
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.22  2.98
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.9  1.20
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1 0.461

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
    ##  [1]  2.0610732  2.2248288  3.0406672  1.7354963  2.0164252  1.9934229
    ##  [7]  1.6656660  3.5444229  1.6852502  1.7214003  3.9441778  2.1174505
    ## [13]  2.3912916  1.8144212  1.8769676  3.0829919  2.8725091  1.3635371
    ## [19]  2.8918079  1.8092030  2.1423780  1.3155584  1.3882684  2.8228962
    ## [25]  2.1230668  2.2733549  1.8390747  1.2508976  2.1651260  0.9567709
    ## [31]  3.3105462  2.2503138  1.4807950  0.8447770  2.9640026  1.9298063
    ## [37]  3.1380415  2.5069710  2.8922606  3.5172773  1.7072086  0.5122143
    ## [43] -0.2656807  0.3219523  0.4374517  0.1057158  1.9581713  1.4549565
    ## [49]  2.6090651  1.6398808
    ## 
    ## $b
    ##  [1]  7.9380340  8.6901803  7.4769665 10.6893120  6.2357684  0.5643410
    ##  [7] 10.4025369  8.3015635  0.3566865  1.6617699  4.2840524  8.1478899
    ## [13]  5.0970637  6.4018671  3.7585272  3.4314370  1.5743394  1.8100306
    ## [19]  9.1686139  3.3840536  9.4875356  6.2860624  3.3094394  7.6705627
    ## [25]  5.2208538  3.9518872  7.8232962  2.9754139  3.9211637  7.4254427
    ## [31]  5.4533437  6.1006276  8.9293321  6.3056872  6.7697967  1.4165648
    ## [37]  2.1752722 -0.1817342  9.4093117  5.1042218  3.0742239  2.3403765
    ## [43]  2.1266729  3.8738106  9.5185303  6.7336215 -0.2451639  2.3937310
    ## [49]  4.8866786  7.1722691
    ## 
    ## $c
    ##  [1] 19.16676 18.86169 19.86733 20.03553 21.80347 20.56021 19.05779 19.91906
    ##  [9] 19.97676 20.55317 18.95427 21.55990 19.92590 19.97858 18.65914 20.45071
    ## [17] 19.45746 23.37509 20.44339 19.84040 18.85995 19.21754 19.68155 20.39364
    ## [25] 19.40444 18.65804 20.20669 20.74103 19.13249 20.56504 20.95360 19.28673
    ## [33] 18.00570 23.33502 20.14895 19.52749 18.67298 19.08068 20.30793 19.45071
    ## [41] 19.84990 16.88054 17.68962 21.01493 18.71750 21.25337 18.82473 21.01153
    ## [49] 19.61086 19.69022
    ## 
    ## $d
    ##  [1] -11.62928 -12.16248 -11.21793 -10.55502 -12.33020 -11.54756 -12.53853
    ##  [8] -12.64062 -13.16173 -12.23014 -12.41640 -11.40101 -11.51416 -13.08640
    ## [15] -12.02942 -12.64994 -12.20467 -11.92645 -12.07004 -11.84650 -12.10834
    ## [22] -13.21646 -12.57617 -12.16137 -12.28716 -12.10051 -12.52198 -11.71865
    ## [29] -11.57578 -11.90206 -12.13106 -11.98723 -11.63242 -12.15944 -11.80321
    ## [36] -12.08153 -12.10458 -11.86479 -12.05811 -11.89552 -11.94471 -12.21767
    ## [43] -12.24631 -11.83028 -12.05411 -11.95667 -12.12719 -12.14840 -11.97921
    ## [50] -12.13897

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
    ## 1  1.99 0.908
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.22  2.98
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.9  1.20
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1 0.461

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
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 hehehehe                          5.0 out of 5 stars "\n  goodjobboys\n"     
    ##  2 Painful                           1.0 out of 5 stars "\n  I think I sneezed …
    ##  3 GRAND                             5.0 out of 5 stars "\n  GRAND\n"           
    ##  4 Hello, 90s                        5.0 out of 5 stars "\n  So nostalgic movie…
    ##  5 Cult Classic                      5.0 out of 5 stars "\n  Watched it with my…
    ##  6 Format was inaccurate             4.0 out of 5 stars "\n  There was an optio…
    ##  7 Good funny                        3.0 out of 5 stars "\n  Would recommend\n" 
    ##  8 Not available w/in 48 hour window 1.0 out of 5 stars "\n  I couldn't watch i…
    ##  9 Your mom went to college.         5.0 out of 5 stars "\n  Classic funny movi…
    ## 10 Very funny movie                  5.0 out of 5 stars "\n  I watch this movie…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  2 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  3 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  4 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  5 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ##  6 Okay                                        3.0 out of 5 stars "\n  Okay\n"  
    ##  7 A WHOLESOME comedic journey                 5.0 out of 5 stars "\n  Not a mo…
    ##  8 Hilarious                                   5.0 out of 5 stars "\n  Funny\n" 
    ##  9 Love it                                     5.0 out of 5 stars "\n  What of …
    ## 10 WORTH IT!                                   5.0 out of 5 stars "\n  It's the…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                         stars              text        
    ##    <chr>                                         <chr>              <chr>       
    ##  1 Funny movie.                                  5.0 out of 5 stars "\n  Great …
    ##  2 Best movie ever!                              5.0 out of 5 stars "\n  Got th…
    ##  3 I was stuck in the oil patch back in the day. 5.0 out of 5 stars "\n  I watc…
    ##  4 Funny Dork humor                              5.0 out of 5 stars "\n  Humor …
    ##  5 Still funny!                                  5.0 out of 5 stars "\n  Still …
    ##  6 Love it!! 💜                                  5.0 out of 5 stars "\n  Love i…
    ##  7 LOVE it                                       5.0 out of 5 stars "\n  cult c…
    ##  8 Perfect                                       5.0 out of 5 stars "\n  Exactl…
    ##  9 Love this movie!                              5.0 out of 5 stars "\n  Great …
    ## 10 Love it                                       5.0 out of 5 stars "\n  Love t…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                             stars              text                    
    ##    <chr>                             <chr>              <chr>                   
    ##  1 As described                      3.0 out of 5 stars "\n  Book is as describ…
    ##  2 GOSH!!!                           5.0 out of 5 stars "\n  Just watch the mov…
    ##  3 Watch it right now                5.0 out of 5 stars "\n  You need to watch …
    ##  4 At this point it’s an addiction   5.0 out of 5 stars "\n  I watch this movie…
    ##  5 💕                                5.0 out of 5 stars "\n  Hands down, one of…
    ##  6 Good dumb movie                   5.0 out of 5 stars "\n  I really wanted to…
    ##  7 funny                             5.0 out of 5 stars "\n  so funny and inven…
    ##  8 Best Movie- Try to prove me wrong 5.0 out of 5 stars "\n  Best movie ever\n" 
    ##  9 Vote For Pedro!!                  5.0 out of 5 stars "\n  What is NOT to lik…
    ## 10 So Funny                          5.0 out of 5 stars "\n  This is such a goo…

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
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## # … with 40 more rows
