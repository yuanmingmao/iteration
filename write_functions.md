Iteration
================
Yuanming Mao

## Do sth. simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.74247 -1.94266  0.45876  0.67369 -1.19389  1.94672 -0.26522 -0.11694
    ##  [9] -0.21214  0.08664  0.17427 -0.50468 -0.66007  0.50729 -0.58243  0.58060
    ## [17]  1.05594 -0.50999  0.32374 -1.72369 -1.17760  2.18373 -0.89885 -0.26855
    ## [25]  0.76959  0.36072  1.96184  0.02756 -0.82575  0.51386

I want a function to compute z-scores

``` r
z_scores = function(x){
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at lease 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_scores(x_vec)
```

    ##  [1] -0.74247 -1.94266  0.45876  0.67369 -1.19389  1.94672 -0.26522 -0.11694
    ##  [9] -0.21214  0.08664  0.17427 -0.50468 -0.66007  0.50729 -0.58243  0.58060
    ## [17]  1.05594 -0.50999  0.32374 -1.72369 -1.17760  2.18373 -0.89885 -0.26855
    ## [25]  0.76959  0.36072  1.96184  0.02756 -0.82575  0.51386

Try my function on some other things. Last two should give errors.

``` r
z_scores(c(TRUE, TRUE, FALSE,TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

``` r
z_scores("my name is Yuanming")
```

    ## Error in z_scores("my name is Yuanming"): Input must be numeric

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at lease 3 numbers

## Multiple outputs

``` r
mean_and_sd = function(x){
  
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at lease 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

Check that the function works.

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0136 0.980

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.11  3.21

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4){
  sim_data = 
    tibble(
      x = rnorm(samp_size, mean = mu, sd = sigma)
    )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}

sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.05  3.14

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.27  4.22

## Let’s review Napolean Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about next page of review?

Let’s turn that code into a function

``` r
read_page_reviews = function(url){
  
  html = read_html(url)
  
  review_titles = 
  html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()

  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()

  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)

reviews
}
```

Let me try my function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                  stars text                                            
    ##    <chr>                  <dbl> <chr>                                           
    ##  1 Vote for Pedro!            5 Just watch the movie. Gosh!                     
    ##  2 Just watch the freaki…     5 Its a great movie, gosh!!                       
    ##  3 Great Value                5 Great Value                                     
    ##  4 I LOVE THIS MOVIE          5 THIS MOVIE IS SO FUNNY ONE OF MY FAVORITES      
    ##  5 Don't you wish you co…     5 Watch it 100 times. Never. Gets. Old.           
    ##  6 Stupid, but very funn…     5 If you like stupidly funny '90s teenage movies …
    ##  7 The beat                   5 The best                                        
    ##  8 Hilarious                  5 Super funny! Loved the online rental.           
    ##  9 Love this movie            5 We love this product.  It came in a timely mann…
    ## 10 Entertaining, limited…     4 Entertainment level gets a 5 star but having pr…

Let’s read a few pages for reviews.

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

all_reviews = 
  bind_rows(
    read_page_reviews(dynamite_urls[1]),
    read_page_reviews(dynamite_urls[2]),
    read_page_reviews(dynamite_urls[3]),
    read_page_reviews(dynamite_urls[4]),
    read_page_reviews(dynamite_urls[5])
  )
```

## Mean scoping example

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

## Functions as arguments

``` r
my_summary = function(x, sum_func) {
  sum_func(x)
}

x_vec = rnorm(100, 3, 7)

mean(x_vec)
```

    ## [1] 3.471

``` r
median(x_vec)
```

    ## [1] 3.375

``` r
my_summary(x_vec, mean)
```

    ## [1] 3.471
