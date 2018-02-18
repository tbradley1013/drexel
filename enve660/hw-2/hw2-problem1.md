

```r
#======================================================================
#graph for hw 2 - number 1
#
# Tyler Bradley
# 1/28/2018
#======================================================================
```

loading required libraries


```r
library(tidyverse)
library(broom)
```

Creating dataset from problem


```r
ffa_data <- tribble(
  ~t,   ~c_ffa,
  0,     4e-5,
  10,    2.43e-5,
  20,    1.48e-5,
  30,    8.98e-6,
  40,    5.46e-6,
  50,    3.32e-6,
  60,    2.02e-6
) %>% 
  # adding ln(C_ffa) column
  # log() function in R defaults to ln
  mutate(ln_c_ffa = log(c_ffa))
```

Plotting the log concentration of FFA vs time


```r
ggplot(ffa_data, aes(t, ln_c_ffa)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Running linear model to get slope and intercept values.
Also generating model fit summaries, even though graph shows clear
linear relationship.


```r
ffa_model <- ffa_data %>% 
  nest() %>% 
  mutate(
    lm_model = map(
      data,
      ~lm(ln_c_ffa ~ t, data = .x)
    ),
    lm_tidy = map(lm_model, tidy),
    lm_glance = map(lm_model, glance)
  )

# Printing linear model coeffecients.
ffa_model %>% unnest(lm_tidy, .drop = TRUE)
```

```
## # A tibble: 2 x 5
##   term        estimate std.error statistic                   p.value
##   <chr>          <dbl>     <dbl>     <dbl>                     <dbl>
## 1 (Intercept) -10.1    0.000531     -19087 0.00000000000000000000749
## 2 t           - 0.0498 0.0000147    - 3382 0.0000000000000000429
```

Printing linear model summary statistics.


```r
ffa_model %>% unnest(lm_glance, .drop = TRUE)
```

```
## # A tibble: 1 x 11
##   r.squared adj.r.squared    sigma statistic    p.value    df logLik   AIC
##       <dbl>         <dbl>    <dbl>     <dbl>      <dbl> <int>  <dbl> <dbl>
## 1     1.000         1.000 0.000779  11439540   4.29e⁻¹⁷     2   41.4 -76.7
## # ... with 3 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>
```

