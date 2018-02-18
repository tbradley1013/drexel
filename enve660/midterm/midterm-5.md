

```r
#===================================================================
# This script contains work for ENVE 660 Midterm 
# Question #5
#
# Tyler Bradley
# 2018-02-09
#====================================================================
```

Reading in required libraries


```r
library(tidyverse)
```

Writing in experimental data


```r
df <-tribble(
  ~t, ~Ca,
  0,  167,
  1,  16.1,
  2, 8.5
)
```

Plotting zero, first, and second order relationships as Ca vs t, 
ln(Ca) vs t, and (1/Ca) vs t, respectively


```r
df %>% 
  mutate(
    ln_Ca = log(Ca), # log() function defaults to ln()
    Ca_inv = (1/Ca)
  ) %>% 
  gather(key = order, value = value, Ca:Ca_inv) %>% 
  mutate(order = factor(order, levels = c("Ca", "ln_Ca", "Ca_inv"),
                        labels = c("Ca", "ln(Ca)", "(1/Ca)"))) %>% 
  ggplot(aes(t, value)) +
  facet_wrap(~ order, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

Linear model fit to (1/Ca) vs t since it has the best fit


```r
model <- df %>% 
  mutate(Ca_inv = 1/Ca) %>% 
  lm(Ca_inv ~ t, data = .) 
```

getting the model coefficients 
k = 0.0558295


```r
broom::tidy(model)
```

```
##          term    estimate    std.error statistic     p.value
## 1 (Intercept) 0.006086111 0.0002193283  27.74886 0.022932275
## 2           t 0.055829517 0.0001698910 328.61962 0.001937248
```

```r
broom::glance(model)
```

```
##   r.squared adj.r.squared        sigma statistic     p.value df   logLik
## 1 0.9999907     0.9999815 0.0002402622  107990.9 0.001937248  2 22.39244
##         AIC       BIC     deviance df.residual
## 1 -38.78488 -41.48905 5.772591e-08           1
```

