#===================================================================
# This script contains work for ENVE 660 Midterm 
# Question #5
#
# Tyler Bradley
# 2018-02-09
#====================================================================

#' Reading in required libraries
library(tidyverse)

#' Writing in experimental data
df <-tribble(
  ~t, ~Ca,
  0,  167,
  1,  16.1,
  2, 8.5
)

#' Plotting zero, first, and second order relationships as Ca vs t, 
#' ln(Ca) vs t, and (1/Ca) vs t, respectively
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

#' Linear model fit to (1/Ca) vs t since it has the best fit
model <- df %>% 
  mutate(Ca_inv = 1/Ca) %>% 
  lm(Ca_inv ~ t, data = .) 

#' getting the model coefficients 
#' k = `r broom::tidy(model)$estimate[[2]]`
broom::tidy(model)
broom::glance(model)
