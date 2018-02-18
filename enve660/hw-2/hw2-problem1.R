#======================================================================
#graph for hw 2 - number 1
#
# Tyler Bradley
# 1/28/2018
#======================================================================
#' Loading required libraries
library(tidyverse)
library(broom)

#' Creating dataset from problem
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

#' Plotting the log concentration of FFA vs time
ggplot(ffa_data, aes(t, ln_c_ffa)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()


#' Running linear model to get slope and intercept values.
ffa_model <- ffa_data %>% 
  nest() %>% 
  mutate(lm_model = map(data, ~lm(ln_c_ffa ~ t, data = .x)),
         lm_tidy = map(lm_model, tidy))

# Printing linear model coeffecients.
ffa_model %>% unnest(lm_tidy, .drop = TRUE) %>% knitr::kable()




