#' ---
#' title: This is the work for ENVE 660 Final Exam
#' output: pdf_document
#' author: Tyler Bradley
#' date: 2018-03-11
#'---
library(tidyverse)

#' ##Probelm #4
#' Find k and n from the equation q = k*C^n^, given the follwoing data
data <- tribble(
  ~c, ~q,
  1.9, 6.2,
  21, 22.5,
  42.3, 33
) %>% 
  mutate(
    ln_q = log(q),
    ln_c = log(c)
  )

model <- lm(ln_q ~ ln_c, data = data) %>% 
  broom::tidy()

#' the intercept of the model gives log(k)
exp(model$estimate[[1]])

#' the slope gives n
model$estimate[[2]]



#' ##Problem #6
#' find k (and units)
data_6 <- tribble(
  ~t, ~Ca,
  0, 10, 
  1, 4.0, 
  2, 1.65
) %>% 
  mutate(
    ln_Ca = log(Ca), # log() function defaults to ln()
    Ca_inv = (1/Ca)
  )


#' Plotting zero, first, and second order relationships between Ca and t
#' as Ca vs t, ln(Ca) vs t, and (1/Ca) vs t, respectively.
#' Since the first order model is the closest to linear, I will you this
#' for the reaction term. 
data_6 %>% 
  gather(key = order, value = value, Ca:Ca_inv) %>% 
  mutate(order = factor(order, levels = c("Ca", "ln_Ca", "Ca_inv"),
                        labels = c("Ca vs t", "ln(Ca) vs t", "(1/Ca) vs t"))) %>% 
  ggplot(aes(t, value)) +
  facet_wrap(~ order, scales = "free") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

#' k is given by the slope of the linear model
lm(ln_Ca ~ t, data = data_6)$coefficients[[2]]

