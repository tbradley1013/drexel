#================================================================
# graph for hw 2 problem 4
#
# Tyler Bradley
# 1/28/2018
#================================================================

# loading required libraries
library(tidyverse)
library(knitr)
library(kableExtra)

#' Part a
#' Defining constants
a <- 4
b <- 1
k <- 1054.08 # [=] $M^{-1} * days^{-1}$
cb_o = 6.25e-5 # [=] M
ca_o = 9.101e-6 # [=] M


#' Creating data set
#' $C_{Mn}$ is calculated from formula derived in problem
mn_data <- tibble(t_days = seq(0, 60, by = 1)) %>%
  mutate(c_mn_mol = ((ca_o - (a / b) * cb_o) / (
    1 - (a / b) * (cb_o / ca_o) * exp(-(((
      b * ca_o
    ) / (
      a * cb_o
    )) - 1) * cb_o * k * t_days)
  )),
  c_mn_ug = c_mn_mol * 54.938 * 1e6)

#' Plotting the decay equation
ggplot(mn_data, aes(t_days, c_mn_ug)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 500, by = 20))



#' Part b
#' $CO_{2}$ is in great excess so $r_{mn}$ becomes a psuedo-first order equation
k_star <- k * cb_o

mn_data_b <- tibble(t_days = seq(0, 60, by = 1)) %>% 
  mutate(c_mn_mol = ca_o * exp(-k_star*t_days),
         c_mn_ug = c_mn_mol * 54.938 * 1e6)


#' Plotting decay rxn as psuedo-first order
ggplot(mn_data_b, aes(t_days, c_mn_ug)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 500, by = 20))

