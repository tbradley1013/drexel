#' loading in the required library
library(tidyverse)

#' creating the influent dataset
influent_data <- tibble::tribble(
  ~avg_Vs, ~count_inf_part,
  0.2, 511,
  0.6, 657, 
  1.0, 876, 
  1.4, 1168,
  1.8, 1460, 
  2.2, 1314, 
  2.6, 657,
  3.0, 438,
  3.4, 292,
  3.8, 292
) %>% 
  nest()

#' creating vector of different settling velocities
Vc <- seq(0.5, 4.0, by = 0.5)

#' combining the influent data for each of the critical velocities
influent_data <- crossing(Vc, influent_data)

#' calculating the fraction removed and the count of particles removed
#' for each settling velocity at each of the eight critical velocities
removal_data <-  influent_data %>% 
  mutate(
    data = map2(Vc, data, ~{
      .y %>% 
        mutate(frac_removed = avg_Vs/.x,
               frac_removed = if_else(frac_removed > 1, 1, frac_removed),
               count_removed = frac_removed*count_inf_part)
    })
  ) %>% 
  unnest(data)
  
#' calculating the removal effeciency for each of the eight critical 
#' velocities
removal_eff <- removal_data  %>%  
  group_by(Vc) %>% 
  summarize(removal_eff = sum(count_removed)/sum(count_inf_part))


knitr::kable(removal_data, format = "latex")

knitr::kable(removal_eff, format = "latex")


removal_eff %>% 
  ggplot(aes(Vc, removal_eff)) + 
  geom_point() + 
  geom_line() + 
  labs(
    title = "Removal effeciency as a function of overflow rate (critical velocity)",
    x = "Overflow rate (critical velocity) [=] m/h",
    y = "Removal effeciency"
  ) +
  theme_bw()



