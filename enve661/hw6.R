library(tidyverse)

data <- tribble(
  ~c, ~q,
  15.7, 1246,
  1.27, 489,
  .396, 298, 
  .225, 250, 
  .161, 213
) %>% 
  mutate(`c/q` = c/q)


freundlich <- lm(`c/q` ~ c, data = data)
summary(freundlich)

langmuir <- lm(log(q) ~ log(c), data = data)
summary(langmuir)
