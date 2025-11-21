rm(list=ls())

library(tidyverse)

avocado <- read_csv("avocado2020.csv")
glimpse(avocado)

regional_avocado <- avocado %>%
     filter(geography %in% c("West", "Northeast", "Great Lakes"))

regional_avocado %>% group_by(geography, type) %>%
     summarize(mean_price = mean(average_price))

regional_avocado %>% mutate(value = average_price*total_volume) %>% 
     select(value, year, type, geography) %>% 
     filter(type == "organic", geography == "Northeast") %>% 
     group_by(year) %>% 
     summarize(mean_value = mean(value), 
               sd_value = sd(value),
               count = n())

library(gt)

y_column <- c(4.2342,10.3444,3.0093,6.367,20.659)
x_column <- seq(5)
z <- bind_cols(x = x_column, y = y_column)
z

z %>% gt() %>% 
     tab_header(title = md("**The z variable**"),
                          subtitle = md("A mystery...")) %>% 
     fmt_number(columns=y, decimals=2) %>% 
     tab_footnote(footnote = md("*source: Unknown*")) %>%
     tab_style(style = cell_fill(color = "#B2D3C2"),
               locations = cells_body(rows = y == min(y)))
