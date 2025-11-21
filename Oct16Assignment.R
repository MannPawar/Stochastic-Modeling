rm(list = ls())

library(tidyverse)
library(fpp3)
library(imputeTS)

avo <- read_csv("https://jagelves.github.io/Data/Avocado2020_2024P.csv")

new <- tibble(
     Week = c("1/3/21", "1/3/21"),
     Type = c("Organic", "Conventional"),
     Geography = c("Richmond/Norfolk", "Richmond/Norfolk"),
     Average_Price = c(1.055744, 0.9048261)
)

avo <- bind_rows(avo, new)

conventional_richmond <- avo %>%
     filter(Type == "Conventional" & Geography == "Richmond/Norfolk")

mean_price <- mean(conventional_richmond$Average_Price)

filtered_avo <- conventional_richmond %>%
     mutate(date = mdy(Week)) %>%
     arrange(date)

date_diffs <- diff(filtered_avo$date)

num_adjustments <- sum(date_diffs == 6)

avo_ts <- filtered_avo %>%
     mutate(week_index = yearweek(date)) %>%
     group_by(week_index) %>%
     summarise(Average_Price = mean(Average_Price, na.rm = TRUE), .groups = "drop") %>%
     as_tsibble(index = week_index) %>%
     fill_gaps() %>%
     mutate(Average_Price = na_interpolation(Average_Price))

decomp <- avo_ts %>%
     model(STL(Average_Price ~ trend() + season())) %>%
     components()

fit <- avo_ts %>%
     model(Mean = MEAN(Average_Price),
           TSLM = TSLM(Average_Price ~ trend()))


accuracy(fit)

fc <- forecast(fit, h = 5)
