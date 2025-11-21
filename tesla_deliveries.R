rm(list=ls())

library(tidyverse)
library(ggthemes)
library(fpp3)

tsla <- read_csv("https://jagelves.github.io/Data/tsla_deliveries.csv")

glimpse(tsla)

tail(tsla)

tsla %>% mutate(date=yearquarter(period)) %>% 
     select(date,deliveries) %>%
     as_tsibble(index=date) -> tsla_ts

tsla_ts %>% autoplot(deliveries) + theme_clean()

#Decomposition (only works with Tsibble)

tsla_ts %>%
     model(STL=STL(deliveries~trend(100))) %>% 
     components() %>% autoplot() + theme_clean()

#TSLM
tsla_ts %>% 
     model(TSLM=TSLM(deliveries~I(poly(trend(),6))+season())) -> fit
fit %>% coef()
fit %>% accuracy()

#Plot
tsla_ts %>% autoplot() + autolayer(fit %>% augment(),.fitted, col="orange", lwd=2)

