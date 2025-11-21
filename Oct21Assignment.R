rm(list=ls())

library(tidyverse)
library(tsibble)
library(fpp3)
library(ggthemes)

tesla_data <- read_csv('http://jagelves.github.io/Data/teslaE.csv')

tesla_data %>% mutate(Date=yearquarter(Date)) %>% 
     as_tsibble(index=Date) -> ele_ts

train_ele<-filter_index(.data=ele_ts,.~"2023 Q4")
test_ele<-filter_index(.data=ele_ts,"2024 Q1"~.)

train_ele %>% 
     model(TREND=TSLM(EnergyStorage~trend()),
           TRENDS=TSLM(EnergyStorage~trend()+season()),
           TRENDS2=TSLM(EnergyStorage~trend()+
                            I(trend()^2)+season())) -> fit
fit %>% coef()

fit %>% forecast(test_ele) %>%
     accuracy(ele_ts, list(winkler = winkler_score)) -> wink
print(wink)

# Information Criterion
fit %>% augment() %>% 
     transmute(SE=.resid^2) %>% pull(SE) %>% sum() -> sse

fit %>% glance() %>% select(.model, AIC:BIC)

# Cross Validation
ele_ts %>% stretch_tsibble(.init = 10, .step=1) %>%
     model(TREND=TSLM(EnergyStorage~trend()),
           TRENDS=TSLM(EnergyStorage~trend()+season()),
           TRENDS2=TSLM(EnergyStorage~trend()+
                             I(trend()^2)+season())) %>%
     forecast(h=1) %>%
     accuracy(ele_ts)  
