rm(list=ls())

library(tidyverse)
library(fpp3)
library(ggthemes)

Netflix <- read_csv("https://jagelves.github.io/Data/Netflix.csv")

Netflix %>% mutate(Quarter = yearquarter(Quarter)) %>% select(Quarter,Subscribers) %>% as_tsibble(index=Quarter) -> Netflix_ts

Netflix_ts %>%
     autoplot() +
     labs(title = "Netflix World Wide Subscribers (Millions)",
          subtitle = "2013 Q1 to 2024 Q2")

train_net<-filter_index(.data=Netflix_ts,.~"2021 Q4")
test_net<-filter_index(.data=Netflix_ts,"2022 Q1"~.)

Netflix_ts %>% stretch_tsibble(.init = 12, .step=4) %>%
     model(ETS1=ETS(Subscribers ~ error("A") + trend("Ad") + season("A")),
           ETS2=ETS(Subscribers),
           TSLM1 = TSLM(Subscribers ~ trend() + I(trend()^2)),
           TSLM2=TSLM(Subscribers ~ trend() + I(trend()^2) + season())) %>%
     forecast(h=4) %>%
     accuracy(Netflix_ts) %>% 
     select(.model, ME, RMSE, MAPE) -> RMSE

#ETS1 and TSLM2 have low RMSE compared to ETS2 and TSLM1

Netflix_ts %>% 
     model(ETS1=ETS(Subscribers ~ error("A") + trend("Ad") + season("A")),
           TSLM2=TSLM(Subscribers ~ trend() + I(trend()^2) + season())) -> fit

fit %>% glance() %>% select(.model, AIC, AICc, BIC)

fit %>% forecast(h=2) %>% hilo(level=90) -> fc

fc
