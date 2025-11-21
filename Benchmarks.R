library(ggthemes)
library(tidyverse)
library(fpp3)

#load
cali <- read_csv("https://jagelves.github.io/Data/CaliforniaAvocado.csv")
glimpse(cali)

#Tsibble
cali %>% select(date, average_price) %>% 
     as_tsibble(index=date) -> cali_ts

#Features Descriptive Stats
cali_ts %>% features(average_price,
                     list(Mean=mean,
                          SD=sd,
                          Q=quantile))

#Moving Averages
cali_ts %>% mutate(MA50=slider::slide_dbl(average_price,mean,
                                          .before=100,.after=100,
                                          .complete=F)) -> cali_ts
cali_ts %>% autoplot(average_price) + 
     geom_line(aes(y=MA50), col="orange", lty=2)

#AutoCorrelations. Lag plots

cali_ts %>% 
     gg_lag(average_price, geom = "point",
            lags = 5, pch = 21, bg = "blue", alpha = 0.5) + theme_clean()

#Decomposition

cali_ts %>% 
     model(STL=STL(average_price~trend(200)+season(52))) %>% 
     components() %>% autoplot() + theme_clean()

#Electricity

ele<-read_csv("https://jagelves.github.io/Data/ElectricityBill.csv")

#Tsibble
ele %>% 
     mutate(Date=yearmonth(make_date(Year,Month))) %>% select(Date, `Bill Amount`) %>% 
     as_tsibble(index=Date) -> ele_ts

ele_ts

ele_ts %>% 
     model(STL=STL(`Bill Amount` ~ trend(100)+season(12))) %>% 
     components() %>% autoplot()



ele_ts %>% 
     model(MEAN=MEAN(`Bill Amount`),
           NAIVE = NAIVE(`Bill Amount`),
           DRIFT=RW(`Bill Amount`~drift()),
           TSLM=TSLM(`Bill Amount`~trend()+season())) -> fit

fit %>% coef()

fit %>% augment()

fit %>% accuracy()

fit %>% augment() %>% filter(.model == "MEAN") %>% 
     pull(.resid) %>% abs() %>%  mean()

#Forecast

fit %>% forecast(h=10) %>% filter(.model=="TSLM") -> result

#Plot

fit %>% forecast(h=10) %>% filter(.model=="TSLM") %>% 
     autoplot(level=90) +
     autolayer(ele_ts, `Bill Amount`) +
     theme_clean()
  

