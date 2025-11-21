rm(list=ls())
## Tesla Deliveries
library(tidyverse)
library(fpp3)
library(ggthemes)

# Load data
deli<-read_csv("http://jagelves.github.io/Data/tsla_deliveries.csv")

# Make Tsibble
deli %>% 
  mutate(period=yearquarter(period)) %>%
  as_tsibble(index=period) %>% 
  mutate(Quarter = factor(quarter(period)))->deli_ts

# Over-fitting. Build models with different polynomials
deli_ts %>%
  model(TREND=TSLM(deliveries~trend()),
        TREND2=TSLM(deliveries~trend()+I(trend()^2)),
        TRENDN=TSLM(deliveries~I(poly(trend(), 20)))) -> fit1

# Find accuracy on train set
fit1 %>% accuracy()

# plot
deli_ts %>% autoplot(.vars=deliveries) +
  geom_line(data=fit1%>%augment()%>%filter(.model=="TRENDN"),
            aes(y=.fitted), col="blue", lty=2, lwd=1.1) +
  theme_classic()

# Creating Train and Test Sets
train_del<-filter_index(.data=deli_ts,.~"2023 Q2")
test_del<-filter_index(.data=deli_ts,"2023 Q3"~.)

# Fitting model on the train set
train_del %>% 
  model(TREND=TSLM(deliveries~trend()),
    TREND2=TSLM(deliveries~trend()+
                                 I(trend()^2)),
    TRENDN=TSLM(deliveries~trend()+
                  I(trend()^2)+I(trend()^3))) -> fit2


# Passing deli_ts gets the accuracy on the test set and provides mase and rmsse
# passing tes_del does not provive the mase or rmsse

# Forecast the test set and calculate accuracy on test
fit2 %>% forecast(new_data = test_del) %>% 
  accuracy(deli_ts)

# Check accuracy on test set trend
fit2 %>% forecast(test_del) %>% filter(.model=="TREND") %>% 
  pull(.mean) -> pred
mean(test_del$deliveries[1:nrow(test_del)]-pred[1:nrow(test_del)])

# Check accuracy on test set Trend 2
fit2 %>% forecast(test_del) %>% filter(.model=="TREND2") %>% 
  pull(.mean) -> pred
mean(test_del$deliveries[1:nrow(test_del)]-pred[1:nrow(test_del)])

# Plot
fit2 %>% forecast(test_del) %>% filter(.model=="TREND") %>% 
  autoplot(level=95) +
  autolayer(deli_ts, deliveries) +
  autolayer(test_del, deliveries, col="orange")+
  autolayer(fit2 %>% augment() %>% 
              filter(.model=="TREND"),.fitted, col="blue") +
  theme_clean()

# Adding Seasonality (dummy variables)
train_del %>% 
  model(STREND=TSLM(deliveries~trend()+season()),
        STREND2=TSLM(deliveries~trend()+
                      I(trend()^2)+season()),
        STREND3=TSLM(deliveries~trend()+
                      I(trend()^3)+season())) -> fit3

# Accuracy on the test set
fit3 %>% forecast(test_del) %>% accuracy(deli_ts)


#Coefficients
fit3 %>% coef()

# Residuals
fit3 %>% augment() %>% autoplot(.resid) + theme_clean()

# Prediction Interval Accuracy
fit3 %>% forecast(test_del) %>% hilo(level=95) %>%
  filter(.model=="STREND") -> pred_int

fit3 %>% forecast(test_del) %>%
  accuracy(deli_ts, list(winkler = winkler_score)) -> wink
print(wink)

# Lower Winkler Score the Better. Winkler rewards
# narrow prediction intervals, that include observations.
# if an observation is outside, a penalty is added.


# Forecast with the entire data set.
deli_ts %>% 
  model(STREND=TSLM(deliveries~trend()+season()))->final_fit

# Forecast:
fc <- final_fit %>% forecast(h=1)

# Scenarios
# Create Dummy
deli_ts<-mutate(deli_ts,
                Down=case_when(
                  period==yearquarter("2022 Q2") ~ 1,
                                       TRUE ~ 0))
# Build models
deli_ts %>%
  model(STREND = TSLM(deliveries~trend()+season()+Down)) -> fit4

# Build new data set to forecast
Down_Scenario <- scenarios(
  Factory_Down = new_data(deli_ts, 1) |>
    mutate(Down=rep(1,1)),
  Factory_Up = new_data(deli_ts, 1) |>
    mutate(Down=rep(0,1)),
  names_to = "Scenario")

# Produce forecast
fc_s<-forecast(fit4,new_data=Down_Scenario)

# Plot
deli_ts %>%
  autoplot(deliveries) + 
  autolayer(forecast(fit4,new_data=Down_Scenario),
            level=NULL)+ theme_classic() +
  labs(x="Quarter", y="",
       title = "Tesla Car Deliveries Forecasts",
       subtitle = "Scenario Forecast") + theme_classic()


# Cross Validation
deli_ts %>% stretch_tsibble(.init = 20, .step=1) %>%
  model(TREND=TSLM(deliveries~trend()),
        STREND2=TSLM(deliveries~trend()+season())) %>%
  forecast(h=1) %>%
  accuracy(deli_ts)  

# Plot
deli_ts %>% stretch_tsibble(.init = 20, .step=5) %>%
  model(TREND=TSLM(deliveries~trend()),
        STREND=TSLM(deliveries~trend()+season())) %>%
  forecast(h=5) %>% autoplot() +
  autolayer(deli_ts)

# Information Criterion
fit4 %>% augment() %>% filter(.model=="STREND") %>%
  transmute(SE=.resid^2) %>% pull(SE) %>% sum() -> sse

fit4 %>% coef() %>% filter(.model=="STREND") %>% nrow()


(AIC= 38*log(sse/38)+2*(6+1))
(AICc=38*log(sse/38)+2*(6+1)+2*((6+1)*(6+2))/(38-6-2))
(BIC=38*log(sse/38)+(6+1)*log(38))

library(gt)
fit4 %>% glance() %>% select(c(.model,df,AIC,AICc,BIC)) %>%
  gt() 

## Interpolate
rm(list=ls())

# Create example data
data<-tibble(
  dates=c(seq(from=dmy("01/01/2023"),
              to=dmy("01/04/2023"),by="1 day"),
          c(seq(from=dmy("01/05/2023"),
                to=dmy("01/11/2023"),by="1 day"))),
  x=rnorm(length(dates)))

# Make tsibble
data %>% as_tsibble(index=dates) -> data_ts

# fill gaps to generate the missing dates
data_ts %>% fill_gaps() -> data_ts

# Imput the data with the mean
data_ts %>% 
  model(ARIMA(x)) %>%
  interpolate(data_ts) %>% 
  left_join(data_ts, by="dates") %>%
  rename(x=x.y,xInter=x.x)->data_ts
  
# Multiple Regressors
library(fpp3)
emissions<-read_csv("https://jagelves.github.io/Data//RwandaCo2.csv")
glimpse(emissions)

# Get data ready
emissions %>% filter(latitude==-0.51 & longitude==29.29) %>%
  filter(week_no!=0 & year==2019 | 
           week_no!=0 & year==2020 |
           year==2021) %>%
  mutate(Date=yearweek(seq(from=as.Date("2019-01-01"),
                           length.out=157, by = "1 week"))) %>%
  select(Date, emission, UvA, Ozone, Cloud) %>%
  as_tsibble(index=Date)-> emi_ts

# plot
emi_ts %>% autoplot(emission) + 
  theme_classic()

# models
emi_ts %>% 
  model(TREND=TSLM(emission~trend()+season()),
        CTREND=TSLM(emission~trend()+season()+Ozone)) -> fit

# accuracy
fit %>% accuracy()

new_data<-scenarios(
  "Mean Oz" = new_data(emi_ts, 10) |>
    mutate(Ozone = mean(emi_ts$Ozone)),
  "+ 1 SD Oz " = new_data(emi_ts, 10) |>
    mutate(Ozone = mean(emi_ts$Ozone)+sd(emi_ts$Ozone)),
  "+ 2 SD Oz" = new_data(emi_ts, 10) |>
    mutate(Ozone = mean(emi_ts$Ozone)+2*sd(emi_ts$Ozone)),
  names_to = "Scenario"
)


(fore<-forecast(fit,new_data))

fore %>% autoplot(level=NULL) +
  autolayer(emi_ts,emission) + theme_classic()



