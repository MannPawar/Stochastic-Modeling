rm(list=ls())

#Alaska Airlines Example
###Alaska Airlines is preparing for the peak holiday travel season and needs to decide how many seats to overbook on the SEA-SFO route. Overbooking is expected in the airline
#industry, as some passengers typically fail to show up for their flights. The airline has data indicating that the number of no-show passengers on this route follows a normal
#distribution with a mean of 20 and a standard deviation of 5.
#If the airline needs to overbook more seats, empty seats result in lost revenue, as the #cost
#of an unused seat is estimated to be $300. However, if the airline overbooks too many
#seats, it may need to compensate bumped passengers with vouchers worth $600 each.
#Use 10,000 simulations to determine the optimal number of seats to overbook to minimize
#the airline’s expected cost. Try possible values from 15 to 25 in increments of 1.

library(tidyverse)
library(ggthemes)

#Inputs

a.noshow <- 20
sd.noshow <- 5
c.unused <- 300
c.voucher <- 600
#Decision Variable
Overbook <- seq(15,25,1)
nsim <- 10000
set.seed(10)
sim <- tibble(simulation=rep(1:length(Overbook),each=nsim),
       overbook=rep(Overbook,each=nsim),
       noshows=round(rnorm(nsim*length(Overbook), a.noshow,sd.noshow)),
       cost=case_when(noshows>overbook~300*(noshows-overbook),
                      TRUE~600*(overbook-noshows)))

sim %>% group_by(simulation) %>% 
     summarise(Overbook = mean(overbook),
               A.Cost=mean(cost)) %>% 
     ggplot() + geom_line(aes(x=simulation, y=A.Cost))

#You are considering an investment in the SPY (S&P 500 ETF) over 20 years. Your investment strategy involves making an initial investment, followed by yearly contributions. The key parameters for your investment are as follows:
#Stock Ticker: SPY (S&P 500 ETF); Initial Investment: $1,000; Average Yearly Return: 10% (0.1); Standard Deviation of
#Yearly Returns: 18% (0.18); Yearly Investment: $6,000; Investment Period: 20 years
#Run 10,000 simulations of your investment over 20 years. Returns are normally distributed each year with the given average and standard deviation. Track the value of your investment at the end of each year. Calculate the following statistics for the investment value at the end of 20 years:
#Average (mean) end value, Worst Case Scenario: Minimum, Bear Case: 25th percentile (Q1), Base Case: Median (50th percentile), Bull Case: 75th percentile (Q3), Best Case Scenario:Maximum 
#Based on your simulation results, how much can you expect to have in your account after 20 years on average?
#What are the Bear, Base, and Bull case predictions?

initial <- 1000
yi <- 6000
a.return <- 0.1
sd.return <- 0.18
years <- 20

#Simulation
nsim <- 10000

set.seed(10)
sim2 <-(tibble(simulation=rep(1:nsim,each=years),
       period<-rep(1:years,nsim),
       returns=rnorm(years*nsim,a.return,sd.return)) %>% 
       group_by(simulation) %>% 
       mutate(Value = accumulate(returns[-1], ~ .x*(1+.y)+yi,
                                 .init = initial * (1+returns[1]) + yi)))

sim2 %>% group_by(simulation) %>% 
     summarise(A.value=mean(Value)) %>%
     ggplot() + geom_histogram(aes(A.value))

sim2 %>% group_by(simulation) %>% 
     summarise(A.value=mean(Value)) %>% 
     pull(A.value) %>% quantile()

