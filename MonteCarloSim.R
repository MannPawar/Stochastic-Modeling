library(tidyverse)
library(ggthemes)
library(gt)
###For Loop
Earn = 0
plays = 100
system.time({
for (i in 1:plays){
   x = sample(c("green","red","black"),1,prob=c(2/38,18/38,18/38))
   if (x=="red"){
    Earn = Earn + 1
    } else{
     Earn = Earn - 1
  }
}
Earn
})
## Vectorize the problem
Earn = 0
plays = 100
system.time({
nsim = 10
sim<-tibble(simulation=rep(1:nsim, each=plays),
     Outcome=sample(c("green","red","black"),plays*nsim,
                        replace=T,prob=c(2/38,18/38,18/38)),
                      Strat=rep(c("red"), plays*nsim),
                      Earn=case_when(Outcome==Strat~1,
                                     TRUE~-1))
})
       
sim %>% group_by(simulation) %>% 
     summarize(S.Earn=sum(Earn)) %>% 
     gt() %>% 
     tab_style(style=list(cell_fill(color="#B2D3C2")),
               locations = cells_body(columns=S.Earn,
                                      rows=S.Earn==max(S.Earn)))

sim %>% group_by(simulation) %>% 
     summarise(S.Earn=sum(Earn)) %>% 
     ggplot() + geom_histogram(aes(S.Earn))

sim %>% group_by(simulation) %>% 
     summarise(S.Earn=sum(Earn)) %>% 
     ggplot() + geom_boxplot(aes(y = S.Earn))     

sim %>% group_by(simulation) %>% 
     summarise(S.Earn=sum(Earn)) %>% 
     pull(S.Earn) %>% quantile()


## Call Data

Calls <- read_csv("https://jagelves.github.io/Data/CallCenter.csv")
library(mice)
glimpse(Calls)

md.pattern(Calls)

Calls %>% ggplot() + geom_histogram(aes(Calls))

Calls %>% drop_na() %>% 
     summarize(mean(Calls),
               sd(Calls))

## Impute Mean

Calls %>% mutate(ICalls=case_when(is.na(Calls)==F~Calls,
                                  T~mean(Calls,na.rm = T))) %>% 
     ggplot() + geom_histogram(aes(ICalls))

## Impute using Poisson Distribution

Calls %>% mutate(ICalls=case_when(is.na(Calls)==F~Calls,
                                  T~rpois(1100,mean(Calls,na.rm = T)))) %>% 
     summarise(mean(ICalls),
               sd(ICalls))

Calls %>% mutate(ICalls=case_when(is.na(Calls)==F~Calls,
                                  T~rpois(1100,mean(Calls,na.rm = T)))) %>% 
     ggplot() + geom_histogram(aes(ICalls))


## Observed

(observed <- table(Calls$Calls))

(lambda <- mean(Calls$Calls, na.rm=T))

## Calculate the Expected Frequencies
(expected <- dpois(as.numeric(names(observed)),lambda) * sum(observed))
round(expected)

(chi_squared <- chisq.test(x=observed, p=expected/sum(expected)))


#High p value can't reject Null hypothesis, vote in favor of Null.
#H_0;Null hypothesis is that the Distribution is Poisson

rpois(20,mean(Calls$Calls, na.rm=T))

#Alaska Airlines Example
###Alaska Airlines is preparing for the peak holiday travel season and needs to decide how many seats to overbook on the SEA-SFO route. Overbooking is expected in the airline
#industry, as some passengers typically fail to show up for their flights. The airline has data indicating that the number of no-show passengers on this route follows a normal
#distribution with a mean of 20 and a standard deviation of 5.
#If the airline needs to overbook more seats, empty seats result in lost revenue, as the #cost
#of an unused seat is estimated to be $300. However, if the airline overbooks too many
#seats, it may need to compensate bumped passengers with vouchers worth $600 each.
#Use 10,000 simulations to determine the optimal number of seats to overbook to minimize
#the airline’s expected cost. Try possible values from 15 to 25 in increments of 1.

#Inputs

a.noshow <- 20
sd.noshow <- 5
c.unused <- 300
c.voucher <- 600
#Decision Variable
overbook <- seq(15)
nsim <- 2
tibble(simulation=rep(1:length(overbook),each=nsim))





