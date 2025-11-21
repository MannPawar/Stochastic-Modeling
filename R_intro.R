
#Objects

x <- 5
x=5

y <- 20

z <- x + y

#Function

ls()
rm(x)

rm(list=ls())

sum(1,2,3,NA,na.rm = T)

#Vectors

prices <- c(5,1,6,2,5)
id <- seq(from=1, to=5, by=1)
rep(c(12,5), 5, each=4)
1:10
sample(prices,10,replace = T, prob=c(0.1,0.1,0.3,0.4,0.6))

Filtering

prices[1]
prices[1:3]
prices[-3]

prices[c(T,T,F,T,F)]

prices > 3

prices[prices > 3]

# > < >= != | &

prices[prices==3]

#appending

c(prices, 16)

append(prices,16)

#coercion
rating <- c("1","2","3")
sum(rating)

class(rating)
as.numeric(rating)

#Packages

install.packages("tidyverse", dependencies=T)
library(tidyverse) #load

##Data Structure 

tibble(ID=id, Prices=prices)

data <- read_csv("https://jagelves.github.io/Data/Returns2.csv")

#browse data

glimpse(data)
summary(data)
head(data)
tail(data)
str(data)

#dplyr

select(data,BTC) #selecting variables
filter(data, BTC>0.2)
mutate(data,add=BTC+NVDA) #adds new variable and keeps the rest of the data
transmute(data,add=BTC+NVDA) #keeps new variable, discards rest of the data

#write

write_csv(data,"returns.csv")














