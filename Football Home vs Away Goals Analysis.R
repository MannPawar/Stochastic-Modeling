rm(list=ls())

library(tidyverse)
library(ggthemes)

#read data

data <- read_csv("https://jagelves.github.io/Data/PL.csv")

glimpse(data)

#Piping
data %>% filter(HomeTeam=="Chelsea") %>% 
     filter(FTHG>=3)

data %>% select(Season:HomeTeam)

## Creating Variable

data %>% select(Season:AR) %>% 
     mutate(Date=mdy(Date)) %>% 
     mutate(Year=year(Date), Month=month(Date)) -> data


### Home Advantage Goals
Team <- "Liverpool"

data %>% filter(Season==2025) %>% 
     filter(HomeTeam==Team|AwayTeam==Team) %>% 
     mutate(Teams=case_when(HomeTeam==Team|AwayTeam==Team ~ 1,
                            TRUE ~ 0)) %>% 
     mutate(Goals=case_when(HomeTeam==Team ~ FTHG,
                            TRUE ~ FTAG)) %>% 
     mutate(Home=case_when(HomeTeam==Team ~ "Yes",
                           TRUE ~ "No")) -> result

##Plot

result %>% ggplot() + 
     geom_boxplot(aes(y=Goals), width=1,
                  outlier.fill = "red",
                  outlier.shape = 21,
                  outlier.color = "black",
                  outlier.alpha = 0.5,
                  outlier.size = 3) +
     scale_x_continuous(limits=c(-3,3), breaks=NULL) +
     theme_clean() +
     labs(title="Goals Scored",
          subtitle="2024-2025 PL",
          caption="Source PL") +
     geom_jitter(aes(y=Goals, x=0))

result %>% ggplot() + 
     geom_boxplot(aes(y=Goals, fill=Home), width=1) +
     scale_x_continuous(limits=c(-3,3), breaks=NULL) +
     theme_clean() +
     labs(title="Goals Scored",
          subtitle="2024-2025 PL",
          caption="Source PL")

# Table
library(gt)

result  %>%  group_by(Home) %>% 
     summarise(A.G=mean(Goals),
               Sd=sd(Goals),
               p95=quantile(Goals,0.95)) %>% gt() %>% 
     tab_header(title = md("**Home vs Away Goals**"),
                subtitle = md("**2024-2025**")) %>% 
     cols_align("center") %>% 
     fmt_number(columns=c(A.G,Sd), decimals=2) %>% 
     tab_style(
          style=list(cell_fill(color="#B2D3C2")),
          locations = cells_body(rows= max(A.G))
     )


