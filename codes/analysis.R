########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 3            ##
## Analyze   data     ##
########################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)
library(scales)

# Set URL for clean data
my_url <- "https://raw.githubusercontent.com/joyce-john/DA2_project/main/data/clean/covid_pop_09_11_2020_clean.csv"

# Read the clean data into a dataframe
df <- read.csv(my_url)

# Delete the URL variable
rm(my_url)

summary(df)

df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# level-level
ggplot( df , aes(x = death, y = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed (level) )",y = "deaths (level)")

# log x - level y
ggplot( df , aes(x = death, y = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed (log) )",y = "deaths (level)") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# level x - log y
ggplot( df , aes(x = death, y = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed (level) )",y = "deaths (log)") + 
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# log x - log y
ggplot( df , aes(x = death, y = confirmed)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed (log) )",y = "deaths (log)") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans() )

##### log-log model looks promising #####

# Mutate dataframe to include logs
df <- df %>% 
      mutate(log_confirmed = log(confirmed),
             log_death = log(death))


