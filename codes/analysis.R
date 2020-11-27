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



