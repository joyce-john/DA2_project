########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 3            ##
## Analyze   data     ##
########################


####################################################
######### SETUP AND OVERVIEW OF THE DATA ###########
####################################################


# Clear memory and call packages
rm(list=ls())
library(tidyverse)
library(scales)
library(lspline)
library(estimatr)
library(car)

# Set URL for clean data
my_url <- "https://raw.githubusercontent.com/joyce-john/DA2_project/main/data/clean/covid_pop_11_10_2020_clean.csv"

# Read the clean data into a dataframe
df <- read.csv(my_url)

# Delete the URL variable
rm(my_url)

# View summary of the data
summary(df)

# Examine histograms for all variables
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()

# There are some possible extreme values on the right tail. What are they?
df %>% arrange(desc(active)) %>% head()
df %>% arrange(desc(confirmed)) %>% head()
df %>% arrange(desc(death)) %>% head()
df %>% arrange(desc(population)) %>% head()
df %>% arrange(desc(recovered)) %>% head()
# These values do not appear to be mistakes. No need to take action.

# Create per_capita variables in the data. Scale to confirmed cases/deaths per one million.
df <- df %>% 
      mutate(confirmed_pc_scaled = confirmed/population * 1000000,
             death_pc_scaled = death/population * 1000000)


# Check and report distributions of x (registered cases per capita) and y (deaths per capita) 
# in several steps below

# Get statistics for confirmed cases per capita
confirmed_pc_scaled_stats <- df %>% 
                              select(confirmed_pc_scaled) %>% 
                              summarize(variable = "Confirmed Cases per Million",
                                        mean = mean(confirmed_pc_scaled),
                                        median = median(confirmed_pc_scaled),
                                        min = min(confirmed_pc_scaled),
                                        max = max(confirmed_pc_scaled),
                                        std_dev = sd(confirmed_pc_scaled))

# Get statistics for deaths per capita
death_pc_scaled_stats <- df %>% 
                           select(death_pc_scaled) %>% 
                           summarize(variable = "Deaths per Million",
                                     mean = mean(death_pc_scaled),
                                     median = median(death_pc_scaled),
                                     min = min(death_pc_scaled),
                                     max = max(death_pc_scaled),
                                     std_dev = sd(death_pc_scaled))

# Create one combined table for confirmed cases per capita and deaths per capita
combined_per_capita_stats <- rbind(confirmed_pc_scaled_stats, death_pc_scaled_stats)

# Remove unneeded individual stat tables
rm(confirmed_pc_scaled_stats, death_pc_scaled_stats)

# View the stats
view(combined_per_capita_stats)

# Vizualize distributions for confirmed cases per capita and deaths per capita with histograms
df %>% 
  ggplot(aes(x = confirmed_pc_scaled)) +
  geom_histogram() +
  labs(x = "Confirmed Cases per Million")

df %>% 
  ggplot(aes(x = death_pc_scaled)) +
  geom_histogram() +
  labs(x = "Deaths per Million")



####################################################
###### VISUALIZE POTENTIAL LN TRANSFORMATIONS ######
####################################################

           

# level-level
ggplot( df , aes(x = confirmed_pc_scaled, y = death_pc_scaled)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_pc_scaled (level) )",y = "death_pc_scaled (level)", title = "level x - level y")

# log x - level y
ggplot( df , aes(x = confirmed_pc_scaled, y = death_pc_scaled)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_pc_scaled (log) )",y = "death_pc_scaled (level)", title = "log x - level y") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# level x - log y
ggplot( df , aes(x = confirmed_pc_scaled, y = death_pc_scaled)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_pc_scaled (level) )",y = "death_pc_scaled (log)", title = "level x - log y") + 
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# log x - log y
ggplot( df , aes(x = confirmed_pc_scaled, y = death_pc_scaled )) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_pc_scaled (log) )",y = "death_pc_scaled (log)", title = "log x - log y") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans() )


# There is a clear linear pattern on the log-log scale


# Mutate dataframe to include logs
df <- df %>% 
  mutate(ln_confirmed_pc_scaled = log(confirmed_pc_scaled),
         ln_death_pc_scaled = log(death_pc_scaled))

# Examine the new columns for problematic values
range(df$ln_confirmed_pc_scaled)
range(df$ln_death_pc_scaled)

# ln_death_pc_scaled has values below zero. Why? Filter them into their own DF and take a look.
problem_vals <- df %>% 
                filter(ln_death_pc_scaled <= 0)

# When viewing the data, observe that negative values for ln_death_pc occur when a country...
# ...has zero deaths or a very small number of deaths relative to the number of cases.
# Most of the time, the cases per capita is fairly low, as well.
view(problem_vals)

# When building linear models, negative ln values must be excluded. Is this a appropriate for the analysis?
# Some of these countries have zero deaths. They cannot be included in a log transformation model.
# Some countries have managed the pandemic so well that they simply have very small death values.
# These small values cause problems when taking logs (if death_pc_scaled < 1, we get a negative log).
# Vietnam is one example. It has a population of 96 million, but only 35 (!!!) deaths.

# Filter out values which cause problems for building the models
df <- df %>% 
      filter(ln_death_pc_scaled > 0)


####################################################
######          CREATING MODELS               ######
####################################################


### simple linear for log log ###

# build the model
reg1 <- lm_robust(ln_death_pc ~ ln_confirmed_pc, data = df, se_type = "HC2")

# view model stats
summary( reg1 )

# visualize the model
ggplot(data = df, aes(x = ln_confirmed_pc, y = ln_death_pc)) +
  geom_point(color = "blue") +
  geom_smooth(method = lm, color = "red")


### quadratic for log log ###

# create a new column with x^2
df <- df %>% 
  mutate(ln_confirmed_pc_sq = ln_confirmed_pc^2)

# build the quadratic model
reg2 <- lm_robust( ln_death_pc ~ ln_confirmed_pc + ln_confirmed_pc_sq, data = df )

# view quadratic model stats
summary(reg2)

# visualize the model
ggplot( data = df, aes( x = ln_confirmed_pc, y = ln_death_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )



### linear splines ###

# set the knot cutoff, thinking in terms of actual values
cutoff <- 20000

# take the log of the knot cutoff 
ln_cutoff <- log(cutoff)

# build the model
reg3 <- lm_robust(ln_death_pc ~ lspline( ln_confirmed_pc , ln_cutoff ), data = df )

# view linear splines stats
summary(reg3)

# visualize linear splines model
ggplot( data = df, aes( x = ln_confirmed_pc, y = ln_death_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,ln_cutoff) , method = lm , color = 'red' )



### weighted linear regression with population weights ###

# build the model, using population as weight
reg4 <- lm_robust(ln_death_pc ~ ln_confirmed_pc, data = df , weights = population)

# view model stats
summary( reg4 )

# visualize the weighted linear regression model
ggplot(data = df, aes(x = ln_confirmed_pc, y = ln_death_pc)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  labs(x = "ln(confirmed cases pc) ",y = "ln(deaths pc)")

# Choose  weighted linear regression has the best r squared, and it is easy to interpret.


####################################################
####   Hypothesis Testing for Beta Parameter    ####
####################################################


# Hypothesis testing
# H0: Beta = 0    HA: Beta != 0

linearHypothesis(reg4, "ln_confirmed_pc = 0")

# With such a small p-value, we will reject the null.


####################################################
#######      Finding the Biggest Errors      #######
####################################################


# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$ln_death_pc - df$reg4_y_pred 


# Find 5 largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
  select( country , ln_death_pc , reg4_y_pred , reg4_res ) %>% 
  arrange(desc(reg4_res))

# Find the 5 largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
  select( country , ln_death_pc , reg4_y_pred , reg4_res ) %>% 
  arrange(reg4_res)








