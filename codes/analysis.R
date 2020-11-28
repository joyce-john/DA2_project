########################
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 3            ##
## Analyze   data     ##
########################

##########READ ASSIGNMENT DETAILS FOR DATE AND PER CAPITA


# Clear memory and call packages
rm(list=ls())
library(tidyverse)
library(scales)
library(lspline)
library(estimatr)

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

# There are some values on the long right tail. What are they?
df %>% arrange(desc(active)) %>% head()
df %>% arrange(desc(confirmed)) %>% head()
df %>% arrange(desc(death)) %>% head()
df %>% arrange(desc(population)) %>% head()
# These values do not appear to be mistakes. No need to take action.

# Create per_capita variables in the data. Scale to deaths per one million.
df <- df %>% 
      mutate(confirmed_per_capita = confirmed/population * 1000000,
             death_per_capita = death/population * 1000000)


# Check and report distributions of x (registered cases per capita) and y (deaths per capita)

# Get statistics for confirmed cases per capita
confirmed_per_capita_stats <- df %>% 
                              select(confirmed_per_capita) %>% 
                              summarize(variable = "Confirmed Cases per Capita",
                                        mean = mean(confirmed_per_capita),
                                        median = median(confirmed_per_capita),
                                        min = min(confirmed_per_capita),
                                        max = max(confirmed_per_capita),
                                        std_dev = sd(confirmed_per_capita))

# Get statistics for deaths per capita
death_per_capita_stats <- df %>% 
                           select(death_per_capita) %>% 
                           summarize(variable = "Deaths per Capita",
                                     mean = mean(death_per_capita),
                                     median = median(death_per_capita),
                                     min = min(death_per_capita),
                                     max = max(death_per_capita),
                                     std_dev = sd(death_per_capita))

# Create one combined table for confirmed cases per capita and deaths per capita
combined_per_capita_stats <- rbind(confirmed_per_capita_stats, death_per_capita_stats)

# Remove unneeded individual stat tables
rm(confirmed_per_capita_stats, death_per_capita_stats)

            

# level-level
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_per_capita (level) )",y = "death_per_capita (level)", title = "level x - level y")

# log x - level y
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_per_capita (log) )",y = "deaths_per_capita (level)", title = "log x - level y") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# level x - log y
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_per_capita (level) )",y = "deaths_per_capita (log)", title = "level x - log y") + 
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# log x - log y
ggplot( df , aes(x = confirmed_per_capita, y = death_per_capita)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "confirmed_per_capita (log) )",y = "deaths_per_capita (log)", title = "log x - log y") + 
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans() )


# There is a clear linear pattern on the log-log scale












# Mutate dataframe to include logs
df <- df %>% 
      mutate(ln_confirmed_pc = log(confirmed_per_capita),
             ln_death_pc = log(death_per_capita))

#####CONSIDER GRAPHING POLYNOMIAL FUNCTIONS TO VISUALIZE FIT#####
#####ADD SCALING BEFORE COMPUTING STATISTICS#####
#####RENAME VARIABLES TO REFLECT PER ONE MILLION SCALING#####

####CLUMSY CODE BELOW FOR BACKING UP#####

# simple linear for level level
reg1 <- lm_robust(death ~ confirmed, data = df , se_type = "HC2")
summary(reg1)
ggplot( data = df, aes( x = confirmed, y = death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

# simple linear for log log
positive_only_df <- df[(df$ln_death_pc > 0),]
reg2 <- lm_robust(ln_death_pc ~ ln_confirmed_pc, data = positive_only_df, se_type = "HC2")
summary( reg2 )
ggplot(data = positive_only_df, aes(x = ln_confirmed_pc, y = ln_death_pc)) +
  geom_point(color = "blue") +
  geom_smooth(method = lm, color = "red")

# quadratic for log log
positive_only_df <- positive_only_df %>% 
  mutate(ln_confirmed_pc_sq = ln_confirmed_pc^2,
         ln_death_pc_sq = ln_death_pc^2)
reg3 <- lm_robust( ln_death_pc ~ ln_confirmed_pc + ln_confirmed_pc_sq, data = positive_only_df )
summary(reg3)
ggplot( data = df, aes( x = ln_confirmed_pc, y = ln_death_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

#piecewise linear splines
cutoff <- 20000
ln_cutoff <- log(cutoff)
reg4 <- lm_robust(ln_death_pc ~ lspline( ln_confirmed_pc , ln_cutoff ), data = positive_only_df )
summary(reg4)
ggplot( data = positive_only_df, aes( x = ln_confirmed_pc, y = ln_death_pc ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,ln_cutoff) , method = lm , color = 'red' )

#weighted linear regression
reg7 <- lm_robust(ln_death_pc ~ ln_confirmed_pc, data = positive_only_df , weights = population)
summary( reg7 )
#outstanding r-squared
ggplot(data = positive_only_df, aes(x = ln_confirmed_pc, y = ln_death_pc)) +
  geom_point(data = positive_only_df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 15)) +
  labs(x = "ln(confirmed cases pc) ",y = "ln(deaths pc)")

#weighted linear regression has the best r squared, and it is easy to interpret


# Hypothesis testing
# H0: Beta = 0    HA: Beta != 0

library(car)
linearHypothesis(reg7, "ln_confirmed_pc = 0")


#finding the biggest errors

# Get the predicted y values from the model
positive_only_df$reg7_y_pred <- reg7$fitted.values
# Calculate the errors of the model
positive_only_df$reg7_res <- positive_only_df$ln_death_pc - positive_only_df$reg7_y_pred 


# Find 5 largest positive errors
positive_only_df %>% top_n( 5 , reg7_res ) %>% 
  select( country , ln_death_pc , reg7_y_pred , reg7_res ) %>% 
  arrange(desc(reg7_res))

# Find the 5 largest negative errors
positive_only_df %>% top_n( -5 , reg7_res ) %>% 
  select( country , ln_death_pc , reg7_y_pred , reg7_res ) %>% 
  arrange(reg7_res)








