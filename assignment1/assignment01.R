
# Assignment 01
# Xiyu Zhang

# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(knitr)

# Replication of Yau's graph ----------------------------------------------

# Load the data

crime <-
  read.csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv',
           sep = ',',
           header = TRUE)

plot(crime$murder, crime$burglary)

# Remove the average data across the whole nation,
# and remove Washington DC as an outlier

crime2 <-
  crime %>% 
  filter(state != 'District of Columbia') %>% 
  filter(state != 'United States')

plot(crime2$murder, crime2$burglary)

# Replicate graph 6-7

crime2 %>% 
  ggplot(
    aes(
      x = murder,
      y = burglary)) +
  geom_point(
    colour = '#6baed6') +
  geom_smooth(
    method = loess,
    se = F,
    colour = '#3182bd') +
  labs(title = 'MURDERS VERSUS BURGLARIES IN THE UNITED STATES',
       subtitle = paste('States with higher murder rates tend to have higher',
                        'burglary rates'),
       caption = 'Source: U.S. Census Bureau | Nathan Yau',
       x = 'Murders per 100,000 population',
       y = 'Burglaries per 100,000 population')

# Original scatterplot ----------------------------------------------------

# Read in the initial dataset

# motor gasoline price for each state, from EIA, in 2021

gasoline_21 <-
  read_csv('data_own/fuel_mg.csv') %>% 
  filter(MSN == 'MGACD') %>% 
  rename(motor_gasoline = '2021')

# all sector electricity price for each state, from EIA, in 2021

electricity_21 <-
  read_csv('data_own/fuel_es.csv') %>% 
  filter(MSN == 'ESTCD') %>% 
  rename(electricity = '2021')

# join the two forms into one

price_21 <-
  left_join(gasoline_21, 
            electricity_21, 
            by = 'State') %>% 
  select(State, motor_gasoline, electricity)

# create an elementary scatterplot

price_21 %>% 
  ggplot(
    aes(x = motor_gasoline,
        y = electricity)) +
  geom_point(colour = 'blue')

# It seems there exists an outlier, so descending the electricity price and
# remove the outlier, and create the scatterplot again

price_21 %>% 
  arrange(desc(electricity)) %>% 
  filter(State != 'HI') %>% 
  ggplot(
    aes(x = motor_gasoline,
        y = electricity,
        label = State)) +
  geom_point(colour = '#3182bd',
             alpha = 0.7) +
  geom_smooth(
    method = loess,
    colour = '#6baed6',
    se = F) +
  geom_text(hjust = 0,
            vjust = -1,
            size = 3,
            colour = '#6baed6') +
  labs(title = paste('The cost of driving gasoline and electric cars in the',
                     'United States'),
       subtitle = paste('Motor gasoline price and electricity price in each',
                        'state of US in 2021'),
       caption = 'Data: eia.gov',
       x = 'Motor gasoline price in transportation sector (USD per MMBtu)',
       y = 'Electricity price in all sector (USD per MMBtu)') +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = '#6baed6'),
    axis.text.x = element_text(colour = '#3182bd'),
    axis.text.y = element_text(colour = '#3182bd'),
    axis.ticks = element_line(colour = '#6baed6'))

# Knit the original .R file -----------------------------------------------

stitch('assignment1/assignment1_original.R')


