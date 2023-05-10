# Assignment 01 original
# Xiyu Zhang

# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)

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
  select(State, motor_gasoline, electricity) %>% 
  mutate(cheap = ifelse(electricity <= motor_gasoline,
                        T,
                        F))

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
        label = State,
        color = cheap)) +
  geom_point(alpha = 0.7,
             size = 4) +
  geom_segment(
    aes(x = 22,
        y = 22,
        xend = 36,
        yend = 36),
    color = '#238443') +
  geom_text(hjust = -0.7,
            vjust = 0.5,
            size = 2.6,
            check_overlap = T,
            color = '#525252') +
  scale_y_continuous(limits = c(20, 60)) +
  scale_color_manual(values = c('#cccccc', '#74c476')) +
  labs(title = paste('The cost of driving gasoline and electric cars in the',
                     'United States'),
       subtitle = paste('Motor gasoline price and electricity price in each',
                        'state of US in 2021'),
       caption = 'Source: U.S. Energy Information Administration',
       x = 'Motor gasoline price in transportation sector (USD per MMBtu)',
       y = 'Electricity price in all sector (USD per MMBtu)') +
  theme_bw() +
  theme(
    axis.line = element_line(colour = '#969696'),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank())

