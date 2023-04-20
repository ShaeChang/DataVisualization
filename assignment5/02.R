
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------

# Stations

stations <-
  read_csv('assignment5/alt_fuel_stations.csv') %>% 
  
  # ON is a Canada state, exclude non-US states.
  
  filter(State != 'ON')

# Data wrangling ----------------------------------------------------------

# 1. Repeat the process to generate the 'All_years' data frame as same as 
# the first graph (omit)

# 2. Minor adjustments

All_years_heat <-
  All_years %>% 
  filter(!is.na(EV_per_charger))

# factorize the state vector

level <-
  All_years_heat %>% 
  filter(year == 2021) %>% 
  arrange(desc(EV_per_charger)) %>% 
  pull(State)

All_years_heat$State <-
  factor(All_years_heat$State, levels = level)

# Reset the discrete intervals that suit heat map better

All_years_heat <-
  All_years_heat %>% 
  mutate(EV_per_Charger = 
           case_when(EV_per_charger <= 7 ~ '0 - 7',
                     EV_per_charger <= 14 ~ '7 - 14',
                     EV_per_charger <= 21 ~ '14 - 21',
                     EV_per_charger <= 28 ~ '21 - 28',
                     EV_per_charger <= 35 ~ '28 - 35',
                     EV_per_charger <= 42 ~ '35 - 42',
                     .default = '42 - 49'))

All_years_heat$EV_per_Charger <-
  factor(All_years_heat$EV_per_Charger,
         levels = c('0 - 7', '7 - 14', '14 - 21', '21 - 28', '28 - 35',
                    '35 - 42', '42 - 49'))

# Heat map ----------------------------------------------------------------

heat_map <-
  All_years_heat %>% 
  ggplot(mapping = 
           aes(x = State,
               y = year)) +
  geom_tile(
    aes(fill = EV_per_Charger)) +
  
  # the mid point is EV-to-charger ratio equals to 10, which is the recommended
  # level of EV charger deployment by the Alternative Fuel Infrastructure 
  # Directive (AFID).
  
  scale_fill_manual(values = 
                      c('#91cf60', '#f0f9e8', '#ffffb2', 
                        '#fecc5c', '#fd8d3c', '#f03b20', '#bd0026')) +
  labs(title = paste('Electric Vehicle Charging Point Deployment by State in',
                     'the U.S., 2018 - 2021'),
       subtitle = paste('From left to right: the state with the least (NJ) to',
                        'most (WY) adequate deployment of EV charging points'),
       caption = 'Data Source: Alternative Fuels Data Center') +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())


  






