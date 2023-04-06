
# The first original graph for assignment 4

# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------

EV_2010_to_2021 <-
  read_csv('data_own/IEA-EV-data.csv')

# Data wrangling ----------------------------------------------------------

EV_cars_quantity <-
  EV_2010_to_2021 %>% 
  filter(region %in% 
           c('China', 'Europe', 'USA', 'India')) %>% 
  filter(year %in% 
           seq(2010, 2030, 5)) %>% 
  filter(category != 'Projection-APS') %>% 
  filter(parameter %in% 
           c('EV stock')) %>% 
  mutate(mode_new = 
           if_else(mode == 'Cars',
                   'Cars',
                   'Vans, buses & trucks')) %>% 
  group_by(region, year, mode_new) %>% 
  summarise(value_new = 
              sum(value))

  # Set the categorical variables as factors with certain orders

EV_cars_quantity$mode_new <-
  factor(EV_cars_quantity$mode_new,
         levels = 
           c('Vans, buses & trucks', 'Cars'))

EV_cars_quantity$region <-
  factor(EV_cars_quantity$region,
         levels = 
           
           # in alphabetical order
           
           c('China', 'Europe', 'India', 'USA'))

# Add percent numbers for reference

EV_cars_percent <-
  EV_cars_quantity %>% 
  pivot_wider(names_from = mode_new,
              values_from = value_new) %>% 
  mutate(percentage = 
           formattable::percent(
             (`Vans, buses & trucks` / 
                 (`Vans, buses & trucks` + `Cars`)))) %>% 
  select(region, year, percentage)

# Combine the data together

EV_cars_all <-
  EV_cars_quantity %>% 
  full_join(EV_cars_percent,
            by = join_by(region, year))

# Data visualization ------------------------------------------------------

EV_cars_all %>% 
  ggplot(mapping = 
           aes(x = year,
               y = value_new,
               fill = mode_new)) +
  
  # only add some of the grid lines
  
  geom_hline(yintercept = 25000000,
             color = 'gray93') +
  geom_hline(yintercept = 50000000,
             color = 'gray93') +
  geom_hline(yintercept = 75000000,
             color = 'gray93') +
  geom_hline(yintercept = 100000000,
             color = 'gray93') +
  geom_bar(stat = 'identity',
           width = 3.2) +
  facet_wrap(~ region,
             nrow = 1) +
  geom_text(aes(label = percentage),
            size = 2.5,
            check_overlap = T) +
  scale_y_continuous(
    breaks = seq(0, 125000000, 25000000),
    labels = c('0', '25', '50', '75', '100', ''),
    limits = c(0, 125000000)) +
  scale_fill_manual(values = 
                      c('#4daf4a', '#377eb8')) +
  labs(title = paste('Cars dominate vehicle electrification in the past,',
                     'present, and foreseeable future'),
       subtitle = paste('Electric vans, buses and trucks account for a little',
                        'proportion of electric vehicles in key markets worldwide'),
       caption = 'Data source: International Energy Agency',
       y = 'Million') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'none')
