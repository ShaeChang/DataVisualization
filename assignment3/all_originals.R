
# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)

# Load the data -----------------------------------------------------------

electricity_station_initial <-
  
  # read in the dataset
  
  st_read('data_own/alt_fuel_stations.geojson') %>% 
  
  # convert an sf object into a pure tibble
  
  as_tibble()

electricity_station <-
  electricity_station_initial %>% 
  
  # filter for the wanted types
  
  filter(
    
    # only include public electricity stations but not private ones
    
    access_code == 'public',
    
    # only include those are currently available but not planned nor
    # temporarily unavailable
    
    status_code == 'E',
    
    # only include those in the US
    
    country == 'US',
    
    # only include the charging stations open to the public
    
    restricted_access == FALSE) %>% 
  
  # select the wanted traits of those electricity charging stations 
  
  select(
    c(access_days_time, id, open_date, owner_type_code, state,
      ev_pricing, ev_renewable_source, facility_type))

# Data preperation --------------------------------------------------------

# create the wanted variable

elec_new <-
  electricity_station %>% 
  
  # create a variable measuring this station charge individuals or not
  
  mutate(Charge = 
           if_else(
             str_detect(ev_pricing, 'Free'),
             'Free',
             'Not free')) %>% 
  
  # filter the missing values for the two variables we care
  
  filter(
    !is.na(Charge),
    !is.na(owner_type_code)) %>% 
  
  # generate new categorical names
  
  mutate(owner_type_new = 
           case_when(
             owner_type_code == 'FG' ~ 'Federal',
             owner_type_code == 'J' ~ 'Jointly',
             owner_type_code == 'LG' ~ 'Local/Municipal',
             owner_type_code == 'P' ~ 'Privately',
             owner_type_code == 'SG' ~ 'State/Provincial',
             owner_type_code == 'T' ~ 'Utility')) 
  
  # convert the owner type into a factor with specific levels
  
elec_new$owner_type_new <-
  factor(elec_new$owner_type_new,
         levels = c('Federal',
                    'State/Provincial',
                    'Local/Municipal',
                    'Jointly',
                    'Utility',
                    'Privately'))

# Data visualization ------------------------------------------------------

elec_new %>% 
  ggplot(mapping = 
           aes(x = owner_type_new)) +
  geom_bar(aes(fill = Charge),
           position = 'dodge') +
  scale_x_discrete(drop = FALSE) +
  
  # to use green to represent free while use a diverging color of orange to 
  # represent not free
  
  scale_fill_manual(values = c('#99d594',
                               '#fc8d59')) +
  labs(title = 'Ownership and charging of electric vehicle charging posts',
       subtitle = paste('Charges for electric vehicle charging stations of', 
               'different owners in US'),
       caption = 'Data: afdc.energy.gov',
       x = 'Ownership of EV charging posts',
       y = 'Count (numbers)') +
  theme(
    axis.ticks = element_blank(),
    panel.background = element_blank())

# Data preparation --------------------------------------------------------

electricity_station %>% 
  
  arrange(desc(open_date))

elec_date <-
  electricity_station %>% 
  filter(!is.na(open_date),
         !is.na(owner_type_code)) %>% 
  mutate(open_time = 
           if_else(open_date >= ymd('2021-11-16'),
                   'Newly constructed',
                   'Pre-existing')) %>% 
  mutate(owner_type_new = 
           case_when(
             owner_type_code == 'FG' ~ 'Federal',
             owner_type_code == 'J' ~ 'Jointly',
             owner_type_code == 'LG' ~ 'Local/Municipal',
             owner_type_code == 'P' ~ 'Privately',
             owner_type_code == 'SG' ~ 'State/Provincial',
             owner_type_code == 'T' ~ 'Utility')) 

elec_date$owner_type_new <-
  factor(elec_date$owner_type_new,
         levels = c('Federal',
                    'State/Provincial',
                    'Local/Municipal',
                    'Jointly',
                    'Utility',
                    'Privately'))

# Data visualization ------------------------------------------------------

elec_date %>% 
  ggplot(mapping = 
           aes(x = owner_type_new)) +
  geom_bar(aes(fill = open_time),
           position = 'dodge') +
  scale_x_discrete(drop = FALSE) +
  
  # to use green to represent free while use a diverging color of orange to 
  # represent not free
  
  scale_fill_manual(values = c('#5ab4ac',
                               '#d8b365')) +
  labs(title = paste('Bipartisan Infrastructure Law Incentives for building EV',
                     'charging posts'),
       subtitle = paste('Comparison of the number of charging piles built by',
                        'different entities before and after November 16, 2021'),
       caption = 'Data: afdc.energy.gov',
       x = 'Ownership of EV charging posts',
       y = 'Count (numbers)') +
  theme(
    axis.ticks = element_blank(),
    panel.background = element_blank())









