
# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(readxl)

# Load data ---------------------------------------------------------------

# Stations

stations <-
  read_csv('assignment5/alt_fuel_stations.csv') %>% 
  
  # ON is a Canada state, exclude non-US states.
  
  filter(State != 'ON')

# EV registration

LDV_2021 <-
  read_excel('assignment5/2021_LDV_registration.xlsx')

LDV_2020 <-
  read_excel('assignment5/2020_LDV_registration.xlsx')

LDV_2019 <-
  read_excel('assignment5/2019_LDV_registration.xlsx')

LDV_2018 <-
  read_excel('assignment5/2018_LDV_registration.xlsx')

# State name and code

state <-
  read_csv('assignment5/state.csv') %>% 
  select(-'abbrev') %>% 
  rename('State' = 'state')

# Data wrangling ----------------------------------------------------------

# 1. Repeat the process to generate the 'All_years' data frame as same as 
# the first graph

# Electric vehicle supply equipment (EVSE, charging points) in 2021 by state

charger_2021 <-
  stations %>% 
  filter(year(`Open Date`) < 2022) %>% 
  filter(State != 'ON') %>% 
  select(`Station Name`, `EV Level1 EVSE Num`, `EV Level2 EVSE Num`,
         `EV DC Fast Count`, State) %>% 
  replace(is.na(.), 0) %>% 
  mutate(EVSE = `EV Level1 EVSE Num` + `EV Level2 EVSE Num` + 
           `EV DC Fast Count`) %>% 
  group_by(State) %>% 
  summarise(EVSE_state = sum(EVSE))

# the Plug-in Electric Vehicles in 2021 by state

PEV_2021 <-
  LDV_2021 %>% 
  filter(State != 'United States') %>% 
  mutate(PEV = `Electric (EV)` + `Plug-In Hybrid Electric (PHEV)`) %>% 
  select(State, PEV)

# Merge the needed data for 2021

merge_2021 <-
  full_join(state, PEV_2021) %>% 
  select(code, PEV) %>% 
  rename(State = code) %>% 
  full_join(charger_2021) %>% 
  mutate(EV_per_charger = PEV / EVSE_state) %>% 
  select(State, EV_per_charger) %>% 
  mutate(year = 2021)

# Electric vehicle supply equipment (EVSE, charging points) in 2020 by state

charger_2020 <-
  stations %>% 
  filter(year(`Open Date`) < 2021) %>% 
  filter(State != 'ON') %>% 
  select(`Station Name`, `EV Level1 EVSE Num`, `EV Level2 EVSE Num`,
         `EV DC Fast Count`, State) %>% 
  replace(is.na(.), 0) %>% 
  mutate(EVSE = `EV Level1 EVSE Num` + `EV Level2 EVSE Num` + 
           `EV DC Fast Count`) %>% 
  group_by(State) %>% 
  summarise(EVSE_state = sum(EVSE))

# the Plug-in Electric Vehicles in 2020 by state

PEV_2020 <-
  LDV_2020 %>% 
  filter(State != 'United States') %>% 
  mutate(PEV = `Electric (EV)` + `Plug-In Hybrid Electric (PHEV)`) %>% 
  select(State, PEV)

# Merge the needed data for 2020

merge_2020 <-
  full_join(state, PEV_2020) %>% 
  select(code, PEV) %>% 
  rename(State = code) %>% 
  full_join(charger_2020) %>% 
  mutate(EV_per_charger = PEV / EVSE_state)%>% 
  select(State, EV_per_charger) %>% 
  mutate(year = 2020)

# Electric vehicle supply equipment (EVSE, charging points) in 2019 by state

charger_2019 <-
  stations %>% 
  filter(year(`Open Date`) < 2020) %>% 
  filter(State != 'ON') %>% 
  select(`Station Name`, `EV Level1 EVSE Num`, `EV Level2 EVSE Num`,
         `EV DC Fast Count`, State) %>% 
  replace(is.na(.), 0) %>% 
  mutate(EVSE = `EV Level1 EVSE Num` + `EV Level2 EVSE Num` + 
           `EV DC Fast Count`) %>% 
  group_by(State) %>% 
  summarise(EVSE_state = sum(EVSE))

# the Plug-in Electric Vehicles in 2019 by state

PEV_2019 <-
  LDV_2019 %>% 
  filter(State != 'United States') %>% 
  mutate(PEV = `Electric (EV)` + `Plug-In Hybrid Electric (PHEV)`) %>% 
  select(State, PEV)

# Merge the needed data for 2019

merge_2019 <-
  full_join(state, PEV_2019) %>% 
  select(code, PEV) %>% 
  rename(State = code) %>% 
  full_join(charger_2019) %>% 
  mutate(EV_per_charger = PEV / EVSE_state)%>% 
  select(State, EV_per_charger) %>% 
  mutate(year = 2019)

# Electric vehicle supply equipment (EVSE, charging points) in 2018 by state

charger_2018 <-
  stations %>% 
  filter(year(`Open Date`) < 2019) %>% 
  filter(State != 'ON') %>% 
  select(`Station Name`, `EV Level1 EVSE Num`, `EV Level2 EVSE Num`,
         `EV DC Fast Count`, State) %>% 
  replace(is.na(.), 0) %>% 
  mutate(EVSE = `EV Level1 EVSE Num` + `EV Level2 EVSE Num` + 
           `EV DC Fast Count`) %>% 
  group_by(State) %>% 
  summarise(EVSE_state = sum(EVSE))

# the Plug-in Electric Vehicles in 2018 by state

PEV_2018 <-
  LDV_2018 %>% 
  filter(State != 'United States') %>% 
  mutate(PEV = `Electric (EV)` + `Plug-In Hybrid Electric (PHEV)`) %>% 
  select(State, PEV)

# Merge the needed data for 2018

merge_2018 <-
  full_join(state, PEV_2018) %>% 
  select(code, PEV) %>% 
  rename(State = code) %>% 
  full_join(charger_2018) %>% 
  mutate(EV_per_charger = PEV / EVSE_state)%>% 
  select(State, EV_per_charger) %>% 
  mutate(year = 2018)

# Merge the data of different years

All_years <-
  rbind(merge_2018,
        merge_2019) %>% 
  rbind(merge_2020) %>% 
  rbind(merge_2021)

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

All_years_heat$year <-
  factor(All_years_heat$year,
         levels = seq(2021, 2018, -1))

# Heat map ----------------------------------------------------------------

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


  






