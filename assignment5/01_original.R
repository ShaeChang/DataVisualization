
# Load packages -----------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)

# Load data ---------------------------------------------------------------

# Stations

stations <-
  read_csv('assignment5/alt_fuel_stations.csv')

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

# Data for 2021 ----------------------------------------------------------

# Electric vehicle supply equipment (EVSE, charging points) in 2021 by state

charger_2021 <-
  stations %>% 
  
  # the open date is ealier than 2022, so this is the existence of chargers in 
  # 2021
  
  filter(year(`Open Date`) < 2022) %>% 
  
  # 'ON' is in Canada
  
  filter(State != 'ON') %>% 
  select(`Station Name`, `EV Level1 EVSE Num`, `EV Level2 EVSE Num`,
         `EV DC Fast Count`, State) %>% 
  
  # 'NA' indicates that there is no this type of EVSE in the station, so there
  # is 0 this type of EVSE in the station
  
  replace(is.na(.), 0) %>% 
  
  # the total number of charging points in a charging station is the sum of 
  # 3 types of EVSEs
  
  mutate(EVSE = `EV Level1 EVSE Num` + `EV Level2 EVSE Num` + 
           `EV DC Fast Count`) %>% 
  
  # calculate the total number of EVSE in each state in 2021
  
  group_by(State) %>% 
  summarise(EVSE_state = sum(EVSE))

# the Plug-in Electric Vehicles in 2021 by state

PEV_2021 <-
  LDV_2021 %>% 
  
  # exclude the total amount
  
  filter(State != 'United States') %>% 
  
  # only calculate the types of EV that needed chargers
  
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

# Data for 2020 -----------------------------------------------------------

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

# Data for 2019 -----------------------------------------------------------

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

# Data for 2018 -----------------------------------------------------------

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

# Merge the data in different years ---------------------------------------

All_years <-
  rbind(merge_2018,
        merge_2019) %>% 
  rbind(merge_2020) %>% 
  rbind(merge_2021) %>% 
  mutate(EV_per_Charger = 
           case_when(EV_per_charger <= 10 ~ '0 - 10',
                   EV_per_charger <= 20 ~ '10 - 20',
                   EV_per_charger <= 30 ~ '20 - 30',
                   EV_per_charger <= 40 ~ '30 - 40',
                   .default = '40 - 50'))

# Export an excel file for Tableau

write_xlsx(All_years, 'assignment5/merge_all.xlsx')

# temp --------------------------------------------------------------------

# only use public?

temp <-
  stations %>% 
  group_by(`Access Code`) %>% 
  summarise(n = n())










