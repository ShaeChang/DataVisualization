library(readr)
library(tidyverse)
library(writexl)

# data wrangling

temp_2 <-
  read_csv('data_own/IEA-EV-data.csv') %>% 
  filter(region %in% c('China')) %>% 
  filter(year %in% c(2019, 2021)) %>% 
  filter(parameter == 'EV stock',
         category == 'Historical') %>% 
  select(mode, year, value)

# export the data, and use it in Tableau

write_xlsx(temp_2, 'data_own/assigment6_chinaEV_19to21.xlsx')
